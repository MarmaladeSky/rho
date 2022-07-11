package org.http4s
package rho
package swagger

import cats.syntax.option._
import cats.syntax.show._
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.RequestAST._
import org.http4s.rho.bits._
import org.http4s.rho.swagger.models.Schema.{ArraySchema, PrimitiveSchema, RefSchema}
import org.log4s.getLogger

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.util.control.NonFatal
import org.typelevel.ci.CIString

import scala.annotation.tailrec

private[swagger] class SwaggerModelsBuilder[F[_]](formats: SwaggerFormats)(implicit
    st: ShowType,
    etag: WeakTypeTag[F[_]]) {
  import models._

  private[this] val logger = getLogger

  def mkSwagger(rr: RhoRoute[F, _])(s: OpenAPI): OpenAPI =
    s.copy(
      paths = s.paths.map(paths => paths.copy(paths = collectPaths(rr)(paths))),
      components = s.components.map { components =>
        components.copy(schemas = collectDefinitions(rr)(components))
      }
    )

  def collectPaths(rr: RhoRoute[F, _])(s: Paths): ListMap[String, PathItem] = {
    val pairs = linearizeRoute(rr).map { lr =>
      val o = mkOperation(lr)
      val p0 = s.paths.getOrElse(lr.pathString, PathItem())
      val p1 = lr.method.name.toLowerCase match {
        case "get" => p0.copy(get = o.some)
        case "put" => p0.copy(put = o.some)
        case "post" => p0.copy(post = o.some)
        case "delete" => p0.copy(delete = o.some)
        case "patch" => p0.copy(patch = o.some)
        case "options" => p0.copy(options = o.some)
        case "head" => p0.copy(head = o.some)
        case unknown =>
          logger.warn("unrecognized method: " + unknown)
          p0
      }
      lr.pathString -> p1
    }
    pairs.foldLeft(s.paths) { case (paths, (s, p)) => paths.updated(s, p) }
  }

  def collectDefinitions(rr: RhoRoute[F, _])(s: Components): Map[String, Schema[_]] = {
    val initial: Set[Schema[_]] = s.schemas.values.toSet
    (
      collectResultTypes(rr) ++
        collectCodecTypes(rr) ++
        collectQueryTypes(rr)
    )
      .foldLeft(initial)((s, tpe) => s ++ TypeBuilder.collectModels(tpe, s, formats, etag.tpe))
      .map(m => m.id -> m)
      .toMap
  }

  def collectResultTypes(rr: RhoRoute[F, _]): Set[Type] =
    rr.resultInfo.collect {
      case TypeOnly(tpe) => tpe
      case StatusAndType(_, tpe) => tpe
    }

  def collectCodecTypes(rr: RhoRoute[F, _]): Set[Type] =
    rr.router match {
      case r: CodecRouter[F, _, _] => Set(r.entityType)
      case _ => Set.empty
    }

  def collectQueryTypes(rr: RhoRoute[F, _]): Seq[Type] = {
    def go(stack: List[RequestRule[F]]): List[Type] =
      stack match {
        case Nil => Nil
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case OrRule(a, b) :: xs => go(a :: b :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case IgnoreRule(r) :: xs => go(r :: xs)
        case MetaRule(x, q @ QueryMetaData(_, _, _, _, _)) :: xs =>
          val tpe = q.m.tpe
          TypeBuilder.DataType.fromType(tpe) match {
            case _: TypeBuilder.DataType.ComplexDataType =>
              tpe :: go(x :: xs)
            case TypeBuilder.DataType.ContainerDataType(
                  _,
                  Some(_: TypeBuilder.DataType.ComplexDataType),
                  _
                ) =>
              q.m.tpe.dealias.typeArgs.head :: go(x :: xs)
            case _ => go(x :: xs)
          }

        case MetaRule(x, _) :: xs => go(x :: xs)
      }

    go(rr.rules :: Nil)
  }

  def collectPathParams(lr: LinearRoute): List[PathParameter] = {

    @tailrec
    def go(stack: List[PathOperation], pps: List[PathParameter]): List[PathParameter] =
      stack match {
        case Nil => pps
        case PathMatch.empty :: xs => go(xs, pps)
        case PathMatch(_) :: xs => go(xs, pps)
        case MetaCons(_, _) :: xs => go(xs, pps)
        case PathCapture(id, desc, p, _) :: xs =>
          go(xs, mkPathParam(id, desc, p.asInstanceOf[StringParser[F, String]]) :: pps)
        case CaptureTail :: _ => PathParameter(name = "tail...".some) :: Nil
      }

    go(lr.path, Nil).reverse
  }

  def collectBodyParams(lr: LinearRoute): Option[RequestBody] =
    mkBodyParam(lr)

  def collectResponses(lr: LinearRoute): Map[String, ApiResponse] =
    lr.resultInfo.collect {
      case TypeOnly(tpe) =>
        "200" -> mkResponse("200", "OK", tpe.some, etag.tpe)
      case StatusAndType(s, tpe) =>
        s.code.toString -> mkResponse(s.code.toString, s.reason, tpe.some, etag.tpe)
      case StatusOnly(s) =>
        s.code.toString -> mkResponse(s.code.toString, s.reason, none, etag.tpe)
    }.toMap

  def collectSummary(lr: LinearRoute): Option[String] = {

    def go(stack: List[PathOperation], summary: Option[String]): Option[String] =
      stack match {
        case PathMatch.empty :: Nil => go(Nil, summary)
        case PathMatch(_) :: xs => go(xs, summary)
        case PathCapture(_, _, _, _) :: xs => go(xs, summary)
        case CaptureTail :: _ => summary

        case MetaCons(_, meta) :: xs =>
          meta match {
            case RouteDesc(meta) => meta.some
            case _ => go(xs, summary)
          }

        case Nil => summary
      }

    go(lr.path, None)
  }

  def collectTags(lr: LinearRoute): List[String] = {

    def go(stack: List[PathOperation], tags: List[String]): List[String] =
      stack match {
        case PathMatch.empty :: xs => go(xs, tags)
        case PathMatch(segment) :: xs =>
          tags match {
            case Nil => go(xs, segment.decoded() :: Nil)
            case ts => go(xs, ts)
          }
        case PathCapture(id, _, _, _) :: xs =>
          tags match {
            case Nil => go(xs, id :: Nil)
            case ts => go(xs, ts)
          }
        case Nil | CaptureTail :: _ =>
          tags match {
            case Nil => "/" :: Nil
            case ts => ts
          }
        case MetaCons(_, meta) :: xs =>
          meta match {
            case RouteTags(ts) => ts
            case _ => go(xs, tags)
          }
      }

    go(lr.path, Nil)
  }

  def collectSecurityScopes(lr: LinearRoute): List[Map[String, List[String]]] = {

    def go(stack: List[PathOperation]): Option[Map[String, List[String]]] =
      stack match {
        case Nil => None
        case MetaCons(_, RouteSecurityScope(secScope)) :: _ => secScope.some
        case _ :: xs => go(xs)
      }

    go(lr.path).toList
  }

  def collectOperationParams(lr: LinearRoute): List[Parameter] =
    collectPathParams(lr) ::: collectQueryParams(lr) ::: collectHeaderParams(lr)

  def collectOperationRequestBody(lr: LinearRoute): Option[RequestBody] =
    collectBodyParams(lr)

  def collectQueryParams(lr: LinearRoute): List[Parameter] = {
    def go(stack: List[RequestRule[F]]): List[Parameter] =
      stack match {
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)

        case OrRule(a, b) :: xs =>
          val as = go(a :: xs)
          val bs = go(b :: xs)
          val set: (Parameter, String) => Parameter =
            (p, s) => p.withDesc(p.description.map(_ + s).orElse(s.some))

          addOrDescriptions(set)(as, bs, "params") :::
            addOrDescriptions(set)(bs, as, "params")

        case MetaRule(rs, q @ QueryMetaData(_, _, _, _, _)) :: xs =>
          mkQueryParam(q.asInstanceOf[QueryMetaData[F, _]]) :: go(rs :: xs)

        case MetaRule(rs, m: TextMetaData) :: xs =>
          go(rs :: Nil).map(_.withDesc(m.msg.some)) ::: go(xs)

        case MetaRule(a, _) :: xs => go(a :: xs)

        case IgnoreRule(r) :: xs => go(r :: xs)

        case Nil => Nil
      }

    go(lr.rules :: Nil)
  }

  def collectHeaderParams(lr: LinearRoute): List[HeaderParameter] = {
    def go(stack: List[RequestRule[F]]): List[HeaderParameter] =
      stack match {
        case Nil => Nil
        case AndRule(a, b) :: xs => go(a :: b :: xs)
        case MetaRule(a, HeaderMetaData(key, r)) :: xs => mkHeaderParam(key, r) :: go(a :: xs)
        case MetaRule(a, _) :: xs => go(a :: xs)
        case (EmptyRule() | CaptureRule(_)) :: xs => go(xs)
        case MapRule(r, _) :: xs => go(r :: xs)
        case IgnoreRule(r) :: xs => go(r :: xs)
        case OrRule(a, b) :: xs =>
          val as = go(a :: xs)
          val bs = go(b :: xs)
          val set: (HeaderParameter, String) => HeaderParameter =
            (p, s) => p.copy(description = p.description.map(_ + s).orElse(s.some))
          addOrDescriptions(set)(as, bs, "headers") :::
            addOrDescriptions(set)(bs, as, "headers")
      }

    go(lr.rules :: Nil)
  }

  def renderMediaRange: MediaRange => String = {
    case tpe: org.http4s.MediaType => tpe.show
    case range: MediaRange => range.show
  }

  def mkOperation(lr: LinearRoute): Operation = {
    val parameters = collectOperationParams(lr)

    Operation(
      tags = collectTags(lr),
      summary = collectSummary(lr),
      operationId = mkOperationId(lr, parameters).some,
      parameters = parameters,
      requestBody = collectOperationRequestBody(lr),
      responses = ApiResponses(responses = collectResponses(lr)).some,
      securityRequirement = collectSecurityScopes(lr).map(SecurityRequirement)
    )
  }

  def mkOperationId(lr: LinearRoute, parameters: List[Parameter]): String = {
    val showParameters =
      if (parameters.isEmpty) ""
      else parameters.flatMap(_.name).mkString("-", "-", "")

    lr.method.toString.toLowerCase +
      lr.pathString
        .split("/")
        .filter(s => s.nonEmpty && !(s.startsWith("{") && s.endsWith("}")))
        .map(_.capitalize)
        .mkString +
      showParameters
  }

  def mkBodyParam(lr: LinearRoute): Option[RequestBody] =
    lr.entityType.map { entityType =>
      val tpe = entityType
      val model: Schema[_] = if (tpe.isPrimitive) {
        val name = TypeBuilder.DataType(tpe).name
        PrimitiveSchema(id = tpe.fullName, typeName = name)
      } else if (tpe.isCollection) {
        val pType = tpe.dealias.typeArgs.head
        ArraySchema(
          id = tpe.fullName,
          itemsSchema = PrimitiveSchema(id = tpe.fullName, typeName = pType.simpleName)
        )
      } else RefSchema(tpe.fullName, tpe.simpleName)

      RequestBody(
        content = lr.responseEncodings.map { encodings =>
          val encodingName = s"${encodings.mainType}/${encodings.subType}"
          encodingName -> models.MediaType(schema = model.some)
        }.toMap,
        description = tpe.simpleName.some,
        required = (!tpe.isOption).some
      )
    }

  def mkPathParam(
      name: String,
      description: Option[String],
      parser: StringParser[F, String]): PathParameter = {
    val tpe = parser.typeTag.map(tag => getType(tag.tpe)).getOrElse("string")
    PathParameter(name = name.some, description = description)
  }

  def mkResponse(code: String, descr: String, otpe: Option[Type], fType: Type): ApiResponse = {

    @tailrec
    def typeToProp(tpe: Type): Option[Schema[_]] =
      if (Reflector.isExcluded(tpe))
        None
      else if (tpe.isUnitOrVoid)
        None
      else if (tpe.isPrimitive)
        mkPrimitiveProperty(tpe).some
      else if (tpe.isMap)
        mkMapProperty(tpe)
      else if (tpe.isCollection)
        mkCollectionProperty(tpe)
      else if (tpe.isStream)
        typeToProp(tpe.dealias.typeArgs(1))
      else if (tpe.isEffect(fType))
        typeToProp(tpe.dealias.typeArgs(0))
      else if (tpe.isSwaggerFile)
        Schema.FileSchema(id = tpe.fullName).some
      else
        RefSchema(id = tpe.fullName, ref = tpe.simpleName).some

    def mkPrimitiveProperty(tpe: Type): Schema[_] = {
      import TypeBuilder._

      DataType.fromType(tpe) match {
        case DataType.ValueDataType(name, format, qName) =>
          Schema.apply[String](
            _id = tpe.fullName,
            `_type` = name.some,
            _description = qName,
            _format = format
          )
        case DataType.ComplexDataType(name, qName) =>
          Schema.apply[String](_id = tpe.fullName, `_type` = name.some, _description = qName)
        case DataType.ContainerDataType(name, _, _) =>
          Schema.apply[String](_id = tpe.fullName, `_type` = name.some)
        case DataType.EnumDataType(enums) =>
          Schema.StringSchema(id = tpe.fullName, enums = enums)
      }
    }

    def mkCollectionProperty(tpe: Type): Option[Schema[_]] = {
      val param = tpe.dealias.typeArgs.head
      val prop =
        if (param.isPrimitive)
          mkPrimitiveProperty(param).some
        else if (param.isCollection)
          typeToProp(param)
        else
          RefSchema(id = tpe.fullName, ref = param.simpleName).some

      prop.map(p => ArraySchema(id = tpe.fullName, itemsSchema = p))
    }

    def mkMapProperty(tpe: Type): Option[Schema[_]] = {
      val param = tpe.dealias.typeArgs.last
      if (param.isPrimitive)
        mkPrimitiveProperty(param).some
      else if (param.isCollection)
        typeToProp(param)
      else
        RefSchema(id = tpe.fullName, ref = param.simpleName).some
    }

    val schema = {
      try otpe.flatMap(typeToProp)
      catch {
        case NonFatal(t) =>
          logger.warn(t)(s"Failed to build model for type ${otpe.get}")
          None
      }
    }

    val content = Map(
      "application/json" -> MediaType(schema = schema)
    )

    ApiResponse(description = descr, content = content)
  }

  def mkQueryParam(rule: QueryMetaData[F, _]): Parameter = {
    val required = !(rule.m.tpe.isOption || rule.default.isDefined)

    val tpe = if (rule.m.tpe.isOption) rule.m.tpe.dealias.typeArgs.head else rule.m.tpe
    TypeBuilder.DataType(tpe) match {
      case TypeBuilder.DataType.ComplexDataType(nm, _) =>
        QueryParameter(
          name = rule.name.some,
          description = rule.description,
          required = required.some
//          , defaultValue = rule.default.map(_ =>
//            ""
//          ) // TODO ideally need to use the parser to serialize it into string
        )
      // XXX uniqueItems is indeed part of `parameter` api,
      // see here: http://swagger.io/specification/#parameterObject
      // however the java api does not include it...
      case TypeBuilder.DataType.ContainerDataType(_, dt, _) =>
        val itemTpe = dt match {
          case Some(TypeBuilder.DataType.ComplexDataType(nm, _)) =>
            Schema.RefSchema(nm, nm).some
          // XXX need to revisit to take care of recursive array type
          case Some(tpe: TypeBuilder.DataType) =>
            Schema.RefSchema(tpe.name, tpe.name).some
          case None =>
            None
        }

        QueryParameter(
          name = rule.name.some,
          required = required.some,
          description = rule.description
        )

      case TypeBuilder.DataType.ValueDataType(nm, _, _) =>
        QueryParameter(
          name = rule.name.some,
          schema = Schema.RefSchema(nm, nm).some,
          description = rule.description,
          required = required.some
        )

      case tpe @ TypeBuilder.DataType.EnumDataType(enums) =>
        QueryParameter(
          name = rule.name.some,
          schema = Schema.StringSchema(tpe.name, enums).some,
          description = rule.description,
          required = required.some
        )
    }
  }

  def mkHeaderParam(key: CIString, isRequired: Boolean): HeaderParameter =
    HeaderParameter(
      schema = Schema.StringSchema(key.toString).some,
      name = key.toString.some,
      required = isRequired.some
    )

  def linearizeRoute(rr: RhoRoute[F, _]): List[LinearRoute] = {

    def go(stack: List[PathRule], acc: List[PathOperation]): List[List[PathOperation]] =
      stack match {
        case PathOr(a, b) :: xs => go(a :: xs, acc) ::: go(b :: xs, acc)
        case PathAnd(a, b) :: xs => go(a :: b :: xs, acc)
        case (m @ MetaCons(a, _)) :: xs => go(a :: xs, m :: acc)
        case (op: PathOperation) :: xs => go(xs, op :: acc)
        case Nil => acc :: Nil
      }

    go(rr.path :: Nil, Nil)
      .map(_.reverse)
      .map(linearPath => LinearRoute(linearPath, rr))
  }

  def addOrDescriptions[A <: Parameter](
      set: (A, String) => A)(as: List[A], bs: List[A], tpe: String): List[A] =
    if (bs.isEmpty) as
    else if (as.isEmpty) bs
    else
      as.map(
        set(
          _,
          s"Optional if the following $tpe are satisfied: " + bs
            .flatMap(_.name)
            .mkString("[", ", ", "]")
        )
      )

  def getType(m: Type): String =
    TypeBuilder.DataType(m).name

  case class LinearRoute(
      method: Method,
      path: List[PathOperation],
      rules: RequestRule[F],
      responseEncodings: Set[org.http4s.MediaType],
      resultInfo: Set[ResultInfo],
      validMedia: Set[MediaRange],
      entityType: Option[Type]) {

    lazy val pathString: String = {
      @tailrec
      def go(stack: List[PathOperation], pathStr: String): String =
        stack match {
          case Nil => if (pathStr.isEmpty) "/" else pathStr
          case PathMatch.empty :: Nil => pathStr + "/"
          case PathMatch.empty :: xs => go(xs, pathStr)
          case PathMatch(s) :: xs => go(xs, pathStr + "/" + s)
          case MetaCons(_, _) :: xs => go(xs, pathStr)
          case PathCapture(id, _, _, _) :: xs => go(xs, s"$pathStr/{$id}")
          case CaptureTail :: _ => pathStr + "/{tail...}"
        }

      go(path, "")
    }
  }

  object LinearRoute {
    def apply(path: List[PathOperation], rr: RhoRoute[F, _]): LinearRoute = {
      val entityType = rr.router match {
        case r: CodecRouter[F, _, _] => r.entityType.some
        case _ => none
      }
      new LinearRoute(
        rr.method,
        path,
        rr.rules,
        rr.responseEncodings,
        rr.resultInfo,
        rr.validMedia,
        entityType
      )
    }
  }
}
