package org.http4s.rho.swagger

import cats.syntax.option._
import io.swagger.v3.oas.models.media
import io.swagger.v3.oas.models.media.Content
import io.swagger.v3.oas.models.servers.{ServerVariable, ServerVariables}
import io.swagger.v3.oas.{models => jm}
import org.http4s.Uri

import scala.collection.compat.view._
import scala.collection.immutable.{ListMap, Seq}
import scala.jdk.CollectionConverters._

object models {

  import JValue._

  case class OpenAPI(
      openapi: String = "3.1.0",
      info: Option[Info] = None,
      jsonSchemaDialect: Option[Uri] = None,
      servers: List[Server] = Nil,
      paths: Option[Paths] = None,
      webhooks: ListMap[String, PathItem] = ListMap.empty,
      components: Option[Components] = None,
      security: List[SecurityRequirement] = Nil,
      tags: List[Tag] = Nil,
      externalDocs: Option[ExternalDocumentation] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.OpenAPI =
      (new jm.OpenAPI)
        .specVersion(
          openapi match {
            case "3.1.0" => jm.SpecVersion.V31
            case "3.0.0" => jm.SpecVersion.V30
            case other =>
              throw new IllegalArgumentException(s"OpenAPI version $other is not supported")
          }
        )
        .info(fromOption(info.map(_.toJModel)))
        .jsonSchemaDialect(fromOption(jsonSchemaDialect.map(_.renderString)))
        .servers(servers.map(_.toJModel).asJava)
        .paths(fromOption(paths.map(_.toJModel)))
        .webhooks(webhooks.view.mapValues(_.toJModel).toMap.asJava)
        .components(fromOption(components.map(_.toJModel)))
        .security(fromList(security.map(_.toJModel)))
        .tags(fromList(tags.map(_.toJModel)))
        .externalDocs(fromOption(externalDocs.map(_.toJModel)))
        .extensions(fromMap(extensions))

  }

  case class Info(
      title: String,
      version: String,
      description: Option[String] = None,
      termsOfService: Option[String] = None,
      contact: Option[Contact] = None,
      license: Option[License] = None,
      vendorExtensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.info.Info = {
      val i = new jm.info.Info
      i.title(title)
        .version(version)
        .description(fromOption(description))
        .termsOfService(fromOption(termsOfService))
        .contact(fromOption(contact.map(_.toJModel)))
        .license(fromOption(license.map(_.toJModel)))
      vendorExtensions.foreach { case (key, value) => i.addExtension(key, value) }
      i
    }
  }

  case class Contact(
      name: String,
      url: Option[String] = None,
      email: Option[String] = None
  ) {

    def toJModel: jm.info.Contact =
      (new jm.info.Contact).name(name).url(fromOption(url)).email(fromOption(email))
  }

  case class License(
      name: String,
      url: String
  ) {

    def toJModel: jm.info.License =
      (new jm.info.License).name(name).url(url)
  }

  case class Server(
      url: String,
      description: Option[String],
      variables: Map[String, ServerVariableObject],
      extensions: Map[String, Any]) {

    def toJModel: jm.servers.Server = {
      val s = new jm.servers.Server()
      s.setUrl(url)
      description.foreach(s.setDescription)

      val vars = new ServerVariables()
      variables.foreach { case (name, v) => vars.addServerVariable(name, v.toJModel) }
      s.setVariables(vars)

      extensions.foreach { case (name, e) => s.addExtension(name, e) }

      s
    }

  }

  case class ServerVariableObject(
      enum: List[String],
      default: Option[String],
      description: Option[String]) {

    def toJModel: ServerVariable = {
      val v = new ServerVariable()
      `enum`.foreach(v.addEnumItem)
      default.foreach(v.setDefault)
      description.foreach(v.setDescription)
      v
    }

  }

  case class Components(
      schemas: Map[String, Schema[_]] = Map.empty,
      responses: Map[String, ApiResponse] = Map.empty,
      parameters: Map[String, Parameter] = Map.empty,
      examples: Map[String, Example] = Map.empty,
      requestBodies: Map[String, RequestBody] = Map.empty,
      headers: Map[String, Header] = Map.empty,
      securitySchemes: Map[String, SecurityScheme] = Map.empty,
      links: Map[String, Link] = Map.empty,
      callbacks: Map[String, Callback] = Map.empty,
      pathItems: Map[String, PathItem] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.Components =
      (new jm.Components)
        .schemas(fromMap(schemas.view.mapValues(_.toJModel)))
        .responses(fromMap(responses.view.mapValues(_.toJModel)))
        .parameters(fromMap(parameters.view.mapValues(_.toJModel)))
        .examples(fromMap(examples.view.mapValues(_.toJModel)))
        .requestBodies(fromMap(requestBodies.view.mapValues(_.toJModel)))
        .headers(fromMap(headers.view.mapValues(_.toJModel)))
        .securitySchemes(fromMap(securitySchemes.view.mapValues(_.toJModel)))
        .links(fromMap(links.view.mapValues(_.toJModel)))
        .callbacks(fromMap(callbacks.view.mapValues(_.toJModel)))
        .pathItems(fromMap(pathItems.view.mapValues(_.toJModel)))
        .extensions(fromMap(extensions))

  }

  case class Paths(
      paths: Map[String, PathItem] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.Paths = {
      val p = new jm.Paths
      p.setExtensions(fromMap(paths))
      extensions.foreach { case (name, e) => p.addExtension(name, e) }
      p
    }

  }

  case class PathItem(
      get: Option[Operation] = None,
      put: Option[Operation] = None,
      post: Option[Operation] = None,
      delete: Option[Operation] = None,
      patch: Option[Operation] = None,
      options: Option[Operation] = None,
      head: Option[Operation] = None,
      parameters: List[Parameter] = Nil,
      vendorExtensions: Map[String, Any] = Map.empty
  ) {

    def operations: Seq[Operation] =
      get.toList ++
        put.toList ++
        post.toList ++
        delete.toList ++
        patch.toList ++
        options.toList ++
        head.toList

    def toJModel: jm.PathItem = {
      val p = new jm.PathItem
      p.setGet(fromOption(get.map(_.toJModel)))
      p.setPut(fromOption(put.map(_.toJModel)))
      p.setPost(fromOption(post.map(_.toJModel)))
      p.setDelete(fromOption(delete.map(_.toJModel)))
      p.setPatch(fromOption(patch.map(_.toJModel)))
      p.setOptions(fromOption(options.map(_.toJModel)))
      p.setHead(fromOption(head.map(_.toJModel)))
      p.setParameters(fromList(parameters.map(_.toJModel)))
      vendorExtensions.foreach { case (key, value) => p.addExtension(key, value) }
      p
    }

    def collectSomeOperations(f: Operation => Option[Operation]): PathItem =
      copy(
        get = get.flatMap(f),
        put = put.flatMap(f),
        post = post.flatMap(f),
        delete = delete.flatMap(f),
        patch = patch.flatMap(f),
        options = options.flatMap(f),
        head = head.flatMap(f)
      )
  }

  case class Operation(
      tags: List[String] = Nil,
      summary: Option[String] = None,
      description: Option[String] = None,
      externalDocs: Option[ExternalDocumentation] = None,
      operationId: Option[String] = None,
      parameters: List[Parameter] = Nil,
      requestBody: Option[RequestBody] = None,
      responses: Option[ApiResponses] = None,
      callbacks: Map[String, Callback] = Map.empty,
      deprecated: Boolean = false,
      securityRequirement: List[SecurityRequirement] = Nil,
      servers: List[Server] = Nil,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.Operation =
      (new jm.Operation)
        .tags(fromList(tags))
        .summary(fromOption(summary))
        .description(fromOption(description))
        .externalDocs(fromOption(externalDocs.map(_.toJModel)))
        .operationId(fromOption(operationId))
        .parameters(fromList(parameters.map(_.toJModel)))
        .requestBody(fromOption(requestBody.map(_.toJModel)))
        .responses(fromOption(responses.map(_.toJModel)))
        .callbacks(fromMap(callbacks.view.mapValues(_.toJModel)))
        .deprecated(deprecated)
        .security(fromList(securityRequirement.map(_.toJModel)))
        .servers(servers.map(_.toJModel).asJava)
        .extensions(fromMap(extensions))

  }

  case class ExternalDocumentation(
      description: String,
      url: String
  ) {

    def toJModel: jm.ExternalDocumentation = {
      val ed = new jm.ExternalDocumentation
      ed.setDescription(description)
      ed.setUrl(url)
      ed
    }
  }

  sealed trait Parameter {
    // Fixed
    def name: Option[String]
    def in: Parameter.in
    def description: Option[String]
    def required: Option[Boolean]
    def deprecated: Option[Boolean]
    // allowEmptyValue is skipped

    // Serialization
    def style: Option[Parameter.Style]
    def explode: Option[Boolean]
    def schema: Option[Schema[_]]
    def example: Option[Any]
    def examples: Map[String, Example]
    def content: Map[String, MediaType]
    def extensions: Map[String, Any]

    private[Parameter] def jConstructor: jm.parameters.Parameter

    def toJModel: jm.parameters.Parameter =
      jConstructor
        .name(fromOption(name))
        .in(in.toString)
        .description(fromOption(description))
        .required(fromOption(required))
        .deprecated(fromOption(deprecated))
        .style(fromOption(style.map(_.toJModel)))
        .explode(fromOption(explode))
        .schema(fromOption(schema.map(_.toJModel)))
        .example(fromOption(example))
        .examples(fromMap(examples.view.mapValues(_.toJModel)))
        .content(content.foldLeft(new jm.media.Content()) { case (c, (n, v)) =>
          c.addMediaType(n, v.toJModel)
        })
        .extensions(fromMap(extensions))

  }

  object Parameter {

    sealed trait in
    object in {
      case object path extends in
      case object query extends in
      case object header extends in
      case object cookie extends in
    }

    sealed trait Style { def toJModel: jm.parameters.Parameter.StyleEnum }
    object Style {
      import jm.parameters.Parameter.StyleEnum._
      case object matrix extends Style { override val toJModel = MATRIX }
      case object label extends Style { override val toJModel = LABEL }
      case object form extends Style { override val toJModel = FORM }
      case object simple extends Style { override val toJModel = SIMPLE }
      case object spaceDelimited extends Style { override val toJModel = SPACEDELIMITED }
      case object pipeDelimited extends Style { override val toJModel = PIPEDELIMITED }
      case object deepObject extends Style { override val toJModel = DEEPOBJECT }
    }

    implicit class Ops(val parameter: Parameter) extends AnyVal {
      def withDesc(desc: Option[String]): Parameter =
        parameter match {
          case p: PathParameter => p.copy(description = desc)
          case p: QueryParameter => p.copy(description = desc)
          case p: HeaderParameter => p.copy(description = desc)
          case p: CookieParameter => p.copy(description = desc)
        }
    }
  }

  case class PathParameter(
      name: Option[String] = None,
      description: Option[String] = None,
      deprecated: Option[Boolean] = None,
      style: Option[Parameter.Style] = Parameter.Style.simple.some,
      explode: Option[Boolean] = None,
      schema: Option[Schema[_]] = None,
      example: Option[Any] = None,
      examples: Map[String, Example] = Map.empty,
      content: Map[String, MediaType] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) extends Parameter {

    override val in: Parameter.in = Parameter.in.path
    override val required: Option[Boolean] = true.some

    override private[Parameter] def jConstructor = new jm.parameters.PathParameter

  }

  case class QueryParameter(
      name: Option[String] = None,
      description: Option[String] = None,
      deprecated: Option[Boolean] = None,
      required: Option[Boolean] = None,
      style: Option[Parameter.Style] = Parameter.Style.form.some,
      explode: Option[Boolean] = None,
      allowReserved: Option[Boolean] = None,
      schema: Option[Schema[_]] = None,
      example: Option[Any],
      examples: Map[String, Example] = Map.empty,
      content: Map[String, MediaType] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) extends Parameter {

    override val in: Parameter.in = Parameter.in.query

    override private[Parameter] def jConstructor: jm.parameters.QueryParameter =
      new jm.parameters.QueryParameter

  }

  case class HeaderParameter(
      name: Option[String] = None,
      description: Option[String] = None,
      deprecated: Option[Boolean] = None,
      required: Option[Boolean] = None,
      style: Option[Parameter.Style] = Parameter.Style.simple.some,
      explode: Option[Boolean] = None,
      schema: Option[Schema[_]] = None,
      example: Option[Any],
      examples: Map[String, Example] = Map.empty,
      content: Map[String, MediaType] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) extends Parameter {

    override val in: Parameter.in = Parameter.in.header

    override private[Parameter] def jConstructor = new jm.parameters.HeaderParameter

  }

  case class CookieParameter(
      name: Option[String] = None,
      description: Option[String] = None,
      deprecated: Option[Boolean] = None,
      required: Option[Boolean] = None,
      style: Option[Parameter.Style] = Parameter.Style.form.some,
      explode: Option[Boolean] = None,
      schema: Option[Schema[_]] = None,
      example: Option[Any],
      examples: Map[String, Example] = Map.empty,
      content: Map[String, MediaType] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) extends Parameter {

    override val in: Parameter.in = Parameter.in.cookie

    override private[Parameter] def jConstructor = new jm.parameters.CookieParameter

  }

  case class RequestBody(
      description: Option[String] = None,
      content: Map[String, MediaType] = Map.empty,
      required: Option[Boolean] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.parameters.RequestBody =
      (new jm.parameters.RequestBody)
        .description(fromOption(description))
        .content(content.foldLeft(new jm.media.Content) { case (c, (n, m)) =>
          c.addMediaType(n, m.toJModel)
        })
        .required(fromOption(required))
        .extensions(fromMap(extensions))

  }

  case class MediaType(
      schema: Option[Schema[_]],
      example: Option[Any],
      examples: Map[String, Example],
      encoding: Map[String, Encoding],
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.media.MediaType =
      (new jm.media.MediaType)
        .schema(fromOption(schema.map(_.toJModel)))
        .example(fromOption(example))
        .examples(fromMap(examples.view.mapValues(_.toJModel)))
        .encoding(fromMap(encoding.view.mapValues(_.toJModel)))
        .extensions(fromMap(extensions))

  }

  case class Encoding(
      contentType: Option[String],
      headers: Map[String, Header] = Map.empty,
      style: Option[Encoding.Style] = None,
      explode: Option[Boolean] = None,
      allowReserved: Option[Boolean] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.media.Encoding =
      (new jm.media.Encoding)
        .contentType(fromOption(contentType))
        .headers(fromMap(headers.view.mapValues(_.toJModel)))
        .style(fromOption(style.map(_.toJModel)))
        .explode(fromOption(explode))
        .allowReserved(fromOption(allowReserved))
        .extensions(fromMap(extensions))

  }

  object Encoding {
    import jm.media.Encoding.StyleEnum
    import jm.media.Encoding.StyleEnum._

    sealed trait Style { def toJModel: StyleEnum }
    object Style {
      case object form extends Style {
        override val toJModel: StyleEnum = FORM
      }
      case object spaceDelimited extends Style {
        override val toJModel: StyleEnum = SPACE_DELIMITED
      }
      case object pipeDelimited extends Style {
        override val toJModel: StyleEnum = PIPE_DELIMITED
      }
      case object deepObject extends Style {
        override val toJModel: StyleEnum = DEEP_OBJECT
      }
    }

  }

  case class ApiResponses(
      responses: Map[String, ApiResponse] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.responses.ApiResponses =
      new jm.responses.ApiResponses {
        responses.foreach { case (n, r) => this.addApiResponse(n, r.toJModel) }
      }.extensions(fromMap(extensions))

  }

  case class ApiResponse(
      description: String,
      headers: Map[String, Header] = Map.empty,
      content: Map[String, MediaType] = Map.empty,
      links: Map[String, Link] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.responses.ApiResponse =
      (new jm.responses.ApiResponse)
        .description(description)
        .headers(fromMap(headers.view.mapValues(_.toJModel)))
        .content {
          content.foldLeft(new jm.media.Content) { case (c, (n, m)) =>
            c.addMediaType(n, m.toJModel)
          }
        }
        .links(fromMap(links.view.mapValues(_.toJModel)))
        .extensions(fromMap(extensions))
  }

  case class Callback(
      items: Map[String, PathItem],
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.callbacks.Callback = {
      val c = new jm.callbacks.Callback
      items.foreach { case (n, i) => c.addPathItem(n, i.toJModel) }
      c.extensions(fromMap(extensions))
      c
    }

  }

  case class Example(
      summary: Option[String] = None,
      description: Option[String] = None,
      value: Option[Any] = None,
      externalValue: Option[String] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.examples.Example =
      (new jm.examples.Example)
        .summary(fromOption(summary))
        .description(fromOption(description))
        .value(fromOption(value))
        .externalValue(fromOption(externalValue))
        .extensions(fromMap(extensions))

  }

  case class Link(
      operationRef: Option[String] = None,
      operationId: Option[String] = None,
      parameters: Map[String, Any] = Map.empty,
      requestBody: Option[Any] = None,
      description: Option[String] = None,
      server: Option[Server] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.links.Link = {
      val l = (new jm.links.Link)
        .operationRef(fromOption(operationRef))
        .operationId(fromOption(operationId))
        .requestBody(fromOption(requestBody))
        .description(fromOption(description))
        .server(fromOption(server.map(_.toJModel)))
        .extensions(fromMap(extensions))

      l.setParameters(fromMap(parameters))
      l
    }

  }

  case class Header(
      description: Option[String] = None,
      required: Option[Boolean] = None,
      deprecated: Option[Boolean] = None,
      style: Option[Header.StyleEnum] = None,
      explode: Option[Boolean] = None,
      schema: Option[Schema[_]] = None,
      examples: Map[String, Example] = Map.empty,
      example: Option[Any] = None,
      content: Map[String, MediaType] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.headers.Header =
      (new jm.headers.Header)
        .description(fromOption(description))
        .required(fromOption(required))
        .deprecated(fromOption(deprecated))
        .style(fromOption(style.map(_.toJModel)))
        .explode(fromOption(explode))
        .schema(fromOption(schema.map(_.toJModel)))
        .examples(fromMap(examples))
        .example(fromOption(example))
        .content {
          content.foldLeft(new Content) { case (a, (n, m)) => a.addMediaType(n, m.toJModel) }
        }
        .extensions(fromMap(extensions))

  }

  object Header {
    import jm.headers.Header.{StyleEnum => JStyleEnum}

    sealed trait StyleEnum { def toJModel: JStyleEnum }
    object StyleEnum {
      import JStyleEnum._
      case object simple extends StyleEnum { override val toJModel: JStyleEnum = SIMPLE }
    }

  }

  case class Tag(
      name: String,
      description: Option[String] = None,
      externalDocs: Option[ExternalDocumentation] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.tags.Tag =
      (new jm.tags.Tag)
        .name(name)
        .description(fromOption(description))
        .externalDocs(fromOption(externalDocs.map(_.toJModel)))
        .extensions(fromMap(extensions))
  }

  sealed trait Schema[T] {

    def id: String

    def name: Option[String] = None
    def title: Option[String] = None
    def multipleOf: Option[BigDecimal] = None
    def maximum: Option[BigDecimal] = None
    def exclusiveMaximum: Option[Boolean] = None
    def minimum: Option[BigDecimal] = None
    def exclusiveMinimum: Option[Boolean] = None
    def maxLength: Option[Int] = None
    def minLength: Option[Int] = None
    def pattern: Option[String] = None
    def maxItems: Option[Int] = None
    def minItems: Option[Int] = None
    def uniqueItems: Option[Boolean] = None
    def maxProperties: Option[Int] = None
    def minProperties: Option[Int] = None
    def required: List[String] = Nil
    def `type`: Option[String] = None
    def not: Option[Schema[_]] = None
    def properties: Map[String, Schema[_]] = Map.empty
    def additionalProperties: Option[Object] = None
    def description: Option[String] = None
    def format: Option[String] = None
    def $ref: Option[String] = None
    def nullable: Option[Boolean] = None
    def readOnly: Option[Boolean] = None
    def writeOnly: Option[Boolean] = None
    def example: Option[T] = None
    def externalDocs: Option[ExternalDocumentation] = None
    def deprecated: Option[Boolean] = None
    def xml: Option[XML] = None
    def extensions: Map[String, Any] = Map.empty
    def _enum: List[T] = Nil
    def discriminator: Option[Discriminator] = None
    def exampleSetFlag: Boolean = false
    def prefixItems: List[Schema[_]] = Nil
    def allOf: List[Schema[_]] = Nil
    def anyOf: List[Schema[_]] = Nil
    def oneOf: List[Schema[_]] = Nil

    def toJModel: jm.media.Schema[T]
  }

  object Schema {

    case class RefSchema(id: String, ref: String) extends Schema[Any] {

      override def toJModel: media.Schema[Any] =
        new jm.media.Schema[Any]
          .$ref(ref)
          .asInstanceOf[jm.media.Schema[Any]]

    }

    case class ObjectSchema(id: String) extends Schema[Object] {

      override def `type`: Option[String] = "object".some

      override def toJModel: media.Schema[Object] =
        new jm.media.Schema[Object]
          .asInstanceOf[jm.media.Schema[Object]]

    }

  }

  case class Discriminator(
      propertyName: String,
      mapping: Map[String, String] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.media.Discriminator = {
      val d = (new jm.media.Discriminator)
        .propertyName(propertyName)
        .mapping(fromMap(mapping))
      d.setExtensions(fromMap(extensions))
      d
    }

  }

  case class XML(
      name: Option[String],
      namespace: Option[String],
      prefix: Option[String],
      attribute: Option[Boolean],
      wrapped: Option[Boolean],
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.media.XML =
      (new jm.media.XML)
        .name(fromOption(name))
        .namespace(fromOption(namespace))
        .prefix(fromOption(prefix))
        .attribute(fromOption(attribute))
        .wrapped(fromOption(wrapped))
        .extensions(fromMap(extensions))

  }

  case class SecurityScheme(
      `type`: SecurityScheme.Type,
      description: Option[String],
      name: Option[String],
      in: Option[SecurityScheme.In],
      scheme: Option[String],
      bearerFormat: Option[String],
      flows: Option[OAuthFlows],
      openIdConnectUrl: Option[String],
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.security.SecurityScheme =
      (new jm.security.SecurityScheme)
        .`type`(`type`.toJModel)
        .description(fromOption(description))
        .name(fromOption(name))
        .in(fromOption(in.map(_.toJModel)))
        .scheme(fromOption(scheme))
        .bearerFormat(fromOption(bearerFormat))
        .flows(fromOption(flows.map(_.toJModel)))
        .openIdConnectUrl(fromOption(openIdConnectUrl))

  }

  object SecurityScheme {
    import jm.security.SecurityScheme.{Type => JType}
    import jm.security.SecurityScheme.{In => JIn}
    import jm.security.SecurityScheme.Type._
    import jm.security.SecurityScheme.In._

    sealed trait Type { def toJModel: jm.security.SecurityScheme.Type }
    object Type {
      case object apiKey extends Type { override val toJModel: JType = APIKEY }
      case object http extends Type { override val toJModel: JType = HTTP }
      case object oauth2 extends Type { override val toJModel: JType = OAUTH2 }
      case object openIdConnect extends Type { override val toJModel: JType = OPENIDCONNECT }
      case object mutualTLS extends Type { override val toJModel: JType = MUTUALTLS }
    }

    sealed trait In { def toJModel: jm.security.SecurityScheme.In }
    object In {
      case object cookie extends In { override val toJModel: JIn = COOKIE }
      case object header extends In { override val toJModel: JIn = HEADER }
      case object query extends In { override val toJModel: JIn = QUERY }
    }

  }

  case class OAuthFlows(
      `implicit`: Option[OAuthFlow] = None,
      password: Option[OAuthFlow] = None,
      clientCredentials: Option[OAuthFlow] = None,
      authorizationCode: Option[OAuthFlow] = None,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.security.OAuthFlows =
      (new jm.security.OAuthFlows)
        .`implicit`(fromOption(`implicit`.map(_.toJModel)))
        .password(fromOption(password.map(_.toJModel)))
        .clientCredentials(fromOption(clientCredentials.map(_.toJModel)))
        .authorizationCode(fromOption(authorizationCode.map(_.toJModel)))

  }

  case class OAuthFlow(
      authorizationUrl: Option[String] = None,
      tokenUrl: Option[String] = None,
      refreshUrl: Option[String] = None,
      scopes: Map[String, String] = Map.empty,
      extensions: Map[String, Any] = Map.empty
  ) {

    def toJModel: jm.security.OAuthFlow =
      (new jm.security.OAuthFlow)
        .authorizationUrl(fromOption(authorizationUrl))
        .tokenUrl(fromOption(tokenUrl))
        .refreshUrl(fromOption(refreshUrl))
        .scopes {
          val s = new jm.security.Scopes()
          s.putAll(scopes.asJava)
          s
        }
        .extensions(fromMap(extensions))

  }

  case class SecurityRequirement(
      items: Map[String, List[String]]
  ) {

    def toJModel: jm.security.SecurityRequirement =
      items.foldLeft(new jm.security.SecurityRequirement) { case (a, (n, l)) =>
        a.addList(n, l.asJava)
      }
  }

  private object JValue {

    def fromOption[A](oa: Option[A]): A =
      oa.getOrElse(null.asInstanceOf[A])

    def fromList[A](xs: List[A]): java.util.List[A] =
      if (xs.isEmpty) null else xs.asJava

    def fromMap[A, B](m: Map[A, B]): java.util.Map[A, B] =
      if (m.isEmpty) null else m.asJava

    def fromMap[A, B](m: IterableView[(A, B), Iterable[_]]): java.util.Map[A, B] =
      if (m.isEmpty) null else m.toMap.asJava
  }
}
