package org.http4s.rho.swagger

import scala.reflect.runtime.universe._

import models._

final case class SwaggerFormats(
    customSerializers: PartialFunction[Type, Set[Schema[_]]],
    customFieldSerializers: PartialFunction[Type, Schema[_]]) {

  def withSerializers(serializer: PartialFunction[Type, Set[Schema[_]]]): SwaggerFormats =
    this.copy(customSerializers = serializer.orElse(this.customSerializers))

  def withSerializers(t: Type, models: Set[Schema[_]]): SwaggerFormats = withSerializers {
    case tpe if tpe =:= t => models
  }

  def withFieldSerializers(fieldSerializer: PartialFunction[Type, Schema[_]]): SwaggerFormats =
    this.copy(customFieldSerializers = fieldSerializer.orElse(this.customFieldSerializers))

  def withFieldSerializers(t: Type, property: Schema[_]): SwaggerFormats = withFieldSerializers {
    case tpe if tpe =:= t => property
  }
}

object SwaggerFormats {
  val emptySerializers: PartialFunction[Type, Set[Schema[_]]] = PartialFunction.empty

  val emptyFieldSerializers: PartialFunction[Type, Schema[_]] = PartialFunction.empty
}
