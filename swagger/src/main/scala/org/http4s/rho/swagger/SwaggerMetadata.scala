package org.http4s.rho.swagger

import org.http4s.rho.swagger.models._

case class SwaggerMetadata(
    apiInfo: Info = Info(title = "My API", version = "1.0.0"),
    servers: List[Server] = Nil,
    security: List[SecurityRequirement] = Nil,
    tags: List[Tag] = Nil,
    extensions: Map[String, Any] = Map.empty) {

  def toSwagger: OpenAPI = OpenAPI(
    info = Some(apiInfo),
    servers = servers,
    security = security,
    tags = tags,
    extensions = extensions
  )
}
