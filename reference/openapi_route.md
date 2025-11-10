# Create a route for serving OpenAPI documentation of your server

This route facilitates serving the OpenAPI specs for your server, using
either [RapiDoc](https://rapidocweb.com),
[Redoc](https://redocly.com/redoc) or [Swagger](https://swagger.io) as a
UI for it. This function does not help you describe your API - you have
to provide the description for it yourself.

## Usage

``` r
openapi_route(
  spec,
  root = "__docs__",
  ui = c("rapidoc", "redoc", "swagger"),
  ...
)
```

## Arguments

- spec:

  The path to the json or yaml file describing your OpenAPI spec

- root:

  The point from which you want to serve your UI from

- ui:

  Either `"rapidoc"`, `"redoc"` or `"swagger"`, setting which UI to use

- ...:

  Further arguments passed on to the ui functions (e.g.
  rapidoc::rapidoc_spec())

## Value

A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
object

## See also

Other Route constructors:
[`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md),
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md),
[`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md),
[`sizelimit_route()`](https://routr.data-imaginist.com/reference/sizelimit_route.md)
