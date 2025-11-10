# High performance route for serving static files

An `asset_route()` is fundamentally different than the other routes
provided by routr. Conceptually it is akin to
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md)
in that it is used for serving static file content, but this route
circumvents the standard dispatch entirely (the request never enters the
R process). This makes it extremely fast but also somewhat limited as
you can't pass the request through any middleware. The choice between
`asset_route()` and
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md)
thus depends on your needs.

## Usage

``` r
asset_route(
  at,
  path,
  use_index = TRUE,
  fallthrough = FALSE,
  html_charset = "utf-8",
  headers = list(),
  validation = NULL,
  except = NULL
)
```

## Arguments

- at:

  The url path to listen to requests on

- path:

  The path to the file or directory on the file system

- use_index:

  Should an `index.html` file be served if present when a client
  requests the folder

- fallthrough:

  Should requests that doesn't match a file enter the request loop or
  have a 404 response send directly

- html_charset:

  The charset to report when serving html files

- headers:

  A list of headers to add to the response. Will be combined with the
  global headers of the app

- validation:

  An optional validation pattern. Presently, the only type of validation
  supported is an exact string match of a header. For example, if
  `validation` is `'"abc" = "xyz"'`, then HTTP requests must have a
  header named `abc` (case-insensitive) with the value `xyz`
  (case-sensitive). If a request does not have a matching header, than
  httpuv will give a 403 Forbidden response. If the `character(0)` (the
  default), then no validation check will be performed.

- except:

  One or more url paths that should be excluded from the route. Requests
  matching these will enter the standard router dispatch. The paths are
  interpreted as subpaths to `at`, e.g. the final path to exclude will
  be `at`+`exclude` (see example)

## Value

An
[AssetRoute](https://routr.data-imaginist.com/reference/AssetRoute.md)
object

## See also

Other Route constructors:
[`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md),
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md),
[`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md),
[`sizelimit_route()`](https://routr.data-imaginist.com/reference/sizelimit_route.md)

## Examples

``` r
asset_route("/wd", "./", except = "/private")
#> A route mapping files from ./ to /wd excluding /wd/private
#> 
#> ── Settings: 
#> use_index: TRUE
#> fallthrough: FALSE
#> html_charset: utf-8
#> headers: <none>
#> validation: <none>

```
