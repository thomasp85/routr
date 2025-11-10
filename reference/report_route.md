# Create a route that renders and serves an Rmarkdown or Quarto report

This route allows you to serve a report written as a Quarto/Rmarkdown
document. The report will be rendered on demand using the query params
as parameters for the report if they match, or by providing them in the
body of a POST request. Depending on the value of the value of `max_age`
the rendered report is kept and served without a re-render on subsequent
requests. The rendering can happen asynchronously in which case a
promise is returned.

## Usage

``` r
report_route(
  path,
  file,
  ...,
  max_age = Inf,
  async = TRUE,
  finalize = NULL,
  continue = FALSE,
  ignore_trailing_slash = FALSE,
  cache_dir = tempfile(pattern = "routr_report"),
  cache_by_id = FALSE,
  param_caster = identity
)
```

## Arguments

- path:

  The url path to serve the report from

- file:

  The quarto or rmarkdown file to use for rendering of the report

- ...:

  Further arguments to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  or
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

- max_age:

  The maximum age in seconds to keep a rendered report before initiating
  a re-render

- async:

  Should rendering happen asynchronously (using mirai)

- finalize:

  An optional function to run before sending the response back. The
  function will receive the request as the first argument, the response
  as the second, and the server as the third.

- continue:

  A logical that defines whether the response is returned directly after
  rendering or should be made available to subsequent routes

- ignore_trailing_slash:

  Should `path` be taken exactly or should both a version with and
  without a terminating slash be accepted

- cache_dir:

  The location of the render cache. By default a temporary folder is
  created for it.

- cache_by_id:

  Should caching be scoped by the user id. If the rendering is dependent
  on user-level access to different data this is necessary to avoid data
  leakage.

- param_caster:

  An optional function to convert the query/body parameters into the
  expected type, or a list with elements `query` and `body` each holding
  a function to convert their respective parts into the expected type.

## Value

A [route](https://routr.data-imaginist.com/reference/Route.md) object

## Details

Only the formats explicitely stated in the header of the report are
allowed and they can be selected in multiple ways. Either by appending
the name of the format as a subpath to the path (e.g.
`/report/revealjs`), by appending the extension of the output type to
the path (e.g. `/report.pdf`), or by standard content negotiation using
the `Content-Type` header of the request. For the latter two, it is only
possible to select the first format of any kind that has the same
mime-type/extension.
