# Limit the size of requests

This route is meant for being called prior to retrieving of the request
body. It inspects the `Content-Length` header and determines if the
request should be allowed to proceed. The limit can be made variable by
supplying a function to the `limit` argument returning a numeric. If the
`Content-Length` header is missing and the limit is not `Inf` the
response will be set to `411 - Length Required`, If the header exists
but exceeds the limit the response will be set to
`413 - Request Entity Too Large`. Otherwise the route will return `TRUE`
and leave the response unchanged.

## Usage

``` r
sizelimit_route(limit = 5 * 1024^2, method = "all", path = "*")
```

## Arguments

- limit:

  Either a numeric or a function returning a numeric when called with
  the request

- method:

  The method this route should respond to. Defaults to `"all"`

- path:

  The URL path this route should respond to. Defaults to `"*"` (any
  path)

## Value

A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
object

## See also

Other Route constructors:
[`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md),
[`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md),
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md),
[`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md)

## Examples

``` r
limit_route <- sizelimit_route() # Default 5Mb limit
rook <- fiery::fake_request('http://www.example.com', 'post',
                            headers = list(Content_Length = 30*1024^2))
req <- reqres::Request$new(rook)
limit_route$dispatch(req)
#> [1] FALSE
req$respond()
#> ── An HTTP response ────────────────────────────────────────────────────────────
#> Status: 413 - Content Too Large
#> Content type: text/plain
#> → Responding to: http://www.example.com:80/
```
