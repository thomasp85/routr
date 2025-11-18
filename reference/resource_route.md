# Create a route for fetching files

This function creates a route mapping different paths to files on the
server filesystem. Different subpaths can be mapped to different
locations on the server so that e.g. `/data/` maps to `/path/to/data/`
and `/assets/` maps to `/a/completely/different/path/`. The route
support automatic expansion of paths to a default extension or file,
using compressed versions of files if the request permits it, and
setting the correct headers so that results are cached.

## Usage

``` r
resource_route(
  ...,
  default_file = "index.html",
  default_ext = "html",
  finalize = NULL,
  continue = FALSE
)
```

## Arguments

- ...:

  Named arguments mapping a subpath in the URL to a location on the file
  system. These mappings will be checked in sequence

- default_file:

  The default file to look for if the path does not map to a file
  directly (see Details)

- default_ext:

  The default file extension to add to the file if a file cannot be
  found at the provided path and the path does not have an extension
  (see Details)

- finalize:

  An optional function to run if a file is found. The function will
  receive the request as the first argument, the response as the second,
  and anything passed on through `...` in the `dispatch` method. Any
  return value from the function is discarded. The function must accept
  `...`

- continue:

  A logical that should be returned if a file is found. Defaults to
  `FALSE` indicating that the response should be send unmodified.

## Value

A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
object

## Details

The way paths are resolved to a file is, for every mounted location,

1.  Check if the path contains the mount point. If not, continue to the
    next mount point

2.  substitute the mount point for the local location in the path

3.  if the path ends with `/` add the `default_file` (defaults to
    `index.html`)

4.  see if the file exists along with compressed versions (versions with
    `.gz`, `.zip`, `.br`, `.zz` appended)

5.  if any version exists, chose the prefered encoding based on the
    `Accept-Encoding` header in the request, and return.

6.  if none exists and the path does not specify a file extension, add
    `default_ext` to the path and repeat 3-4

7.  if none exists still and the path does not specify a file extension,
    add `default_file` to the path and repeat 3-4

8.  if none exists still, continue to the next mount point

This means that for the path `/data/mtcars`, the following locations
will be tested (assuming the `/data/` -\> `/path/to/data/` mapping):

1.  `/path/to/data/mtcars`, `/path/to/data/mtcars.gz`,
    `/path/to/data/mtcars.zip`, `/path/to/data/mtcars.br`,
    `/path/to/data/mtcars.zz`

2.  `/path/to/data/mtcars.html`, `/path/to/data/mtcars.html.gz`,
    `/path/to/data/mtcars.html.zip`, `/path/to/data/mtcars.html.br`,
    `/path/to/data/mtcars.html.zz`

3.  `/path/to/data/mtcars/index.html`,
    `/path/to/data/mtcars/index.html.gz`,
    `/path/to/data/mtcars/index.html.zip`,
    `/path/to/data/mtcars/index.html.br`,
    `/path/to/data/mtcars/index.html.zz`

Assuming the default values of `default_file` and `default_ext`

If a file is not found, the route will simply return `TRUE` to hand of
control to subsequent routes in the stack, otherwise it will return the
logical value in the `continue` argument (defaults to `FALSE`, thus
shortcutting any additional routes in the stack).

If a file is found the request headers `If-Modified-Since` and
`If-None-Match`, will be fetched and, if exist, will be used to
determine whether a `304 - Not Modified` response should be send instead
of the file. If the file should be send, it will be added to the
response along with the following headers:

- `Content-Type` based on the extension of the file (without any
  encoding extensions)

- `Content-Encoding` based on the negotiated file encoding

- `ETag` based on
  [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) of the
  last modified date

- `Cache-Control` set to `max-age=3600`

Furthermore `Content-Length` will be set automatically by `httpuv`

Lastly, if found, the finalize function will be called, forwarding the
`request`, `response` and `...` from the `dispatch` method.

## See also

Other Route constructors:
[`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md),
[`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md),
[`shared_secret_route()`](https://routr.data-imaginist.com/reference/shared_secret_route.md),
[`sizelimit_route()`](https://routr.data-imaginist.com/reference/sizelimit_route.md)

## Examples

``` r
# Map package files
res_route <- resource_route(
  '/package_files/' = system.file(package = 'routr')
)

rook <- fiery::fake_request('http://example.com/package_files/DESCRIPTION')
req <- reqres::Request$new(rook)
res_route$dispatch(req)
#> [1] FALSE
req$response$as_list()
#> $status
#> [1] 200
#> 
#> $headers
#> $headers$`content-type`
#> [1] "text/plain"
#> 
#> $headers$`content-encoding`
#> [1] "identity"
#> 
#> $headers$etag
#> [1] "0f96278583ee2ac26943ee3bb7a76a60"
#> 
#> $headers$`cache-control`
#> [1] "max-age=3600"
#> 
#> $headers$`last-modified`
#> [1] "Tue, 18 Nov 2025 08:09:04 GMT"
#> 
#> $headers$`content-location`
#> [1] "/package_files/DESCRIPTION"
#> 
#> $headers$date
#> [1] "Tue, 18 Nov 2025 08:09:24 GMT"
#> 
#> 
#> $body
#>                                                file 
#> "/home/runner/work/_temp/Library/routr/DESCRIPTION" 
#> 
```
