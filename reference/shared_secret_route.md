# Reject requests not in possession of the correct shared secret

This route is a simple authentication method that limits requests based
on whether they are in possession of an agreed upon shared secret. Be
aware that if the request is send over HTTP then the secret will be
visible to anyone intercepting the request. For this reason you should
only use this route in combination with HTTPS or accept the probability
that the secret is exposed. If no shared secret is provided with the
request *or* if the shared secret doesn't match a `400L Bad Request`
response is returned.

## Usage

``` r
shared_secret_route(secret, header)
```

## Arguments

- secret:

  The secret to check for in a request

- header:

  The name of the header to look for the secret

## Value

A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
object

## See also

Other Route constructors:
[`asset_route()`](https://routr.data-imaginist.com/reference/asset_route.md),
[`openapi_route()`](https://routr.data-imaginist.com/reference/openapi_route.md),
[`resource_route()`](https://routr.data-imaginist.com/reference/resource_route.md),
[`sizelimit_route()`](https://routr.data-imaginist.com/reference/sizelimit_route.md)
