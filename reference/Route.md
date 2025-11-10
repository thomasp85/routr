# Construct a new route

This function constructs a new
[Route](https://routr.data-imaginist.com/reference/Route-class.md),
optionally with a set of handlers already attached.

## Usage

``` r
route(..., root = "")
```

## Arguments

- ...:

  Handlers to add up front. Must be in the form of named lists where the
  names corresponds to paths and the elements are the handlers. The name
  of the argument itself defines the method to listen on (see examples)

- root:

  The root of the route. Will be removed from the path of any request
  before matching a handler

## Value

A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
object

## Examples

``` r
# An empty route
route <- route()
route
#> A route with 0 handlers

# Prepopulating it at construction
route <- route(all = list(
  '/*' = function(request, response, keys, ...) {
    message('Request received')
    TRUE
  }
))
route
#> A route with 1 handler
#> all:
#> • /*
```
