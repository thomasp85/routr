# Combine routes in a stack

This function allows you to combine multiple routes into a stack in
order to dispatch on them until one of them returns `FALSE`. This allows
you to have a router that can pass a request through multiple handlers
before sending it along to the client or other middleware

## Usage

``` r
route_stack(x, ...)

# Default S3 method
route_stack(x, ...)

# S3 method for class 'Route'
route_stack(x, ...)

# S3 method for class 'AssetRoute'
route_stack(x, ...)

# S3 method for class 'RouteStack'
route_stack(x, ..., .after = NULL)
```

## Arguments

- x:

  A [Route](https://routr.data-imaginist.com/reference/Route-class.md)
  or
  [RouteStack](https://routr.data-imaginist.com/reference/RouteStack.md)
  object

- ...:

  one or more named
  [Route](https://routr.data-imaginist.com/reference/Route-class.md)
  objects

- .after:

  Where in the stack should the new routes be placed. `NULL` means place
  them at the end.

## Value

A [RouteStack](https://routr.data-imaginist.com/reference/RouteStack.md)
object. If `x` is a
[RouteStack](https://routr.data-imaginist.com/reference/RouteStack.md)
then this will be returned, modified.

## Examples

``` r
# Create an empty route stack
route_stack()
#> A RouteStack containing 0 routes

# Stack a route with another, returning a RouteStack
route(all = list("*" = function(...) TRUE)) |>
  route_stack(
    limit = sizelimit_route()
  )
#> A RouteStack containing 2 routes
#> 1. x
#> 2. limit
```
