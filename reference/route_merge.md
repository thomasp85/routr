# Merge one route into another

This function allows you to combine two separate routes into one. This
is different from combining them in a routestack, because a request is
only matched to one handler in each route (thus combining them with
`route_merge()` will ensure only one handler is called).

## Usage

``` r
route_merge(x, route, use_root = TRUE)
```

## Arguments

- x, route:

  [Route](https://routr.data-imaginist.com/reference/Route-class.md)
  objects to merge. `route` will be merged into `x`

- use_root:

  Should the root of `route` be added to all its paths before it is
  merged into `x`

## Value

`x` with `route` merged into it

## Examples

``` r
route() |>
  route_add("HEAD", "*", function(...) {
    message("Someone's looking")
    TRUE
  }) |>
  route_merge(
    sizelimit_route()
  )
#> A route with 2 handlers
#> head:
#> • *
#> all:
#> • *
```
