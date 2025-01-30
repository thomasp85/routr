#' Construct a new route
#'
#' This function constructs a new [Route], optionally with a set of handlers
#' already attached.
#'
#' @param ... Handlers to add up front. Must be in the form of named lists
#' where the names corresponds to paths and the elements are the handlers.
#' The name of the argument itself defines the method to listen on (see
#' examples)
#' @param root The root of the route. Will be removed from the path of any
#' request before matching a handler
#'
#' @return A [Route] object
#'
#' @export
#'
#' @examples
#' # An empty route
#' route <- route()
#' route
#'
#' # Prepopulating it at construction
#' route <- route(all = list(
#'   '/*' = function(request, response, keys, ...) {
#'     message('Request received')
#'     TRUE
#'   }
#' ))
#' route
#'
route <- function(..., root = "") {
  x <- Route$new(...)
  x$root <- root
  x
}

#' Route handlers
#'
#' These functions helps you to add, remove, and retrieve route handlers. They
#' all (except for `route_get()`) returns the route for easy chaining.
#'
#' @param x A [Route] object
#' @param method A request method for the handler. The special method `"all"`
#' will allow the handler to match all http request methods that comes in.
#' @param path A URL path for the handler. See Paths for more on path
#' semantics
#' @param handler A handler function. See Handlers for more about the semantics
#' of handlers
#'
#' @return `x`, modified. `route_get()` returns the requested handler
#'
#' @details
#' # Paths
#' The path is a URL path consisting of strings, parameters (strings prefixed
#' with `:`), and wildcards (`*`), separated by `/`. A wildcard will match
#' anything and is thus not restricted to a single path element (i.e. it will
#' span multiple `/` if possible). When serving a request only a single handler
#' is selected based on the path match that is most specific. Specificity is
#' based on number of path parts (ie. number of elements separated by `/`), the
#' more the better; number of wildcards, the fewer the better; and number of
#' keys, the fewer the better. When printing the route you can see the priority
#' of all the paths in the route as they are sorted by that
#'
#' # Handlers
#' The handler is a function. At the very least it should have a `...` argument
#' and it must return eiter `TRUE` or `FALSE`. Returning `TRUE` means that the
#' request is allowed to continue processing and can be passed on to the next
#' route in the stack. Returning `FALSE` stops the processing of the request by
#' the stack.
#'
#' While any arguments besides `...` are optional, there are a few that will get
#' passed in named:
#'
#' * `request` will hold the request as a [reqres::Request] object
#' * `response` will hold the request as a [reqres::Response] object
#' * `keys` will be a named list containing the values of the matched path keys
#' (see example)
#'
#' Further, if routr is used as a fiery plugin, the handler will receive:
#'
#' * `server` is the `fiery::Fire` object defining the app
#' * `id` is the id of the client sending the request, as provided by fiery
#' * `arg_list` is a list of values as calculated by the servers `before-request`
#' event handlers
#'
#' Any and all of the above can be ignored by your handler, but accepting the
#' `server` is often paramount to more powerful features such as delayed
#' execution or logging.
#'
#' @export
#'
#' @examplesIf packageVersion("base") >= "4.1.0"
#' # Add a handler
#' route <- route() |>
#'   route_add("get", "/:what", function(request, response, keys, ...) {
#'     message("Requesting", keys$what)
#'     TRUE
#'   })
#' route
#'
#' # Retrieve the handler
#' route |> route_get("get", "/:what")
#'
#' # Remove the handler
#' route |> route_remove("get", "/:what")
#'
route_add <- function(x, method, path, handler) {
  x$add_handler(method, path, handler)
  x
}

#' @rdname route_add
#' @export
#'
route_remove <- function(x, method, path) {
  x$remove_handler(method, path)
  x
}

#' @rdname route_add
#' @export
#'
route_get <- function(x, method, path) {
  x$get_handler(method, path)
  x
}

#' Merge one route into another
#'
#' This function allows you to combine two separate routes into one. This is
#' different from combining them in a routestack, because a request is only
#' matched to one handler in each route (thus combining them with
#' `route_merge()` will ensure only one handler is called).
#'
#' @param x,route [Route] objects to merge. `route` will be merged into `x`
#' @param use_root Should the root of `route` be added to all its paths before
#' it is merged into `x`
#'
#' @return `x` with `route` merged into it
#'
#' @export
#'
#' @examplesIf packageVersion("base") >= "4.1.0"
#' route() |>
#'   route_add("HEAD", "*", function(...) {
#'     message("Someone's looking")
#'     TRUE
#'   }) |>
#'   route_merge(
#'     sizelimit_route()
#'   )
#'
route_merge <- function(x, route, use_root = TRUE) {
  x$merge_route(route, use_root)
  x
}

#' Combine routes in a stack
#'
#' This function allows you to combine multiple routes into a stack in order to
#' dispatch on them until one of them returns `FALSE`. This allows you to have a
#' router that can pass a request through multiple handlers before sending it
#' along to the client or other middleware
#'
#' @param x A [Route] or [RouteStack] object
#' @param ... one or more named [Route] objects
#' @param .after Where in the stack should the new routes be placed. `NULL`
#' means place them at the end.
#'
#' @return A [RouteStack] object. If `x` is a [RouteStack] then this will be
#' returned, modified.
#'
#' @export
#'
#' @examplesIf packageVersion("base") >= "4.1.0"
#' # Create an empty route stack
#' route_stack()
#'
#' # Stack a route with another, returning a RouteStack
#' route(all = list("*" = function(...) TRUE)) |>
#'   route_stack(
#'     limit = sizelimit_route()
#'   )
route_stack <- function(x, ...) {
  UseMethod("route_stack")
}
#' @rdname route_stack
#' @export
route_stack.default <- function(x, ...) {
  if (missing(x)) {
    return(RouteStack$new(...))
  }
  stop_input_type(x, cli::cli_fmt(cli::cli_text("a {.cls Route} or {.cls RouteStack} object")))
}
#' @rdname route_stack
#' @export
route_stack.Route <- function(x, ...) {
  RouteStack$new(x = x, ...)
}
#' @rdname route_stack
#' @export
route_stack.AssetRoute <- route_stack.Route
#' @rdname route_stack
#' @export
route_stack.RouteStack <- function(x, ..., .after = NULL) {
  dots <- list2(...)
  check_named(dots, arg = "...")
  check_number_whole(.after, min = 0, allow_null = TRUE)
  for (i in seq_along(dots)) {
    x$add_route(dots[[i]], names(dots)[[i]], .after)
    if (!is.null(.after)) .after <- .after + 1
  }
  x
}

