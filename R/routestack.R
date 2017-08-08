#' Combine multiple routes for sequential routing
#'
#' The `RouteStack` class encapsulate multiple [Route]s and lets a request be
#' passed through each sequentially. If a route is returning `FALSE` upon
#' dispatch further dispatching is cancelled.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'RouteStack'-object is initialized using the \code{new()} method on the
#' generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{route <- RouteStack$new(...)}
#' }
#'
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{...} \tab  \tab Routes to add up front. Must be in the form of named
#'  arguments containing `Route` objects
#' }
#'
#' @section Field:
#' The following fields are accessible in a `RouteStack` object:
#'
#' \describe{
#'  \item{`header`}{A logical indicating whether the object should be added to
#'  the `header` event when used as a `fiery` plugin. Defaults to `FALSE` (the
#'  object will be added to the `request` event)}
#'  \item{`name`}{The plugin name (used by `fiery`). Will always return `'routr'`}
#' }
#'
#' @section Methods:
#' The following methods are accessible in a `RouteStack` object:
#'
#' \describe{
#'  \item{`add_route(route, name, after = NULL)`}{Adds a new route to the stack.
#'  `route` must be a `Route` object, `name` must be a string. If `after` is
#'  given the route will be inserted after the given index, if not (or `NULL`)
#'  it will be inserted in the end of the stack.}
#'  \item{`has_route(name)`}{Test if the routestack contains a route with the
#'  given name.}
#'  \item{`remove(name`}{Removes the route with the given name from the stack.}
#'  \item{`dispatch(request, ...`}{Passes a [reqres::Request] through the stack
#'  of route in sequence until one of the routes return `FALSE` or every route
#'  have been passed through. `...` will be passed on to the dispatch of each
#'  `Route` on the stack.}
#'  \item{`on_attach(app, ...)`}{Method for use by `fiery` when attached as a
#'  plugin. Should not be called directly.}
#' }
#'
#' @seealso [Route] for defining single routes
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.string assert_that has_attr is.count
#' @importFrom reqres as.Request is.Request
#'
#' @export
#'
#' @examples
#' # Create a new stack
#' routes <- RouteStack$new()
#'
#' # Populate it wih routes
#' first <- Route$new()
#' first$add_handler('all', '*', function(request, response, keys, ...) {
#'   message('This will always get called first')
#'   TRUE
#' })
#' second <- Route$new()
#' second$add_handler('get', '/demo/', function(request, response, keys, ...) {
#'   message('This will get called next if the request asks for /demo/')
#'   TRUE
#' })
#' routes$add_route(first, 'first')
#' routes$add_route(second, 'second')
#'
#' # Send a request through
#' rook <- fiery::fake_request('http://example.com/demo/', method = 'get')
#' req <- reqres::Request$new(rook)
#' routes$dispatch(req)
#'
RouteStack <- R6Class('RouteStack',
    public = list(
        # Methods
        initialize = function(...) {
            routes <- list(...)
            if (length(routes) > 0) {
                assert_that(has_attr(routes, 'names'))
                lapply(names(routes), function(name) {
                    self$add_route(routes[[name]], name)
                })
            }
        },
        print = function(...) {
            n_routes <- length(private$stack)
            cat('A RouteStack containing ', n_routes, ' routes\n', sep = '')
            for (i in seq_len(n_routes)) {
                cat(format(i, width = nchar(n_routes)), ': ', private$routeNames[i], '\n', sep = '')
            }
            invisible(self)
        },
        add_route = function(route, name, after = NULL) {
            assert_that(inherits(route, 'Route'))
            assert_that(is.string(name))
            if (is.null(after)) after <- length(private$stack)
            assert_that(after == 0 || is.count(after))
            if (self$has_route(name)) {
                stop('Route named "', name, '" already exists', call. = FALSE)
            }
            private$stack <- append(private$stack, list(route), after)
            private$routeNames <- append(private$routeNames, name, after)
            invisible(self)
        },
        has_route = function(name) {
            assert_that(is.string(name))
            name %in% private$routeNames
        },
        remove_route = function(name) {
            assert_that(is.string(name))
            ind <- match(name, private$routeNames)
            if (is.na(ind)) {
                warning('No route named "', name, '" exists')
            } else {
                private$stack <- private$stack[-ind]
                private$routeNames <- private$routeNames[-ind]
            }
            invisible(self)
        },
        dispatch = function(request, ...) {
            if (!is.Request(request)) {
                request <- as.Request(request)
            }
            for (route in private$stack) {
                continue <- route$dispatch(request, ...)
                if (!continue) break
            }
            continue
        },
        on_attach = function(app, ...) {
            assert_that(inherits(app, 'Fire'))
            event <- if (self$header) 'header' else 'request'
            app$on(event, function(server, id, request, arg_list) {
                self$dispatch(request, server = server, id = id, arg_list = arg_list)
            })
        }
    ),
    active = list(
        header = function(value) {
            if (missing(value)) return(private$HEADER)
            assert_that(is.flag(value))
            private$HEADER <- value
        },
        name = function() 'routr'
    ),
    private = list(
        # Data
        stack = list(),
        routeNames = character(),
        HEADER = FALSE
    )
)
