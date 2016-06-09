#' @include request.R
#' @include response.R
NULL

#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.string assert_that
#'
#' @export
#'
RouteStack <- R6Class('RouteStack',
    public = list(
        # Methods
        add_route = function(route, name) {
            assert_that(inherits(route, 'Route'))
            is.scalar(name)
            is.string(name)
            if (self$has_route(name)) {
                stop('Route named "', name, '" already exists')
            }
            private$stack <- append(private$stack, list(route))
            private$routeNames <- append(private$routeNames, name)
        },
        has_route = function(name) {
            is.scalar(name)
            is.string(name)
            any(private$routeNames == name)
        },
        remove_route = function(name) {
            is.scalar(name)
            is.string(name)
            ind <- which(private$routeNames == name)
            if (length(ind) == 0) {
                warning('No route named "', name, '" exists')
            } else {
                private$stack <- private$stack[-ind]
                private$routeNames <- private$routeNames[-ind]
            }
        },
        dispatch = function(request, response, ...) {
            if (!inherits(request, 'Request')) {
                request <- Request$new(request)
            }
            if (missing(respones)) {
                response <- Response$new()
            }
            for (route in private$stack) {
                continue <- route$dispatch(request, response, ...)
                if (!continue) break
            }
            response$as_list()
        }
    ),
    private = list(
        # Data
        stack = list(),
        routeNames = character()
    )
)
