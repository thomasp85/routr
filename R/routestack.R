#' @importFrom R6 R6Class
#' @importFrom assertthat is.scalar is.string assert_that
#' @importFrom reqres as.Request is.Request
#'
#' @export
#'
RouteStack <- R6Class('RouteStack',
    public = list(
        # Methods
        add_route = function(route, name) {
            assert_that(inherits(route, 'Route'))
            assert_that(is.string(name))
            if (self$has_route(name)) {
                stop('Route named "', name, '" already exists', call. = FALSE)
            }
            private$stack <- append(private$stack, list(route))
            private$routeNames <- append(private$routeNames, name)
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
        },
        dispatch = function(request, ...) {
            if (!is.Request(request)) {
                request <- as.Request(request)
            }
            for (route in private$stack) {
                continue <- route$dispatch(request, ...)
                if (!continue) break
            }
            request$respond()$as_list()
        }
    ),
    private = list(
        # Data
        stack = list(),
        routeNames = character()
    )
)
