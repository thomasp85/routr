#' Create a route for dispatching on URL
#'
#' The `Route` class is used to encapsulate a single URL dispatch, that is,
#' chose a single handler from a range based on a URL path. A handler will be
#' called with a request, response, and keys argument as well as any additional
#' arguments passed on to `dispatch()`.
#'
#' The path will strip the query string prior to assignment of the handler, can
#' contain wildcards, and can be parameterised using the `:` prefix. If there
#' are multiple matches of the request path the most specific will be chosen.
#' Specificity is based on number of elements (most), number of parameters
#' (least), and number of wildcards (least), in that order. Parameter
#' values will be available in the keys argument passed to the handler, e.g. a
#' path of `/user/:user_id` will provide `list(user_id = 123)` for a dispatch on
#' `/user/123` in the `keys` argument.
#'
#' Handlers are only called for their side-effects and are expected to return
#' either `TRUE` or `FALSE` indicating whether additional routes in a
#' [`RouteStack`] should be called, e.g. if a handler is returning `FALSE` all
#' further processing of the request will be terminated and the response will be
#' passed along in its current state. Thus, the intend of the handlers is to
#' modify the request and response objects, in place. All calls to handlers will
#' be wrapped in [try()] and if an exception is raised the response code will be
#' set to `500` with the body of the response being the error message. Further
#' processing of the request will be terminated. If a different error handling
#' scheme is wanted it must be implemented within the handler (the standard
#' approach is chosen to avoid handler errors resulting in a server crash).
#'
#' A handler is referencing a specific HTTP method (`get`, `post`, etc.) but can
#' also reference `all` to indicate that it should match all types of requests.
#' Handlers referencing `all` have lower precedence than those referencing
#' specific methods, so will only be called if a match is not found within the
#' handlers of the specific method.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'Route'-object is initialized using the \code{new()} method on the
#' generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{route <- Route$new(...)}
#' }
#'
#' \strong{Arguments}
#' \tabular{lll}{
#'  \code{...} \tab  \tab Handlers to add up front. Must be in the form of named
#'  lists where the names corresponds to paths and the elements are the handlers.
#'  The name of the argument itself defines the method to listen on (see examples)
#' }
#'
#' @section Methods:
#' The following methods are accessible in a `Route` object:
#'
#' \describe{
#'  \item{`add_handler(method, path, handler)`}{Add a handler to the specified
#'  method and path. The special method `'all'` will allow the handler to match
#'  all http request methods. The path is a URL path consisting of strings,
#'  parameters (strings prefixed with `:`), and wildcards (`*`), separated by
#'  `/`. A wildcard will match anything and is thus not restricted to a single
#'  path element (i.e. it will span multiple `/` if possible). The handler must
#'  be a function containing the arguments `request`, `response`, `keys`, and
#'  `...`, and must return either `TRUE` or `FALSE`. The `request` argument will
#'  be a [reqres::Request] object and the `response` argument will be a
#'  [reqres::Response] object matching the current exchange. The `keys` argument
#'  will be a named list with the value of all matched parameters from the path.
#'  Any additional argument passed on to the `dispatch` method will be avaiable
#'  as well. This method will override an existing handler with the same method
#'  and path.}
#'  \item{`remove_handler(method, path)`}{Removes the handler assigned to the
#'  specified method and path. If no handler have been assigned it will throw a
#'  warning.}
#'  \item{`dispatch(request, ...)`}{Based on a [reqres::Request] object the
#'  route will find the correct handler and call it with the correct arguments.
#'  Anything passed in with `...` will be passed along to the handler.}
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.scalar has_args assert_that is.flag has_attr
#' @importFrom uuid UUIDgenerate
#' @importFrom reqres is.Request
#' @importFrom stringi stri_match_first
#'
#' @export
#'
#' @seealso [RouteStack] for binding multiple routes sequentially
#'
#' @examples
#' # Initialise an empty route
#' route <- Route$new()
#'
#' # Initialise a route with handlers assigned
#' route <- Route$new(
#'   all = list(
#'     '/*' = function(request, response, keys, ...) {
#'       message('Request recieved')
#'       TRUE
#'     }
#'   )
#' )
#'
#' # Remove it again
#' route$remove_handler('all', '/*')
#'
Route <- R6Class('Route',
  public = list(
    # Methods
    initialize = function(...) {
      private$handlerMap = list()
      private$handlerStore = new.env(parent = emptyenv())
      handlers <- list(...)
      if (length(handlers) == 0) return()
      assert_that(has_attr(handlers, 'names'))
      lapply(names(handlers), function(method) {
        assert_that(has_attr(handlers[[method]], 'names'))
        lapply(names(handlers[[method]]), function(path) {
          assert_that(is.function(handlers[[method]][[path]]))
          self$add_handler(method, path, handlers[[method]][[path]])
        })
      })
    },
    print = function(...) {
      n_handlers <- length(ls(private$handlerStore))
      cat('A route with ', n_handlers, ' handlers\n', sep = '')
      if (n_handlers != 0) {
        method_order <- c('get', 'head', 'post', 'put', 'delete', 'connect', 'options', 'trace', 'patch', 'all')
        reg_methods <- names(private$handlerMap)
        map_order <- match(reg_methods, method_order)
        map_order[is.na(map_order)] <- sum(!is.na(map_order)) + seq_len(is.na(map_order))
        method_length <- max(nchar(reg_methods))
        for (i in order(map_order)) {
          paths <- names(private$handlerMap[[reg_methods[i]]])
          cat(format(reg_methods[i], width = method_length), ': ', paths[1], '\n', sep = '')
          for(j in 1 + seq_len(length(paths) - 1)) {
            cat(format(' ', width = method_length), ': ', paths[j], '\n', sep = '')
          }
        }
      }
      return(invisible(self))
    },
    add_handler = function(method, path, handler) {
      assert_that(is.string(method))
      assert_that(is.string(path))
      path <- sub('\\?.+', '', path)
      assert_that(has_args(handler,  c('request', 'response', 'keys', '...')))
      method <- tolower(method)

      id <- private$find_id(method, path)
      if (is.null(id)) {
        id <- private$make_id()
        private$add_id(method, path, id)
      }
      assign(id, handler, envir = private$handlerStore)
      invisible(self)
    },
    remove_handler = function(method, path) {
      id <- private$find_id(method, path)
      if (is.null(id)) {
        warning('No handler assigned to ', method, ' and ', path, call. = FALSE)
      } else {
        private$remove_id(id)
        rm(list = id, envir = private$handlerStore)
      }
      invisible(self)
    },
    dispatch = function(request, ...) {
      assert_that(is.Request(request))
      response <- request$respond()

      method <- request$method
      handlerInfo <- private$match_url(request$path, method)
      if (is.null(handlerInfo)) {
        handlerInfo <- private$match_url(request$path, 'all')
        if (is.null(handlerInfo)) return(TRUE)
      }
      handler <- private$handlerStore[[handlerInfo$id]]
      handlerKeys <- as.list(handlerInfo$values)
      names(handlerKeys) <- handlerInfo$keys
      continue <- handler(request, response, handlerKeys, ...)
      assert_that(is.flag(continue))
      continue
    }
  ),
  private = list(
    # Data
    handlerMap = NULL,
    handlerStore = NULL,
    # Methods
    find_id = function(method, path) {
      private$handlerMap[[method]][[path]]$id
    },
    find_handler = function(method, path) {
      id <- private$find_id(method, path)
      if (is.null(id)) return(NULL)
      private$handlerStore[[id]]
    },
    make_id = function() {
      id <- UUIDgenerate()
      while (!is.null(private$handlerStore[[id]])) {
        id <- UUIDgenerate()
      }
      id
    },
    add_id = function(method, path, id) {
      method <- tolower(method)
      path <- tolower(path)
      if (is.null(private$handlerMap[[method]])) {
        private$handlerMap[[method]] <- list()
      }
      path_reg <- private$path_to_regex(path)
      path_reg$id <- id
      private$handlerMap[[method]][[path]] <- path_reg
      private$sort_ids(method)
    },
    remove_id = function(id) {
      for (i in names(private$handlerMap)) {
        index <- which(vapply(private$handlerMap[[i]], `[[`, character(1), i = 'id') == id)
        if (length(index != 0)) {
          private$handlerMap[[i]][index] <- NULL
        }
      }
    },
    sort_ids = function(method) {
      n_tokens <- sapply(private$handlerMap[[method]], `[[`, 'n_tokens')
      n_keys <- sapply(private$handlerMap[[method]], `[[`, 'n_keys')
      n_wildcard <- sapply(private$handlerMap[[method]], `[[`, 'n_wildcard')
      sort_order <- order(n_tokens, -n_wildcard, -n_keys, decreasing = TRUE)
      private$handlerMap[[method]] <- private$handlerMap[[method]][sort_order]
    },
    path_to_regex = function(path) {
      path <- sub('^/', '', path)
      terminator <- if (grepl('/$', path)) '/$' else '$'
      path <- sub('/$', '', path)
      tokens <- strsplit(path, '/')[[1]]
      n_tokens <- length(tokens)
      keys <- grep('^:', tokens)
      wildcard <- which(tokens == '*')
      reg <- tokens
      reg[keys] <- '([^\\/]+?)'
      reg[wildcard] <- '.*'
      reg <- paste0('^/', paste(reg, collapse = '/'), terminator)
      list(
        regex = reg,
        n_tokens = n_tokens,
        n_keys = length(keys),
        n_wildcard = length(wildcard),
        keys = sub(':', '', tokens[keys])
      )
    },
    match_url = function(url, method) {
      if (is.null(private$handlerMap[[method]])) return(NULL)
      url <- tolower(url)
      regexes <- vapply(private$handlerMap[[method]], `[[`, character(1), i = 'regex')
      url_match <- NA
      for (i in seq_along(regexes)) {
        url_match <- stri_match_first(url, regex = regexes[i])[1,]
        if (!is.na(url_match[1])) {
          break
        }
      }
      if (!is.na(url_match[1])) {
        handlerInfo <- private$handlerMap[[method]][[i]]
        handlerInfo$values <- url_match[-1]
        handlerInfo
      } else {
        NULL
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
