#' Limit the size of requests
#'
#' This route is meant for being called prior to retrieving of the request body.
#' It inspects the `Content-Length` header and determines if the request should
#' be allowed to proceed. The limit can be made variable by supplying a function
#' to the `limit` argument returning a numeric. If the `Content-Length` header
#' is missing and the limit is not `Inf` the response will be set to
#' `411 - Length Required`, If the header exists but exceeds the limit the
#' response will be set to `413 - Request Entity Too Large`. Otherwise the route
#' will return `TRUE` and leave the response unchanged.
#'
#' @param limit Either a numeric or a function returning a numeric when called
#' with the request
#' @param method The method this route should respond to. Defaults to `"all"`
#' @param path The URL path this route should respond to. Defaults to `"*"` (any
#' path)
#'
#' @return A [Route] object
#'
#' @export
#'
#' @family Route constructors
#'
#' @examples
#' limit_route <- sizelimit_route() # Default 5Mb limit
#' rook <- fiery::fake_request('http://www.example.com', 'post',
#'                             headers = list(Content_Length = 30*1024^2))
#' req <- reqres::Request$new(rook)
#' limit_route$dispatch(req)
#' req$respond()
#'
sizelimit_route <- function(limit = 5 * 1024^2, method = "all", path = "*") {
  if (is_function(limit)) {
    check_function_args(limit, "request")
  } else {
    check_number_decimal(limit, min = 0, allow_infinite = FALSE)
  }
  route <- Route$new()
  route$add_handler(method, path, function(request, response, keys, ...) {
    if (is_function(limit)) {
      limit <- limit(request = request)
      check_number_decimal(limit, min = 0, allow_infinite = FALSE)
    }
    req_length <- request$get_header('Content-Length')
    if (is.null(req_length) && limit < Inf) {
      response$status_with_text(411L)
      FALSE
    } else if (as.numeric(req_length) > limit) {
      response$status_with_text(413L)
      FALSE
    } else {
      TRUE
    }
  })
  route
}
