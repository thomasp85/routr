#' Reject requests not in possession of the correct shared secret
#'
#' This route is a simple authentication method that limits requests based on
#' whether they are in possession of an agreed upon shared secret. Be aware that
#' if the request is send over HTTP then the secret will be visible to anyone
#' intercepting the request. For this reason you should only use this route in
#' combination with HTTPS or accept the probability that the secret is exposed.
#' If no shared secret is provided with the request *or* if the shared secret
#' doesn't match a `400L Bad Request` response is returned.
#'
#' @param secret The secret to check for in a request
#' @param header The name of the header to look for the secret
#'
#' @return A [Route] object
#'
#' @family Route constructors
#'
#' @export
#'
shared_secret_route <- function(secret, header) {
  check_string(secret)
  check_string(header)
  header <- paste0("HTTP_", toupper(gsub("-", "_", header)))
  routr::Route$new(all = list(
    "*" = function(request, response, ...) {
      if (!identical(secret, request$origin[[header]])) {
        response$status_with_text(400L)
        FALSE
      } else {
        TRUE
      }
    }
  ))
}
