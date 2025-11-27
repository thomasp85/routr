#' @importFrom waysign path_params
Redirector <- R6Class(
  "Redirector",
  inherit = Route,
  public = list(
    initialize = function() {
      super$initialize()
    },
    redirect_temporary = function(method, from, to) {
      private$add_redirect(method, from, to, 307L)
    },
    redirect_permanently = function(method, from, to) {
      private$add_redirect(method, from, to, 308L)
    }
  ),
  private = list(
    add_redirect = function(method, from, to, status, call = caller_env()) {
      method <- tolower(method)
      method <- arg_match0(method, c(http_methods, "all"), error_call = call)
      check_string(from, call = call)
      check_string(to, call = call)
      from <- sub('\\?.+', '', from)
      to <- sub('\\?.+', '', to)
      from <- private$canonical_path(from)
      to <- private$canonical_path(to)

      handler <- make_redirect_handler(
        from = path_params(from),
        to = path_params(to),
        status,
        call
      )

      private$assign_handler(method, from, handler)
    }
  )
)

make_redirect_handler <- function(
  from,
  to,
  status,
  call = caller_env()
) {
  if (!all(to$keys %in% from$keys)) {
    cli::cli_abort(
      "{.arg to} cannot contain path parameters not present in {.arg from}",
      call = call
    )
  }
  force(status)
  function(request, response, keys, ...) {
    response$status <- status
    response$set_header("Location", glue::glue_data(keys, to$glue))
    FALSE
  }
}
