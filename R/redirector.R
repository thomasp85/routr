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

      to_keys <- private$path_to_regex(to)$keys

      handler <- make_redirect_handler(
        private$path_to_regex(from),
        to_keys,
        private$canonical_path(to),
        status,
        call
      )

      private$assign_handler(method, from, handler)
    }
  )
)

make_redirect_handler <- function(
  from,
  to_keys,
  to_path,
  status,
  call = caller_env()
) {
  if (from$n_wildcard > 1 && any(to_keys == "*")) {
    to_keys[to_keys == "*"] <- "*_1"
  }
  if (!all(to_keys %in% from$keys)) {
    cli::cli_abort(
      "{.arg to} cannot contain path parameters not present in {.arg from}",
      call = call
    )
  }
  to_match <- gregexpr("(:[^\\/]+)|(\\*)", to_path)
  force(status)
  function(request, response, keys, ...) {
    new_path <- to_path
    regmatches(new_path, to_match) <- list(unlist(keys[to_keys]))
    response$status <- status
    response$set_header("Location", new_path)
    FALSE
  }
}
