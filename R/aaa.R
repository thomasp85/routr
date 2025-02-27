check_function_args <- function(x, args, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  check_function(x, ..., allow_null = allow_null, arg = arg, call = call)
  if (!is.null(x) && !all(args %in% fn_fmls_names(x))) {
    cli::cli_abort("{.arg {arg}} must be a function with the following arguments: {.arg {args}}", call = call)
  }
  invisible(NULL)
}

check_named <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (!is.null(x) && !is_named2(x)) {
    cli::cli_abort("{.arg {arg}} must be named", call = call)
  }
  invisible(NULL)
}

http_methods <- c('get', 'head', 'post', 'put', 'delete', 'connect', 'options', 'trace', 'patch')
