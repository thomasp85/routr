# better try
tri <- function(expr) try_fetch(expr, error = function(e) e)
is.condition <- function(x) inherits(x, 'condition')
is.error_cond <- function(x) is.condition(x) && inherits(x, 'error')

check_function_args <- function(x, args, ..., allow_null = FALSE, arg = caller_arg, call = caller_env()) {
  check_function(x, ..., allow_null = allow_null, arg = arg, call = call)
  if (!is.null(x) && !all(args %in% fn_fmls_names(x))) {
    cli::cli_abort("{.arg {arg}} must be a function with the following arguments: {.arg {fn_fmls_names(x)}}", call = call)
  }
  invisible(NULL)
}

check_named <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (!is.null(x) && !is_named2(x)) {
    cli::cli_abort("{.arg {arg}} must be named", call = call)
  }
  invisible(NULL)
}
