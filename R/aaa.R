# better try
tri <- function(expr) {
  tryCatch(expr, error = function(e) e)
}
is.condition <- function(x) inherits(x, 'condition')
is.error_cond <- function(x) is.condition(x) && inherits(x, 'error')
