# AssetRoute validates path exists

    Code
      AssetRoute$new(at = "/assets", path = "non/existent/path")
    Condition
      Error in `initialize()`:
      ! `non/existent/path` does not point to an existing file or directory

# AssetRoute validates input parameters

    Code
      AssetRoute$new(at = 123, path = "fixtures/test_files")
    Condition
      Error in `initialize()`:
      ! `at` must be a single string, not the number 123.

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", use_index = "yes")
    Condition
      Error in `initialize()`:
      ! `use_index` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", fallthrough = "yes")
    Condition
      Error in `initialize()`:
      ! `fallthrough` must be `TRUE` or `FALSE`, not the string "yes".

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", html_charset = 123)
    Condition
      Error in `initialize()`:
      ! `html_charset` must be a single string, not the number 123.

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", headers = list(
        "Content-Type"))
    Condition
      Error in `initialize()`:
      ! `headers` must be named

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", validation = 123)
    Condition
      Error in `initialize()`:
      ! `validation` must be a single string or `NULL`, not the number 123.

---

    Code
      AssetRoute$new(at = "/assets", path = "fixtures/test_files", except = 123)
    Condition
      Error in `initialize()`:
      ! `except` must be a character vector or `NULL`, not the number 123.

# AssetRoute path setter validates path exists

    Code
      route$path <- "non/existent/path"
    Condition
      Error:
      ! `non/existent/path` does not point to an existing file or directory

# AssetRoute headers setter validates input

    Code
      route$headers <- list("Content-Type")
    Condition
      Error:
      ! `value` must be named

---

    Code
      route$headers <- list(`Content-Type` = 123)
    Condition
      Error:
      ! `headers[[Content-Type]]` must be a single string, not the number 123.

