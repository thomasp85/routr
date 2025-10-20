# report_route validates inputs correctly

    Code
      report_route("/report", "nonexistent_file.Rmd")
    Condition
      Error in `report_route()`:
      ! `file` does not point to an existing file

---

    Code
      report_route("/report", "fixtures/reports/test.Rmd", max_age = "invalid")
    Condition
      Error in `report_route()`:
      ! `max_age` must be a whole number, not the string "invalid".

---

    Code
      report_route("/report", "fixtures/reports/test.Rmd", continue = "invalid")
    Condition
      Error in `report_route()`:
      ! `continue` must be `TRUE` or `FALSE`, not the string "invalid".

---

    Code
      report_route("/report", "fixtures/reports/test.Rmd", finalize = "not_function")
    Condition
      Error in `report_route()`:
      ! `finalize` must be a function or `NULL`, not the string "not_function".

---

    Code
      report_route("/report", "fixtures/reports/test.Rmd", finalize = function(req,
        res) { })
    Condition
      Error in `report_route()`:
      ! `finalize` take at least three arguments

---

    Code
      report_route("/report", "fixtures/reports/test.Rmd", async = "invalid")
    Condition
      Error in `report_route()`:
      ! `async` must be `TRUE` or `FALSE`, not the string "invalid".

# register_report_format works

    Code
      register_report_format(format_name, "text/new", "new", force = FALSE)
    Condition
      Error in `register_report_format()`:
      ! "test_format" already exists. Set `force = TRUE` to overwrite

# register_report_format auto-detects extension

    Code
      register_report_format("unknown_format", "application/x-unknown")
    Condition
      Error in `register_report_format()`:
      ! Failed to discover default file extension for "application/x-unknown"
      i Please set it using the `extension` argument

