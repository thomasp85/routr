# resource_route validates inputs

    Code
      resource_route(continue = "not_logical")
    Condition
      Error in `resource_route()`:
      ! `continue` must be `TRUE` or `FALSE`, not the string "not_logical".

---

    Code
      resource_route(finalize = "not_function")
    Condition
      Error in `resource_route()`:
      ! `finalize` must be a function or `NULL`, not the string "not_function".

---

    Code
      resource_route(finalize = function(request, response) { })
    Condition
      Error in `resource_route()`:
      ! `finalize` must be a function taking `...` as argument

---

    Code
      resource_route(default_file = 1)
    Condition
      Error in `resource_route()`:
      ! `default_file` must be a single string, not the number 1.

---

    Code
      resource_route(default_ext = 1)
    Condition
      Error in `resource_route()`:
      ! `default_ext` must be a single string, not the number 1.

---

    Code
      resource_route("path/to/dir")
    Condition
      Error in `resource_route()`:
      ! `...` must be named

