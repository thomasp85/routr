# Route initialization works

    Code
      route$print()
    Output
      A route with 0 handlers

---

    Code
      route$print()
    Output
      A route with 1 handlers
      get: /test

# handlers can get added and removed

    Code
      route$print()
    Output
      A route with 1 handlers
      get: /test

---

    Code
      route$print()
    Output
      A route with 0 handlers

---

    Code
      route$remove_handler("get", "/test")
    Condition
      Warning:
      No handler assigned to get and /test

---

    Code
      route$add_handler("get", "/test", function(request) {
        FALSE
      })
    Condition
      Error:
      ! `handler` must be a function with the following arguments: `...`

# dispatch dispatches

    Code
      route$dispatch(rook)
    Condition
      Error in `route$dispatch()`:
      ! `request` must be a <Request> object, not an environment.

---

    Code
      route$dispatch(req)
    Condition
      Error:
      ! not working

---

    Code
      route$dispatch(req)
    Condition
      Error in `route$dispatch()`:
      ! `continue` must be `TRUE` or `FALSE`, not `NULL`.

