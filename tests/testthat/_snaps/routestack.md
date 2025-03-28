# routestacks can be initiated with routes

    Code
      router$print()
    Message
      A RouteStack containing 0 routes

---

    Code
      router$print()
    Message
      A RouteStack containing 1 route
      1. test

# routes can be added, queried, and removed

    Code
      router$print()
    Message
      A RouteStack containing 1 route
      1. test

---

    Code
      router$print()
    Message
      A RouteStack containing 0 routes

---

    Code
      router$remove_route("test")
    Condition
      Warning:
      No route named "test" exists

---

    Code
      router$add_route(route, "test")
    Condition
      Error in `router$add_route()`:
      ! Route with name "test" already exists

---

    Code
      router$print()
    Message
      A RouteStack containing 4 routes
      1. test3
      2. test
      3. test4
      4. test2

# attach_to and name works as expected

    Code
      router$attach_to <- "test"
    Condition
      Error:
      ! `value` must be one of "request", "header", or "message", not "test".

---

    Code
      router$name <- "test"
    Condition
      Error:
      ! unused argument (base::quote("test"))

