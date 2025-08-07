# sizelimit_route validates numeric limits correctly

    Code
      sizelimit_route(-1)
    Condition
      Error in `sizelimit_route()`:
      ! `limit` must be a number larger than or equal to 0, not the number -1.

---

    Code
      sizelimit_route("not a number")
    Condition
      Error in `sizelimit_route()`:
      ! `limit` must be a number, not the string "not a number".

# sizelimit_route validates function limits correctly

    Code
      sizelimit_route(function() 1024)
    Condition
      Error in `sizelimit_route()`:
      ! `limit` must be a function with the following arguments: `request`

# sizelimit_route validates function limit return value

    Code
      route$dispatch(req)
    Condition
      Error in `handler()`:
      ! `limit` must be a number, not the string "not a number".

