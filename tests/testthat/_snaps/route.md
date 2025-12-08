# Route initialization works

    Code
      route$print()
    Message
      A route with 0 handlers

---

    Code
      route$print()
    Message
      A route with 1 handler
      get:
      * /test

# handlers can get added and removed

    Code
      route$print()
    Message
      A route with 1 handler
      get:
      * /test

---

    Code
      route$print()
    Message
      A route with 0 handlers

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
      route$dispatch(rook)
    Condition
      Error in `route$dispatch()`:
      ! `request` must be a <Request> object, not an environment.

# keys are case sensitive

    Code
      r$dispatch(req)
    Output
      $key
      [1] "a_Test"
      
      [1] TRUE

