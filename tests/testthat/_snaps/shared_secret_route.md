# shared_secret_route validates inputs correctly

    Code
      shared_secret_route(123, "X-Secret")
    Condition
      Error in `shared_secret_route()`:
      ! `secret` must be a single string, not the number 123.

---

    Code
      shared_secret_route("my-secret", 123)
    Condition
      Error in `shared_secret_route()`:
      ! `header` must be a single string, not the number 123.

