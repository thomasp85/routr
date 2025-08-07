# openapi_route validates spec file exists

    Code
      openapi_route("nonexistent_file.json")
    Condition
      Error in `openapi_route()`:
      ! `spec` must point to an existing file

