#' @importFrom R6 R6Class
#' @importFrom httpuv decodeURIComponent
Request <- R6Class('Request',
    public = list(
        initialize = function(rook) {
            private$ROOK <- rook
            private$TYPE <- rook$REQUEST_METHOD
            private$URL <- rook$PATH_INFO
            private$QUERY <- private$parse_query(rook$QUERY_STRING)
            private$HEADERS <- private$get_headers(rook)
            private$CONTENT <- private$parse_content(rook$rook.input)
        }
    ),
    active = list(
        type = function() {
            private$TYPE
        },
        url = function() {
            private$URL
        },
        query = function() {
            private$QUERY
        },
        headers = function() {
            private$HEADERS
        },
        content = function(value) {
            if(missing(value)) return(private$CONTENT)
            else private$CONTENT <- value
        },
        rook = function() {
            private$ROOK
        }
    ),
    private = list(
        TYPE = '',
        URL = '',
        QUERY = list(),
        HEADERS = list(),
        CONTENT = '',
        ROOK = NULL,
        parse_query = function(query) {
            if(query == '') return(list())
            query <- decodeURIComponent(query)
            query <- sub('^\\?', '', query)
            query <- gsub('\\+', ' ', query)
            query <- strsplit(unlist(strsplit(query, '&|;')), '=')
            ans <- list()
            for(i in query) {
                ans[[i[1]]] <- append(ans[[i[1]]], i[2])
            }
            lapply(ans, type.convert, as.is=TRUE)
        },
        parse_content = function(input) {
            if(inherits(input, 'NullInputStream')) return(raw())

            input$read(input$.length)
        },
        get_headers = function(rook) {
            vars <- ls(rook)
            headers <- vars[grepl('^HTTP_', vars)]
            ans <- lapply(headers, function(head) {
                rook[[head]]
            })
            names(ans) <- sub('^HTTP_', '', headers)
            ans
        }
    )
)

InputStreamFake <- R6Class('InputStreamFake',
    public = list(
        initialize = function(content) {
            private$content <- content
        },
        read_lines = function(n = -1L) {
            content <- strsplit(private$content, '\n')[[1]]
            if (n < 0) {
                private$atLine <- length(content) + 1
                content
            } else {
                content <- content[seq(private$atLine, length.out = n)]
                private$atLine <- private$atLine + n
                content
            }
        },
        read = function(l = -1L) {
            private$content
        },
        rewind = function() {
            private$atLine <- 1
            invisible()
        }
    ),
    private = list(
        content = '',
        atLine = 1
    )
)

NullInputStreamFake <- R6Class('NullInputStreamFake',
    public = list(
        read_lines = function(n = -1L) {
            character()
        },
        read = function(l = -1L) {
            raw()
        },
        rewind = function() invisible(),
        close = function() invisible()
    )
)

fake_request <- function(url, method = 'get', appLocation = '', content = '', headers = list(), ...) {
    rook <- new.env(parent = emptyenv())
    rook$REQUEST_METHOD <- toupper(method)

    # Split up URL
    url <- strsplit(url, '://')[[1]]
    if (length(url) == 1) {
        scheme <- 'http'
    } else {
        scheme <- url[1]
        url <- url[-1]
    }
    rook$rook.url_scheme <- scheme
    url <- strsplit(url, '/')[[1]]
    server <- url[1]
    path <- url[-1]
    server <- strsplit(server, ':')[[1]]
    rook$SERVER_NAME <- server[1]
    if (length(server) == 1) {
        rook$SERVER_PORT <- '80'
    } else {
        rook$SERVER_PORT <- server[2]
    }
    if (appLocation != '') {
        appLocation <- strsplit(appLocation, '/')[[1]]
        appInd <- seq_len(length(appLocation))
        if (!identical(tolower(appLocation), tolower(path)[appInd])) {
            stop('appLocation must correspond to the beginning of the path')
        }
        rook$SCRIPT_NAME <- paste0('/', paste(path[appInd], collapse = '/'))
        path <- path[-appInd]
    }
    path <- paste0('/', paste(path, collapse = '/'))
    path <- strsplit(path, '?', fixed = TRUE)[[1]]
    if (length(path) == 1) {
        rook$QUERY_STRING <- ''
    } else {
        rook$QUERY_STRING <- path[2]
    }
    rook$PATH_INFO <- path[1]

    # Misc
    rook$httpuv.version <- packageVersion('httpuv')
    rook$rook.version <- "1.1-0"
    rook$REMOTE_PORT <- paste(sample(0:9, 5, TRUE), collapse = '')

    # Headers
    if (length(headers) > 0) {
        names(headers) <- paste0('HTTP_', sub('^HTTP_', '', toupper(names(headers))))
        for (i in names(headers)) {
            assign(i, headers[[i]], envir = rook)
        }
    }

    # Extra
    extra <- list(...)
    for (i in extra) {
        assign(i, extra[[i]], envir = rook)
    }

    # Input
    if (content == '') {
        rook$rook.input <- NullInputStreamFake$new()
    } else {
        rook$rook.input <- InputStreamFake$new(content)
    }

    rook
}
