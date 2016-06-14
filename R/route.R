#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.scalar %has_args% assert_that
#' @importFrom uuid UUIDgenerate
#'
#' @export
#'
Route <- R6Class('Route',
    public = list(
        # Methods
        initialize = function() {
            private$handlerMap = list()
            private$handlerStore = new.env(parent = emptyenv())
        },
        add_handler = function(method, path, handler) {
            is.string(method)
            is.scalar(method)
            is.string(path)
            is.scalar(path)
            handler %has_args% c('request', 'response', 'keys', '...')
            method <- tolower(method)

            id <- private$find_id(method, path)
            if (is.null(id)) {
                id <- private$make_id()
                private$add_id(method, path, id)
            }
            assign(id, handler, envir = private$handlerStore)
        },
        remove_handler = function(id) {
            if (is.null(private$handlerStore[[id]])) {
                warning('No handler assigned to ', id, call. = FALSE)
                return()
            }
            private$remove_id(id)
            rm(id, envir = private$handlerStore)
        },
        dispatch = function(request, response, ...) {
            assert_that(inherits(request, 'Request'))
            assert_that(inherits(response, 'Response'))

            method <- request$type
            if (is.null(private$handlerMap[[method]])) {
                if (is.null(private$handlerMap$all)) {
                    return(TRUE)
                } else {
                    method <- 'all'
                }
            }
            handlerInfo <- private$match_url(request$url, method)
            if (is.null(handlerInfo)) return(TRUE)
            handler <- private$handlerStore[[handlerInfo$id]]
            handlerKeys <- as.list(handlerInfo$values)
            names(handlerKeys) <- handlerInfo$keys
            continue <- tryCatch(
                handler(request, response, handlerKeys, ...),
                error = function(e) {
                    response$status <- 500L
                    response$headers[['Content-Type']] <- 'text/html'
                    response$body <- geterrmessage()
                    FALSE
                }
            )
            assertthat::is.flag(continue)
            continue
        }
    ),
    private = list(
        # Data
        handlerMap = NULL,
        handlerStore = NULL,
        # Methods
        find_id = function(method, path) {
            private$handlerMap[[method]][[path]]$id
        },
        find_handler = function(method, path) {
            id <- private$find_id(method, path)
            if (is.null(id)) return(NULL)
            private$handlerStore[[id]]
        },
        make_id = function() {
            id <- UUIDgenerate()
            while (!is.null(private$handlerStore[[id]])) {
                id <- UUIDgenerate()
            }
            id
        },
        add_id = function(method, path, id) {
            method <- tolower(method)
            path <- tolower(path)
            if (is.null(private$handlerMap[[method]])) {
                private$handlerMap[[method]] <- list()
            }
            pathReg <- private$path_to_regex(path)
            pathReg$id <- id
            private$handlerMap[[method]][[path]] <- pathReg
            private$sort_ids(method)
        },
        remove_id = function(id) {
            for (i in names(private$handlerMap)) {
                index <- which(unlist(private$handlerMap[[i]]) == id)
                if (length(index != 0)) {
                    private$handlerMap[[i]][index] <- NULL
                }
            }
        },
        sort_ids = function(method) {
            nTokens <- sapply(private$handlerMap[[method]], `[[`, 'nTokens')
            nKeys <- sapply(private$handlerMap[[method]], `[[`, 'nKeys')
            wildcard <- sapply(private$handlerMap[[method]], `[[`, 'wildcard')
            sortOrder <- order(nTokens, nKeys, !wildcard, decreasing = TRUE)
            private$handlerMap[[method]] <- private$handlerMap[[method]][sortOrder]
        },
        path_to_regex = function(path) {
            tokens <- strsplit(path, '/')[[1]]
            nTokens <- length(tokens)
            keys <- grep('^:', tokens)
            wildcard <- which(tokens == '*')
            reg <- tokens
            reg[keys] <- '([^\\/]+?)'
            reg[wildcard] <- '.*'
            reg <- paste(reg, collapse = '/')
            list(
                regex = reg,
                nTokens = nTokens,
                nKeys = length(keys),
                wildcard = length(wildcard) != 0,
                keys = list(sub(':', '', tokens[keys]))
            )
        },
        match_url = function(url, method) {
            url <- tolower(url)
            regexes <- sapply(private$handlerMap[[method]], `[[`, 'regex')
            found <- FALSE
            urlMatch <- NA
            for (i in seq_along(regexes)) {
                urlMatch <- stri_match_first(url, regex = regexes[i])[1,]
                if (!is.na(urlMatch)) {
                    found <- TRUE
                    break
                }
            }
            if (found) {
                handlerInfo <- private$handlerMap[[method]][[i]]
                handlerInfo$values <- urlMatch[-1]
                handlerInfo
            } else {
                NULL
            }
        }
    ),
    lock_objects = TRUE,
    lock_class = TRUE
)
