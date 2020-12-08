library(crul)
library(jsonlite)
library(tidyverse)
library(data.table)

token <- '8Xms4CPMIXVZbC0czZ4mwzw9rt5SnhWQwSa3kA4E1wPGNCzbo71p6xfH2X7NHo105Z5kcP'

url <- 'https://api.osf.io/v2/registrations'

user_agent <- function(agent = "osfr") {
  version <- system.file("DESCRIPTION", package = "osfr", mustWork = FALSE)
  if (file.exists(version)) {
    version <- base::read.dcf(version, "Version")
    sprintf("%s v%s", agent, version)
  } else {
    agent
  }
}

process_response <- function(res) {
  stopifnot(class(res)[1] == "HttpResponse")

  if (res$status_code > 500) {
    abort(paste0(
      "Encountered an unexpected error with the OSF API\n",
      "Please report this at https://github.com/ropensci/osfr/issues\n",
      "* Status code: ", res$status_code, "\n",
      "* Request: ", res$request$url$url, "\n",
      "* Resonse: \n", res$parse("UTF-8")
    ))
  }

  out <- jsonlite::fromJSON(res$parse("UTF-8"), simplifyVector = F)
  out$data <- out$data %>% map(rlist::list.flatten) %>% map(rbind.data.frame) %>% map(~ .x %>% mutate_all(as.character)) %>% bind_rows()
  out
}

is_entity_collection <- function(x) is.null(names(x$data))

parse_datetime_attrs <- function(x) {
  stopifnot(is.list(x))
  stopifnot("attributes" %in% names(x))

  dt_keys <- intersect(
    c("date_registered", "date_created", "date_modified", "modified_utc"),
    names(x$attributes)
  )

  dt_vals <- lapply(x$attributes[dt_keys], parse_datetime)

  x$attributes <- modifyList(x$attributes, dt_vals)
  return(x)
}

parse_datetime <- function(x) {
  if (is.null(x)) return(x)
  as.POSIXct(x, format = "%Y-%m-%dT%X", tz = "UTC")
}

.process_osf_response <- function(x) {
  if (is.null(x$errors)) {
    # process dates in successful responses
    x["data"] <- purrr::modify_depth(
      x["data"],
      .f = parse_datetime_attrs,
      .depth = ifelse(is_entity_collection(x), 2, 1)
    )
  }

  x
}

prepend_version <- function(path, version) {
  stopifnot(is.character(path))
  stopifnot(is.numeric(version))
  path <- sub("^\\/?v\\d\\/", "", path)
  url_path(paste0("v", floor(version)), path)
}

url_path <- function(...) {
  gsub("\\/{2,}", "/", paste0(list(...), collapse = "/"))
}

retry_message <- function(res, time) {
  msg <- sprintf(
    "Request failed (Status code: %s). Retrying in %ds...",
    res$status_code, ceiling(time)
  )
  if (!is.null(getOption("osfr.log"))) logger::log_info(msg)
  message(msg)
}

.build_client <-
  function(api,
           encode,
           version = NULL,
           progress = NULL,
           pat = getOption("osfr.pat")) {

    api <- match.arg(api, c("osf", "wb"))
    encode <- match.arg(encode, c("form", "multipart", "json", "raw"))
    server <- Sys.getenv("OSF_SERVER")

    url <- switch(api,
                  osf = ifelse(nzchar(server), "api.%s.osf.io",      "api.osf.io/v2/registrations"),
                  wb  = ifelse(nzchar(server), "files.us.%s.osf.io", "files.osf.io")
    )

    if (nzchar(server)) url <- sprintf(url, server)

    # assemble headers
    headers <- list(`User-Agent` = user_agent())

    if (!is.null(pat)) {
      headers$Authorization <- sprintf("Bearer %s", pat) # nolint
    }

    if (api == "osf") {
      headers$`Accept-Header` <- sprintf( # nolint
        "application/vnd.api+json;version=%s",
        version)
    }

    crul::HttpClient$new(
      url = paste0("https://", url),
      opts = list(
        encode = encode
      ),
      headers = headers,
      # hooks = list(
      #   request = log_request,
      #   response = log_response
      # ),
      progress = progress
    )
  }

.osf_request <-
  function(method,
           path,
           page = 1,
           body = NULL,
           verbose = FALSE,
           version = 2.8,
           ...) {

    method <- match.arg(method, c("get", "put", "patch", "post", "delete"))
    cli <- .build_client(api = "osf", encode = "json", version = version)

    cli$retry(
      method,
      #prepend_version(path, version),
      query = list(page = page),
      body = body,
      times = 3,
      retry_only_on = "502",
      onwait = retry_message
    )
  }

osf_paginated_request <-
  function(method,
           path,
           query = list(),
           n_max = Inf,
           verbose = FALSE) {

    items <- list()
    i <- 1
    retrieved <- 0

    repeat {

      res <- .osf_request(method, path, page = i)
      out <- process_response(res)

      total <- out$links$meta$total
      n_max <- ifelse(is.infinite(n_max), total, n_max)

      retrieved <- retrieved + nrow(out$data)
      items <- bind_rows(items, out$data)

      if (verbose && n_max > 10) {

        if (i == 1) {
          message(sprintf("Retrieving %i of %i available items:", n_max, total))
        }
        message(sprintf("..retrieved %i items", retrieved), appendLF = TRUE)
      }

      if (is.null(out$links$`next`) || retrieved >= total) {
        if (verbose && i > 1 && n_max > 10) message("..done")
        break
      }
      i <- i + 1
    }
    items
  }

osf_paginated_request(method = "get", path = url, query = list(), verbose = TRUE) %>%
  mutate_all(~na_if(., '')) %>%
  write.csv("ofs_registrations.csv")

