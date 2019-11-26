# Wed Nov 20 19:22:08 2019 ------------------------------
#
# TODO
# Gewählte Filterkombination auf der Kurve als Sensitivität
# # Sensitivität auf der Kurve nach Effect Size
# # Sensitivität auf der Kurve nach Sample Size
# # Sensitivität auf der Kurve nach ähnlichstem Filter (i.e., zu Grunde liegende Variablen und Werte)
#
# -------------------------------------------------------


#' Checking the validity of dataset
#'
#' @param effvar numeric, the column name of the
#' @param efffac numeric or factor, the column name
#' @param exp numeric or factor, vector of column names
#' @param method character, ...
#' @param data numeric or factor, a dataset
#'
#' @return tbd
#'
#' @examples
#' library(filtR)
#'
#' @export

get_point <- function(effvar,
                      efffac,
                      exp,
                      method = "effect",
                      data)
  {

  if (!any(c("effect", "sample", "distance") %in% method)) {
    stop("Method type not available")
  }

  exp <- get_exp(exp)

  filtervars <- unlist(exp[["var"]])

  results <- get_filter(effvar = effvar,
                        efffac = efffac,
                        filtervars = filtervars,
                        data = data)

  id <- get_id(exp,
               results[["comb"]])

  sid <- get_sid(id,
                 results[["results"]],
                 method)

  idL <- sid[[1]]
  idU <- sid[[2]]

  results_point <- list(lower = c(Sample.Size = results[["results"]]$SS[idL],
                                  Power = results[["results"]]$PO[idL],
                                  Effect.Size = results[["results"]]$ES[idL],
                                  Confidence.Interval.Lower = results[["results"]]$CL[idL],
                                  Confidence.Interval.Upper = results[["results"]]$CU[idL]
                                  ),
                        point = c(Sample.Size = results[["results"]]$SS[id],
                                  Power = results[["results"]]$PO[id],
                                  Effect.Size = results[["results"]]$ES[id],
                                  Confidence.Interval.Lower = results[["results"]]$CL[id],
                                  Confidence.Interval.Upper = results[["results"]]$CU[id]
                                  ),
                        upper = c(Sample.Size = results[["results"]]$SS[idU],
                                  Power = results[["results"]]$PO[idU],
                                  Effect.Size = results[["results"]]$ES[idU],
                                  Confidence.Interval.Lower = results[["results"]]$CL[idU],
                                  Confidence.Interval.Upper = results[["results"]]$CU[idU]
                                  )
                        )

  return(results_point)

}

get_sid <- function(id,
                    results,
                    method)
  {

  x <- data.frame(
    SS = results$SS,
    PO = results$PO,
    ES = results$ES
  )

  if (method == "effect") {
    x <- x[order(x$ES), ]
    id <- which(row.names(x) == id)
    idL <- as.numeric(row.names(x[(id - 1),]))
    idU <- as.numeric(row.names(x[(id + 1),]))
  }

  if (method == "sample") {
    x <- x[order(x$SS), ]
    id <- which(row.names(x) == id)
    idL <- as.numeric(row.names(x[(id - 1),]))
    idU <- as.numeric(row.names(x[(id + 1),]))
  }

  # NOT IMPLEMENTED YET
  # if (method == "distance") {
  #   # x <- x[order(x$ES), ] #TODO: Calculate distance vector
  #   idL <- id + 1
  #   idU <- id - 1
  # }

  return(list(lower = idL, upper = idU))

  }

get_id <- function(exp,
                   comb,
                   method)
  {

  f <- NULL

  for (i in 1:length(exp[["var"]])) {

    f <- ifelse(is.null(f),
                f <- paste("comb$", exp[["var"]][[i]], " == ", exp[["value"]][[i]], sep = ""),
                paste(f, paste("comb$", exp[["var"]][[i]], " == ", exp[["value"]][[i]], sep = ""), sep = " & "))
  }


  id <- which(eval(parse(text = f)))

  if(length(id) == 0){
    stop("Filter values do not exist in data")
  }

  return(id)

}

get_exp <- function(exp)
  {

  var <- list()
  operator <- list()
  value <- list()
  i <- 1

  for (e in exp) {

    d <- gregexpr("[<>=!]=?", e)
    var[[i]] <- trimws(substr(e,
                              1,
                              (eval(d[[1]]) - 1)))
    operator[[i]] <- substr(e,
                            d[[1]],
                            d[[1]][1] + attr(d[[1]], "match.length") - 1)
    value[[i]] <- trimws(substr(e,
                                d[[1]][1] + attr(d[[1]], "match.length"),
                                nchar(e)))
    i <- i + 1

    }

  return(list(var = var, operator = operator, value = value))

  }

