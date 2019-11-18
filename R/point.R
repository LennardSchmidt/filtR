
#' Checking the validity of dataset
#'
#' @param effvar numeric, the column name of the
#' @param efffac numeric or factor, the column name
#' @param exp numeric or factor, vector of column names
#' @param df numeric or factor, a dataset
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
                      df) {

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

  filtervars <- unlist(var)

  combs <- valid_get_comb(effvar = effvar,
                          efffac = efffac,
                          filtervars = filtervars,
                          df = df,
                          sample = FALSE)

  f <- NULL

  for (i in 1:length(var)) {

    f <- ifelse(is.null(f),
                f <- paste("combs$", var[i], " == ", value[i], sep = ""),
                paste(f, paste("combs$", var[i], " == ", value[i], sep = ""), sep = " & "))

  }

  id <- which(eval(parse(text = f)))

  if(length(id) == 0){
    stop("Filter combination does not exist in data")
  }

  results <- valid(effvar = effvar,
                   efffac = efffac,
                   filtervars = filtervars,
                   df = df)

  results_point <- c(results$Sample.Size[id],
                     results$Power[id],
                     results$Effect.Size[id])

  return(results_point)

}

# get_point.bootstrap <- function()
