#' Checking the validity of dataset
#'
#' @param target1 the name of the
#' @param target2 bla
#' @param groupvar bla
#' @param dat bla
#'
#' @return a data.frame including
#'
#' @examples
#' library(filtR)
#' dat <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' target1 <- "a"
#' target2 <- "b"
#' valid(target1 = target1, target2 = target2, dat = dat)
#'
#' @export

valid <- function(target1, target2 = NULL, groupvar = NULL, dat) {

  # Split predictor variables
  if (is.null(groupvar)) {
    predictors <- subset(dat, select = -c(eval(parse(text = target1)), eval(parse(text = target2))))
  } else {
    predictors <- subset(dat, select = -c(eval(parse(text = target1)), eval(parse(text = groupvar))))
  }

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(predictors))

  # For every predictor-value combination subset dataframe and append as list
  output <- apply(comb, 1, subset_fun, target1 = target1, target2 = target2, groupvar = groupvar, dat = dat)

  # #Map over list and generate validity criteria
  results <- do.call(rbind, output)

  return(results)
}
