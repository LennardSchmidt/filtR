# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more
# about package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package
# authoring: Install Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift +
# T'

# Define functions------------------------------

#' Subsets original dataset and calculates validity metrics
#'
#' @param x input from wrapper
#' @param t1 the name of the
#' @param t2 bla
#' @param gv bla
#' @param df bla
#'
#' @importFrom  rlang .data
#' @import dplyr
#' @import pwr
#' @import tibble
#' @import magrittr
#' @importFrom effsize cohen.d
#'
#' @return a tibble with validity criteria

subset_fun <- function(x, target1, target2, dat) {
  names <- names(x)

  # Filter
  for (name in names) {
    y <- x[name]

    if (is.factor(dat[, name])) {
      dat <- subset(dat, select = name == y)
    } else if (is.numeric(dat[, name])) {
      dat <- subset(dat, select = name <= y)
    } else if (is.character(dat[, name])) {
      dat <- subset(dat, select = name <= y)
    }
  }

  eff_data <- subset(dat, select = c(eval(parse(text = target1)), eval(parse(text = target2))))

  # Calculate metrics
  n <- nrow(eff_data)
  esize <- cohen.d(eff_data[target1], new_data[target2], na.rm = T)$estimate
  pwr <- pwr.t.test(n = n, d = esize, type = "paired")$power

  # Store metrics
  results <- data.frame(`Sample Size` = n, Power = pwr, `Effect Size` = esize)

  return(results)
}

#' Checking the validity of dataset
#'
#' @param target1 the name of the
#' @param target2 bla
#' @param groupvar bla
#' @param df bla
#'
#' @retun a tibble including
#'
#' @import dplyr
#' @import magrittr
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
#' @examples
#' library(filtR)
#' df <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' target1 <- "a"
#' target2 <- "b"
#' valid(target1 = target1, target2 = target2, df = df)
valid <- function(target1, target2 = NULL, groupvar = NULL, dat) {

  # Split predictor variables
  predictors <- subset(dat, select = -c(eval(parse(text = target1)), eval(parse(text = target2))))

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(predictors))

  # For every predictor-value combination subset dataframe and append as list
  output <- apply(comb, 1, subset_fun, target1 = target1, target2 = target2, dat = dat)


  # #Map over list and generate validity criteria
  results <- output %>% unnest(subset)
}
