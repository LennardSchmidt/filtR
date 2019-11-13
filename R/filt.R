
#' Checking the validity of dataset
#'
#' @param target1 the name of the
#' @param target2 a
#' @param groupvar a treatment variable
#' @param dat a dataset
#' @param plot call plot.valid
#' @param smooth call smooth.valid
#'
#' @return a filtR object including estimations of effect size an power for each combination
#'
#' @examples
#' library(filtR)
#' dat <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' target1 <- "a"
#' target2 <- "b"
#' valid(target1 = target1, target2 = target2, df = dat)
#'
#' @export

valid <- function(target1, target2 = NULL,
                  groupvar = NULL, filtervar = NULL,
                  df,
                  plot = FALSE,
                  smooth = FALSE) {

  if (all(is.null(target2), is.null(groupvar))) {
    stop("Specify one option")
  }

  if (all(!is.null(target2), !is.null(groupvar))) {
    stop("Specify one option only")
  }

  # Split test and filter variables
  if (is.null(groupvar)) {
    # Get relevant dataframe
    if(is.null(filtervar)){
      dat <- df
    } else {
      dat <- subset(df, select = c(target1, target2, filtervar))
    }
    predictors <- subset(dat, select = -c(eval(parse(text = target1)), eval(parse(text = target2))))
  } else {
    # Get relevant dataframe
    if(is.null(filtervar)){
      dat <- df
    } else {
      dat <- subset(df, select = c(target1, groupvar, filtervar))
    }
    predictors <- subset(dat, select = -c(eval(parse(text = target1)), eval(parse(text = groupvar))))
  }

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(predictors))

  # For every predictor-value combination subset dataframe and append as list
  output <- apply(comb, 1, valid.subset_fun, target1 = target1, target2 = target2, groupvar = groupvar, dat = dat)

  # Map over list and generate validity criteria
  results <- do.call(rbind, output)

  # Set class
  class(results) <- "filtR"

  # Options
  if (smooth) {
    # smooth.valid(results)
  }

  if (plot) {
    plot.valid(results)
  }

  return(results)
}


#' Subsets original dataset and calculates validity metrics
#'
#' @param x input from wrapper
#' @param target1 the name of the
#' @param target2 bla
#' @param groupvar bla
#' @param dat bla
#'
#' @return a vector with validity criteria

valid.subset_fun <- function(x, target1, target2 = NULL, groupvar = NULL, dat) {
  names <- names(x)

  # Filter gross dataset
  for (name in names) {
    y <- x[name]

    if (nrow(dat) == 0) {
      break
    } else if (is.factor(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) == factor(y, levels = unique(dat[, name])))
    } else if (is.numeric(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) <= as.double(y))
    } else if (is.character(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) == y)
    }
  }

  # Within design
  if (is.null(groupvar)) {

    # Net dataset
    eff_data <- subset(dat, select = c(eval(parse(text = target1)), eval(parse(text = target2))))

    # Calculate net metrics
    n <- nrow(eff_data)
    esize <- as.numeric(effsize::cohen.d(eff_data[, target1], eff_data[, target2], na.rm = T)$estimate)
    pwr <- pwr::pwr.t.test(n = n, d = esize, type = "paired")$power

    # Store net metrics
    results <- data.frame(`Sample Size` = n, Power = pwr, `Effect Size` = esize)

    # Between design
  } else {

    # Net dataset
    eff_data <- subset(dat, select = c(eval(parse(text = target1)), eval(parse(text = groupvar))))

    # Calculate net metrics
    n1 <- table(eff_data[, groupvar])[1]
    n2 <- table(eff_data[, groupvar])[2]
    esize <- as.numeric(effsize::cohen.d(eff_data[, target1], eff_data[, groupvar], na.rm = T)$estimate)
    pwr <- tryCatch(
      {
        pwr::pwr.t2n.test(n = n1, n2 = n2, d = esize)$power
      },
      error = function(e) {
        return(NA)
      }
    )

    # Store net metrics
    results <- data.frame(`Sample Size` = (n1 + n2), Power = pwr, `Effect Size` = esize)
  }

  return(results)
}

#' Subsets original dataset and calculates validity metrics
#'
#' @param obj an filtR object
#' @param caption a caption for each plot
#' @param main a title for each plot
#'
#' @return a plot of effect size and power for all combinations
#'
#' @importFrom graphics plot
#'
#' @export

plot.valid <- function(obj, caption = c("Effect Size vs. Filter", "Power vs. Filter"), main = "") {
  if (!inherits(obj, "filtR")) {
    stop("use only with \"filtR\" objects")
  }

  # Drop 0/NA/ INF

  x <- data.frame(
    Sample.Size = obj$Sample.Size,
    Power = obj$Power,
    Effect.Size = obj$Effect.Size
  )

  x <- x[order(x$Sample.Size), ]
  x$ID <- c(1:nrow(x))

  plot(x$ID, x$Sample.Size, xlab = "Factor Level Combinations", ylab = "Sample.Size", main = main, type = "s")

  x <- x[order(x$Power), ]
  x$ID <- c(1:nrow(x))

  plot(x$ID, x$Power, xlab = "Factor Level Combinations", ylab = "Power", main = main, type = "s")

  x <- abs(x[order(x$Effect.Size, decreasing = F), ])
  x$ID <- c(1:nrow(x))

  plot(x$ID, x$Effect.Size, xlab = "Factor Level Combinations", ylab = "Effect Size", main = main, type = "s")
}


#smooth.valid
