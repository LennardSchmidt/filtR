
#' Checking the validity of dataset
#'
#' @import pbapply
#'
#' @param effvar the column name of the
#' @param efffac the column name
#' @param filtervars a vector of column names
#' @param df a dataset
#' @param plot call plot.valid
#' @param smooth call smooth.valid
#'
#' @return a filtR object including estimations of effect size an power for all possible filtervariable x value combination
#'
#' @examples
#' library(filtR)
#' data <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' valid(effvar = "a", efffac = "b", df = data)
#'
#' @export

valid <- function(effvar, efffac,
                  filtervars = NULL, df,
                  plot = FALSE,
                  smooth = FALSE) {

  if( ! any(c("numeric","integer") %in% class(df[,effvar]))){
    stop("First parameter must be a numeric type")
  }

  if(is.null(filtervars)){
    filtervars <- subset(df, select = -c(eval(parse(text = effvar)), eval(parse(text = efffac))))
  } else {
    filtervars <- subset(df, select = filtervars)
  }

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(filtervars))

  # For every filtervar x value combination subset dataframe and append as list
  if (requireNamespace("pbapply", quietly = TRUE)) {
    op <- pbapply::pboptions(type="timer")
    output <- pbapply::pbapply(comb, 1, valid.subset, effvar = effvar, efffac = efffac, dat = df)
  } else {
    output <- apply(comb, 1, valid.subset, effvar = effvar, efffac = efffac, dat = df)
  }


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
#' @param effvar the name of the
#' @param efffac bla
#' @param dat bla
#'
#' @return a vector with validity criteria

valid.subset <- function(x, effvar, efffac, dat) {
  names <- names(x)

  # Filter gross dataset
  for (name in names) {
    y <- x[name]

    if (nrow(dat) == 0) {
      break
    } else if (is.factor(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) == factor(y, levels = levels(dat[, name])))
    } else if (is.numeric(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) <= as.double(y))
    } else if (is.character(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) == y)
    }
  }

  # Effect variables
  d <- dat[,effvar]
  f <- dat[,efffac]

  if( "character" %in% class(f)){
    f = factor(f)
  }

  # Within design
  if (! "factor" %in% class(f) ) {

    # Calculate net metrics
    n <- length(d)
    esize <- as.numeric(effsize::cohen.d(d, f, na.rm = T)$estimate)
    pwr <- pwr::pwr.t.test(n = n, d = esize, type = "paired")$power

    # Store net metrics
    results <- data.frame(`Sample Size` = n, Power = pwr, `Effect Size` = esize)

  # Between design
  } else {

    # Calculate net metrics
    ns = as.numeric(table(f))
    n1 = ns[1]
    n2 = ns[2]

    if(length(unique(n2)) != 2) {
      return(NA)
    }

    esize <- as.numeric(effsize::cohen.d(d, f, na.rm = T)$estimate)
    pwr <- pwr::pwr.t2n.test(n = n1, n2 = n2, d = esize)$power

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
