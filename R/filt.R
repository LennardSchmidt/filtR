
#' Checking the validity of dataset
#'
#' @import pbapply
#'
#' @param effvar numeric, the column name of the
#' @param efffac numeric or factor, the column name
#' @param filtervars numeric or factor, vector of column names
#' @param df numeric or factor, a dataset
#' @param sample boolean, if TRUE sample from observations
#' @param plot boolean, if TRUE call plot.valid
#' @param smooth boolean, if TRUE call smooth.valid
#'
#' @return a filtR object including estimations of effect size an power for all possible filtervariable x value combination
#'
#' @examples
#' library(filtR)
#' data <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' valid(effvar = "a", efffac = "b", df = data)
#' @export

valid <- function(effvar, efffac,
                  filtervars = NULL, df,
                  sample = FALSE,
                  plot = FALSE,
                  smooth = FALSE) {
  if (!any(c("numeric", "integer") %in% class(df[, effvar]))) {
    stop("First parameter must be a numeric type")
  }

  if (is.null(filtervars)) {
    filtervars <- subset(df, select = -c(eval(parse(text = effvar)), eval(parse(text = efffac))))
  } else {
    filtervars <- subset(df, select = filtervars)
  }

  # Sample observations for memory performance
  if(sample == TRUE){
    filtervars <- filtervars[sample(1:nrow(filtervars), round(nrow(filtervars)*0.25)),] # Check threshold
  }

  #Add "no-filter" case for each variable
  filtervars <- rbind(filtervars, c(rep(NA, ncol(filtervars))))

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(filtervars)) # Random sampling b/c of memory limit [add before NA_action]?

  # Compute validity metrics for every combination
  if (requireNamespace("pbapply", quietly = TRUE)) {
    op <- pbapply::pboptions(type = "timer")
    output <- pbapply::pbapply(comb, 1, valid.subset, effvar = effvar, efffac = efffac, dat = df)
  } else {
    output <- apply(comb, 1, valid.subset, effvar = effvar, efffac = efffac, dat = df)
  }

  # Create results datafrane
  results <- do.call(rbind, output)

  # Set class
  class(results) <- "filtR"

  # Set options
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

    if (is.na(y)) {
      next
    }

    if (nrow(dat) == 0) {
      return(data.frame(`Sample Size` = 0, Power = NA, `Effect Size` = NA))
    }

    if (is.factor(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) == factor(y, levels = levels(dat[, name])))
    }

    if (is.numeric(dat[, name])) {
      dat <- subset(dat, eval(parse(text = name)) < as.double(y))
    }
  }

  # Effect variables
  d <- dat[, effvar]
  f <- dat[, efffac]

  if ("character" %in% class(f)) {
    f <- factor(f)
  }

  # Within design
  if (!"factor" %in% class(f)) {

    # Calculate net metrics
    n <- length(d)

    if ( n < 2) {
      return(data.frame(`Sample Size` = n, Power = NA, `Effect Size` = NA))
    }

    esize <- as.numeric(effsize::cohen.d(d, f, na.rm = T)$estimate)
    pwr <- pwr::pwr.t.test(n = n, d = esize, type = "paired")$power

    # Store net metrics
    results <- data.frame(`Sample Size` = n, Power = pwr, `Effect Size` = esize)

    # Between design
  } else {

    # Calculate net metrics
    ns <- as.numeric(table(f))
    n1 <- ns[1]
    n2 <- ns[2]

    if (length(unique(f)) != 2 | any(n1 < 2, n2 < 2)) {
      return(data.frame(`Sample Size` = (n1+n2), Power = NA, `Effect Size` = NA))
    }

    # Effect size and power
    esize <- as.numeric(effsize::cohen.d(d, f, na.rm = T)$estimate)
    pwr <- pwr::pwr.t2n.test(n = n1, n2 = n2, d = esize)$power

    # Store net metrics
    results <- data.frame(`Sample Size` = (n1 + n2), Power = pwr, `Effect Size` = esize)
  }

  return(results)
}

# Thu Nov 14 13:04:18 2019 ------------------------------

# WIP
# valid.point <- function()

#' Plots validity metrics
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

  plot(x$ID, x$Sample.Size, xlab = "Combination", ylab = "Sample.Size", main = main, type = "s")

  # x <- x[order(x$Power), ]
  # x$ID <- c(1:nrow(x))

  plot(x$ID, x$Power, xlab = "Combination", ylab = "Power", main = main, type = "s")

  # x <- abs(x[order(x$Effect.Size, decreasing = F), ])
  # x$ID <- c(1:nrow(x))

  plot(x$ID, x$Effect.Size, xlab = "Combination", ylab = "Effect Size", main = main, type = "s")
}

# Thu Nov 14 13:04:27 2019 ------------------------------

# WIP
# smooth.valid <- function()
