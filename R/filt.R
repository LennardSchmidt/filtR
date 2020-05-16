# Wed Nov 20 19:22:08 2019 ------------------------------
#
# TODO
# Einflussstärke einer Filtervariable
# P Value plot : CI DONE
# Plots übereinander (ES vs. PV) : CI DONE
# Shiny App
#
# ------------------------------------------------------------

#' Checking the validity of dataset
#'
#' @param out numeric, the column name of the
#' @param treat numeric or factor, the column name
#' @param filtervars numeric or factor, vector of column names
#' @param data numeric or factor, a dataset
#' @param sample boolean, if TRUE sample from observations
#' @param plot boolean, if TRUE call plot.valid
#' @param smooth boolean, if TRUE call smooth.valid
#'
#' @return a filtR object including estimations of effect size an power for all possible filtervariable x value combination
#'
#' @examples
#' library(filtR)
#' data <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400))
#' get_filter(outcome = "a", treatment = "b", data = data)
#' @export

get_filter <- function(outcome, treatment, filtervars = NULL, data, control = FALSE) {
  # add direction?

  if (!any(c("numeric", "integer") %in% class(data[, outcome]))) {
    stop("First parameter must be a numeric type")
  }

  #Create model formula
  if (control == TRUE) {
    formula <- as.formula(paste(outcome, "~", treatment, "+", paste(filtervars, collapse = "+")))
  } else {
    formula <- as.formula(paste(outcome, "~", treatment))
  }

  # Compute all possible filter combinations
  grid <- get_comb(outcome = outcome, treatment = treatment, filtervars = filtervars,
                   data = data)

  # Compute model output for every combination
  output <- apply(grid, 1, get_results, outcome = outcome, treatment = treatment,
                  data = data, formula = formula, cols = colnames(grid))

  # Bind results to dataframe
  results <- do.call(rbind, output)

  results <- results[which(results$term == treatment),]

  # Should this be the distribution of the effect size depending on its
  # likelihood (we could then look at the elasticity by simple derivation
  # and additionally give the likelihood of a researcher chosing the filter)
  # or are we interested in the single values (and their interaction with
  # Sample Size or Power)

  # Set class for output
  class(results) <- "filtR"

  return(results)
}

#' Get all possible filter combinations
#'
#' @inheritParams get_filter
#'
#' @return An object of class "data.frame", which includes all unique possible combinations of filter variables

get_comb <- function(outcome, treatment, filtervars, data) {
  if (is.null(filtervars)) {
    filtervars <- data[, -which(colnames(data) %in% c(outcome, treatment))]
  } else {
    filtervars <- data[, filtervars]
  }

  # Add "no-filter" case for each variable
  filtervars <- rbind(filtervars, NA)

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(filtervars))

  return(comb)
}

#' Compute model output for each possible combination
#'
#' @inheritParams get_filter
#' @param formula An object of class "formula"
#' @param cols A vector of class "character"
#'
#' @return An object of class "data.frame", which includes the model output for the given combination of filter variables

get_results <- function(grid, outcome, treatment, data, formula, cols) {

  ids <- get_ids(grid, data, cols)

  data <- data[ids, ]

  browser()

  if (nrow(data) < 10) {
    return(NA)
  }

  if (any(c("character", "factor") %in% data[, outcome])) {
    fit <- glm(formula, data = data, family = "binomial")
  } else {
    fit <- lm(formula, data = data)
  }

  fstat <- unname(summary(fit)$fstatistic)

  results <- broom::tidy(fit)

  results["power"] <- pwr::pwr.f2.test(f2 = fstat[[1]], u = fstat[[2]], v = fstat[[3]], sig.level = 0.01)$power
  results["effect"] <- effectsize::effectsize(fit)$Std_Coefficient

  if (any(c("character", "factor") %in% data[, treatment])) {
    results["sample-1"] <- nrow(subset(data, treatment == "0"))
    results["sample-2"] <- nrow(subset(data, treatment == "0"))
  } else {
    results["sample-1"] <- nrow(data)
    results["sample-2"] <- NA
  }

  return(results)
}

#' Subset dataset given a combinatiuon of filter variables
#'
#' @inheritParams get_filter
#' @inheritParams get_results
#'
#' @return A vector of class "integer", which includes the rows that are within the filter thresholds

get_ids <- function(grid, data, cols) {

  grid <- data.frame(matrix(grid, ncol = length(grid)))
  colnames(grid) <- cols

  ids <- sapply(colnames(grid), function(x) {
    if (is.na(grid[, x])) {
      as.numeric(row.names(data))
    } else {
      if (any(c("character", "factor") %in% class(data[, x]))) {
        grid[, x] <- factor(grid[, x], levels = levels(data[, x]))
        which(data[, x] == grid[, x])
      } else {
        if ("factor" %in% class(grid[, x])) {
          grid[, x] <- as.integer(levels(grid[, x]))
        }
        which(data[, x] < grid[, x])
      }
    }
  })

  ids <- Reduce(intersect, ids)

  return(ids)
}


# Thu Nov 14 13:04:18 2019 ------------------------------

#' Plots validity metrics
#'
#' @param x an filtR object
#' @param caption a caption for each plot
#' @param main a title for each plot
#' @param ... Arguments to be passed to methods
#'
#' @return a plot of effect size and power for all combinations
#'
#' @export

plot_filtr <- function(x,
                       caption = c("Effect Size vs. Filter", "Power vs. Filter"),
                       main = "",
                       # sorting = Sample.Size,
                       ...) {
  if (!inherits(x, "filtR")) {
    stop("use only with \"filtR\" objects")
  }

  # Drop 0/NA/ INF

  x <- data.frame(
    SS = x[["results"]]$SS,
    PO = x[["results"]]$PO,
    ES = x[["results"]]$ES,
    CL = x[["results"]]$CL,
    CU = x[["results"]]$CU
  )

  x <- x[order(x$SS), ]

  graphics::plot(x$SS,
    x$ES,
    xlab = "Sample.Size",
    ylab = "Effect.Size",
    main = main,
    type = "p"
  )

  graphics::plot(x$SS,
    x$PO,
    xlab = "Sample.Size",
    ylab = "Power",
    main = main,
    type = "p"
  )

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = SS,
        y = ES
      )
    ) +
      ggplot2::geom_point(ggplot2::aes(color = CL > 0 & CU > 0 | CL < 0 & CU < 0)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = CL,
          ymax = CU,
          color = CL > 0 & CU > 0 | CL < 0 & CU < 0
        ),
      ) +
      ggplot2::scale_color_manual(name = "Significant", values = c("red", "green")) +
      ggplot2::xlab("Sample Size") +
      ggplot2::ylab("Effect Size") +
      ggplot2::theme_minimal()

    print(p)
  }
}

# Thu Nov 14 13:04:27 2019 ------------------------------

# WIP
# smooth.valid <- function()
