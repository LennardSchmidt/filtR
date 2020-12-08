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

  if ("data.table" %in% class(data)){
    data = data.frame(data)
  }

  if (!(all(c(outcome, treatment) %in% colnames(data)))){
    stop("Variable not in data")
  }

  if (!is.null(filtervars)) {
    if (!(all(filtervars %in% colnames(data)))){
      stop("Variable not in data")
    }
  }

  if (!any(c("numeric", "integer") %in% class(data[, outcome]))) {
    stop("First parameter must be a numeric type")
  }

  # Create model formula
  if (control == TRUE) {
    formula <- as.formula(paste(outcome, "~", treatment, "+", paste(filtervars, collapse = "+")))
  } else {
    formula <- as.formula(paste(outcome, "~", treatment))
  }

  # Compute all possible filter combinations
  grid <- get_comb(
    outcome = outcome, treatment = treatment, filtervars = filtervars,
    data = data
  )

  # Compute model output for every combination
  output <- apply(grid, 1, get_results,
    outcome = outcome, treatment = treatment,
    data = data, formula = formula, cols = colnames(grid)
  )

  # Bind results to dataframe
  results <- do.call(rbind, output)
  results <- results[which(results$term == treatment), ]
  results <- tibble::rowid_to_column(results, "ID")

  # Should this be the distribution of the effect size depending on its
  # likelihood (we could then look at the elasticity by simple derivation
  # and additionally give the likelihood of a researcher chosing the filter)
  # or are we interested in the single values (and their interaction with
  # Sample Size or Power)

  # Set class for output
  class(results) <- append(class(results), "filtR")

  return(results)
}

#' Get all possible filter combinations
#'
#' @inheritParams get_filter
#'
#' @return An object of class "data.frame", which includes all unique possible combinations of filter variables

get_comb <- function(outcome, treatment, filtervars, data) {

  if (is.null(filtervars)) {
    filtervars <- data[, -which(colnames(data) %notin% c(outcome, treatment))]
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

plot.filtr <- function(x, metric, ...) {
  if (!inherits(x, "filtR")) {
    stop("use only with \"filtR\" objects")
  }

  x <- tidyr::gather(x, statistic, value, p.value:effect, factor_key = TRUE)

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(
      x[which(x$statistic == metric), ],
      ggplot2::aes(
        x = 100,
        y = eval(rlang::sym("ID")),
        fill = eval(rlang::sym("value"))
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::theme(
        legend.position = "top",
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        legend.key.size = grid::unit(0.01, "npc"),
        legend.key.width = grid::unit(0.1, "npc")
      ) +
      ggplot2::scale_fill_gradient2("",
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = (min(x$value[which(x$statistic == metric)], na.rm = T) + max(x$value[which(x$statistic == metric)], na.rm = T)) / 2,
        limits = c(min(x$value[which(x$statistic == metric)], na.rm = T), max(x$value[which(x$statistic == metric)], na.rm = T))
      )
  }

  print(p)
}

# Thu Nov 14 13:04:27 2019 ------------------------------

# WIP
# smooth.valid <- function()
