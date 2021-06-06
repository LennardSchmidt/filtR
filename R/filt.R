# Wed Nov 20 19:22:08 2019 ------------------------------
#
# TODO
# Einflussstärke einer Filtervariable
# P Value plot : CI DONE
# Plots übereinander (ES vs. PV) : CI DONE
# Shiny App
# Check need for sampling?
#
# ------------------------------------------------------------

#' Checking the validity of dataset
#'
#' @param outcome numeric, the column name of the
#' @param treatment numeric or factor, the column name
#' @param data numeric or factor, a dataset
#' @param parallel bool
#'
#' @importFrom foreach %do% %dopar%
#'
#' @return a filtR object including estimations of effect size an power for all possible filtervariable x value combination
#'
#' @examples
#' library(filtR)
#' data <- data.frame(outcome = c(1:200), treatment = sample(rep(c(1, 2), 100)), filt = rnorm(200), filt2 = rnorm(200))
#' get_filter(outcome = "outcome", treatment = "treatment", data = data)
#' @export

get_filter <- function(outcome, treatment, data, parallel = TRUE) {
  # add direction?

  if ("data.table" %in% class(data)) {
    data <- data.frame(data)
  }

  if (!(all(c(outcome, treatment) %in% colnames(data)))) {
    stop("Outcome or treatment not in data")
  }

  # if (!is.null(filtervars)) {
  #   if (!(all(filtervars %in% colnames(data)))) {
  #     stop("Variable not in data")
  #   }
  # }

  if (!any(c("numeric", "integer") %in% class(data[, outcome]))) {
    stop("First parameter must be a numeric type")
  }

  # Create model formula
  # TODO: Add control option
  formula <- stats::as.formula(paste(outcome, "~", treatment))

  # Compute all possible filter combinations
  print("Computing combinations...")
  grid <- get_comb(
    outcome = outcome,
    treatment = treatment, # filtervars = filtervars,
    data = data
  )
  print(paste(nrow(grid), "combinations found"))

  #TODO: check for existing results
  print("Computing results...")
  if (isTRUE(parallel)) {
    if (!require("foreach")) {
      install.packages("foreach")
    }

    if (!require("doParallel")) {
      install.packages("doParallel")
    }

    # Initialize parallel processing
    no_cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = no_cores)
    cl <- parallel::makeCluster(no_cores, type = "PSOCK")

    # Compute model output for every combination
    output <- foreach::foreach(
      d = iterators::iter(grid, by = "row"),
      .combine = pbcombine(nrow(grid))
    ) %dopar%
      get_results(
        combination = d,
        data = data,
        formula = formula,
        treatment = treatment
      )

    # Stop cluster
    parallel::stopCluster(cl)
  } else {
    output <- apply_pb(grid, 1, get_results,
      data = data,
      formula = formula,
      treatment = treatment
    )
  }
  # Bind results to dataframe
  results <- data.frame(cbind(grid, output))
  results$ID <- seq(1:nrow(results))

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

get_comb <- function(outcome, treatment, data) {
  names <- names(data)[!(names(data) %in% c(outcome, treatment))]

  # TODO: Add filtervars option
  filtervars <- data[, names, drop = FALSE]

  # Add "no-filter" case for each variable
  filtervars <- rbind(filtervars, NA)

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(filtervars))

  return(comb)
}

#' Compute model output for each possible combination
#'
#' @param combination TBD
#' @param data TBD
#' @param formula TBD
#'
#' @return An object of class "data.frame", which includes the model output for the given combination of filter variables

get_results <- function(combination, data, formula, treatment) {
  # combination <- as.data.frame(t(combination))

  for (filter in colnames(combination)) {
    data <- data[data[, filter] < combination[1, filter], ]
  }

  # TODO: Add additional Models
  tryCatch(
    {
      model <- stats::aov(formula, data = data)

      results <- broom::tidy(model)
      results <- results[which(results$term == treatment), ]

      results["effect"] <- effectsize::cohens_d(formula, data = data)$cohens_d

      # TODO: Add Power
      # TODO: Add N
      # TODO: Add subset cache/check
    },
    error = function(e) results <<- NA
  )

  return(results)
}

# Thu Nov 14 13:04:27 2019 ------------------------------

# WIP
# smooth.valid <- function()
