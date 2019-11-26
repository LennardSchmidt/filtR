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
#' @param effvar numeric, the column name of the
#' @param efffac numeric or factor, the column name
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
#' get_filter(effvar = "a", efffac = "b", data = data)
#'
#' @export

get_filter <- function(effvar,
                       efffac,
                       filtervars = NULL,
                       data,
                       sample = FALSE,
                       plot = FALSE,
                       smooth = FALSE)
  { # add direction?

  if (!any(c("numeric", "integer") %in% class(data[, effvar]))) {
    stop("First parameter must be a numeric type")
  }

  comb <- get_comb(effvar = effvar,
                   efffac = efffac,
                   filtervars = filtervars,
                   data = data,
                   sample = sample)

  # Compute validity metrics for every combination
  if (requireNamespace("pbapply", quietly = TRUE)) {
    op <- pbapply::pboptions(type = "timer")
    output <- pbapply::pbapply(comb,
                               1,
                               get_subset,
                               effvar = effvar,
                               efffac = efffac,
                               data = data)
  } else {
    output <- apply(comb,
                    1,
                    get_subset,
                    effvar = effvar,
                    efffac = efffac,
                    data = data)
  }

  # Create results datafrane
  results <- list()
  results[["comb"]] <- comb
  results[["results"]] <- do.call(rbind,
                                  output)     # Should this be the distribution of the effect size depending on its
                                              # likelihood (we could then look at the elasticity by simple derivation
                                              # and additionally give the likelihood of a researcher chosing the filter)
                                              # or are we interested in the single values (and their interaction with
                                              # Sample Size or Power)

  # Set class
  class(results) <- "filtR"

  # Set options
  if (smooth) {
    # smooth.valid(results)
  }

  if (plot) {
    plot(results)
  }

  return(results)
}

#' Get all possible filter combinations
#'
#' @inheritParams get_filter
#'
#' @return a data with all combinations

get_comb <- function(effvar,
                     efffac,
                     filtervars,
                     data,
                     sample)
  {

  if (is.null(filtervars)) {
    filtervars <- subset(data,
                         select = -c(eval(parse(text = effvar)),
                                         eval(parse(text = efffac))))
  } else {
    filtervars <- subset(data,
                         select = filtervars)
  }

  # Get number of combinations
  # ncomb <- NULL
  # for (filtervar in filtervars) {
  #   ncomb <- ifelse(is.null(ncomb), ncomb <- unique(data[, filtervar]) + 1, ncomb * (unique(data[, filtervar]) + 1))
  # }
  #
  # prompt <- readline(prompt = paste("Your data inclues", ncomb, "possible combinations.Do you want to use sampling, please enter 1"))
  #
  # if (prompt == 1){
  #   sample = prompt
  # }

  # Sample observations for memory performance
  if (sample == TRUE) {
    filtervars <- filtervars[sample(1:nrow(filtervars),
                                    round(nrow(filtervars) * 0.25)), ]  # Use MonteCarlo of BLB Simulation
                                                                        # for high-dimensional problems -
                                                                        # what is high dimensional?
  }

  #Add "no-filter" case for each variable
  filtervars <- rbind(filtervars,
                      c(rep(NA, ncol(filtervars))))

  # Generate all possible predictor-value combinations
  comb <- unique(expand.grid(filtervars))

  return(comb)
}


#' Subsets original dataset and calculates validity metrics
#'
#' @inheritParams get_filter
#' @param x combination passed
#'
#' @return a vector with validity criteria

get_subset <- function(x,
                       effvar,
                       efffac,
                       data)
  {

  names <- names(x)

  # Filter gross dataset
  for (name in names) {
    y <- x[name]

    if (is.na(y)) {
      next
    }

    if (nrow(data) == 0) {
      return(data.frame(SS = 0,
                        PO = NA,
                        ES = NA,
                        CL = NA,
                        CU = NA))
    }

    if (is.factor(data[, name])) {
      data <- subset(data,
                    eval(parse(text = name)) == factor(y,
                                                       levels = levels(data[, name])))
    }

    if (is.numeric(data[, name])) {
      data <- subset(data,
                    eval(parse(text = name)) < as.double(y))
    }
  }

  # Effect variables
  d <- data[, effvar]
  f <- data[, efffac]

  if ("character" %in% class(f)) {
    f <- factor(f)
  }

  # Within design
  if (!"factor" %in% class(f)) {

    # Calculate net metrics
    n <- length(d)

    if (n < 2) {
      return(data.frame(SS = n,
                        PO = NA,
                        ES = NA,
                        CL = NA,
                        CU = NA))
    }

    esize <- effsize::cohen.d(d,
                              f,
                              na.rm = T) # generalize for multiple models using reformulate and aov?

    pwr <- pwr::pwr.t.test(n = n,
                           d = esize$estimate,
                           type = "paired"
                           )$power

    # Store net metrics
    results <- data.frame(SS = n,
                          PO = pwr,
                          ES = esize$estimate,
                          CL = esize$conf.int[[1]],
                          CU = esize$conf.int[[2]]
                          )

    # Between design
  } else {

    # Calculate net metrics
    ns <- as.numeric(table(f))
    n1 <- ns[1]
    n2 <- ns[2]

    if (length(unique(f)) != 2 | any(n1 < 2, n2 < 2)) {
      return(data.frame(SS = (n1 + n2),
                        PO = NA,
                        ES = NA,
                        CL = NA,
                        CU = NA))
    }

    # Effect size and power
    esize <- effsize::cohen.d(d,
                              f,
                              na.rm = T)

    pwr <- pwr::pwr.t2n.test(n = n1,
                             n2 = n2,
                             d = esize$estimate
                             )$power

    # Store net metrics
    results <- data.frame(SS = n1 + n2,
                          PO = pwr,
                          ES = esize$estimate,
                          CL = esize$conf.int[[1]],
                          CU = esize$conf.int[[2]]
    )
  }

  return(results)
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
                      #sorting = Sample.Size,
                      ...)
  {

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
       type = "p")

  graphics::plot(x$SS,
       x$PO,
       xlab = "Sample.Size",
       ylab = "Power",
       main = main,
       type = "p")

  if (requireNamespace("ggplot2", quietly = TRUE)){
    p <- ggplot2::ggplot(data = x,
                         ggplot2::aes(
                           x = SS,
                           y = ES)
                         ) +
      ggplot2::geom_point(ggplot2::aes(color = CL > 0 & CU > 0 | CL < 0 & CU < 0)) +
      ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = CL,
        ymax = CU,
        color =  CL > 0 & CU > 0 | CL < 0 & CU < 0),
      ) +
      ggplot2::scale_color_manual(name = "Significant", values = c("red","green")) +
      ggplot2::xlab("Sample Size") +
      ggplot2::ylab("Effect Size") +
      ggplot2::theme_minimal()

    print(p)
  }

}

# Thu Nov 14 13:04:27 2019 ------------------------------

# WIP
# smooth.valid <- function()
