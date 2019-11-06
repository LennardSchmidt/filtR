# Hello, world!  This is an example function named 'hello' which prints 'Hello, world!'.  You can learn more
# about package authoring with RStudio at: http://r-pkgs.had.co.nz/ Some useful keyboard shortcuts for package
# authoring: Install Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package: 'Ctrl + Shift +
# T'

# Define functions------------------------------

#' Subsets original dataset and calculates validity metrics
#'
#' @param x input from wrapper
#' @inheritParams valid
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return a tibble with validity criteria

subset_fun <- function(x) {

    names <- names(x)
    data <- tibble::as_tibble(.data$df)

    # Create subset
    for (name in names) {
        val <- x[[name]]

        if (is.factor(val)) {
            data <- data %>% dplyr::filter(!!rlang::sym(name) == val)

        } else if (is.numeric(val)) {
            data <- data %>% dplyr::filter(!!rlang::sym(name) <= val)

        }
    }

    # Select variables
    data <- data %>% dplyr::select(.data$target1, .data$target2) %>% tibble::as_tibble()

    # Calculate metrics
    if (!is.null(.data$target2)) {
        n <- nrow(data)
        esize <- effsize::cohen.d(data[[.data$target1]], data[[.data$target2]], na.rm = T)$estimate
        pwr <- pwr::pwr.t.test(n = n, d = esize, type = "paired")$power
    } else {
        n1 <- table(data[[.data$groupvar]])[1]
        n2 <- table(data[[.data$groupvar]])[2]
        esize <- effsize::cohen.d(data[[.data$target1]], data[[.data$groupvar]])
        pwr <- pwr::pwr.t2n.test(n = n1, n2 = n2, d = esize)$power
    }

    # Store metrics
    results <- tibble::tibble(`Sample Size` = n, Power = pwr, `Effect Size` = esize)

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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' library(filtR)
#' df <- data.frame(a = c(1:200), b = c(201:400), c = factor(rep(1:2,100)), d = c(201:400))
#' target1 <- "a"
#' target2 <- "b"
#' valid(target1 = target1, target2 = target2, df = df)

valid <- function(target1 = target1, target2 = NULL, groupvar = NULL, df = df) {

    # Split predictor variables
    predictors <- df %>% dplyr::select(-.data$target1, -.data$target2)

    # Generate all possible predictor-value combinations
    comb <- predictors %>% expand.grid() %>% unique()

    # For every predictor-value combination subset dataframe and append as list
    output <- comb %>% dplyr::rowwise() %>% dplyr::do(row = tibble::as_tibble(.data$.)) %>% tibble::as_tibble %>% dplyr::mutate(subset = purrr::map(row, subset_fun))

    # #Map over list and generate validity criteria
    results <- output %>% tidyr::unnest(subset)

}
