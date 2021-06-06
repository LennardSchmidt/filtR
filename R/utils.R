
# Thu Nov 14 13:04:18 2019 ------------------------------
# Y-Axis Filter combinations
# X-Axis Filter vars

#' Plots validity metrics
#'
#' @param X an filtR object
#' @param MARGIN a metric
#' @param FUN a caption for each plot
#' @param ... TBD
#'
#' @return TBD


apply_pb <- function(X, MARGIN, FUN, ...) {
  env <- environment()
  pb_Total <- sum(dim(X)[MARGIN])
  counter <- 0
  pb <- utils::txtProgressBar(
    min = 0, max = pb_Total,
    style = 3
  )

  wrapper <- function(...) {
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1, envir = env)
    utils::setTxtProgressBar(
      get("pb", envir = env),
      curVal + 1
    )
    FUN(...)
  }
  res <- apply(X, MARGIN, wrapper, ...)
  close(pb)
  res
}


# Thu Nov 14 13:04:18 2019 ------------------------------
# Y-Axis Filter combinations
# X-Axis Filter vars

#' Plots validity metrics
#'
#' @param iterator an filtR object
#'
#' @return TBD

pbcombine <- function(iterator){
  env <- environment()
  pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    flush.console()
    rbind(...) # this can feed into .combine option of foreach
  }
}
