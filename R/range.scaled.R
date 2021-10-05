#' range.scaled
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

range.scaled <- function(dat, cols, probs = c(0.2, 0.99)) {
  tmp <- as.matrix(dat[, cols])
  ranges <- matrixStats::colQuantiles(tmp, probs = probs)
  if(ncol(tmp) == 1){
    tmp <- (tmp - ranges[1])/(ranges[2] - ranges[1])
  }else{
    tmp <- t((t(tmp) - ranges[, 1]) / (ranges[, 2] - ranges[, 1]))
  }
  tmp[tmp < 0] <- 0
  tmp[tmp > 1] <- 1
  dat[, cols] <- tmp
  dat
}
