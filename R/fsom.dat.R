#' fsom.dat
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fsom.dat <- function(fsom) {
  if("node|cluster" %in% colnames(fsom$data)){
    fsom$data[, -grep("node|cluster", colnames(fsom$data))]
  }
  dat <- data.frame(fsom$data,
                    node = as.factor(fsom$map$mapping[, 1]),
                    cluster = as.factor(fsom$metaclustering[fsom$map$mapping[, 1]])
  )
  names(dat)[1:length(fsom$markers)] <- fsom$markers
  dat
}
