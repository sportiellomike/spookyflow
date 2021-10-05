#' fsom.cluster.medians
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fsom.cluster.medians <- function(fsom, dat.type = c("map", "data", "codes"), ranged = TRUE) {

  if(dat.type == "map") {
    if(ranged){
      tmp <- range.scaled(fsom$map$medianValues)
    } else {
      tmp <- fsom$map$medianValues
    }
    colnames(tmp) <- fsom$markers
    tmp <- data.frame(tmp, cluster = fsom$metaclustering)
    tmp
  } else if(dat.type == "data") {
    if(ranged) {
      tmp <- range.scaled(fsom$data)
    } else {
      tmp <- fsom$data
    }
    colnames(tmp) <- fsom$markers
    tmp <- data.frame(tmp, cluster = fsom$metaclustering[fsom$map$mapping[, 1]])
    tmp
  } else if(dat.type == "codes") {
    if(ranged) {
      tmp <- range.scaled(fsom$map$codes)
    } else {
      tmp <- fsom$map$codes
    }
    #colnames(tmp) <- fsom$markers
    tmp <- data.frame(tmp, cluster = fsom$metaclustering)
    tmp
  }

  tmp <- tmp %>% group_by(cluster) %>% summarize_all(list(median))
  tmp
}
