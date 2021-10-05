#' nodes.to.meta_fSOM
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

nodes.to.meta_fSOM <- function(fSOM.object, nodes) {
  levels(fSOM.object$metaclustering) <- c(levels(fSOM.object$metaclustering), length(levels(fSOM.object$metaclustering))+1)
  fSOM.object$metaclustering[nodes] <- length(levels(fSOM.object$metaclustering))
  fSOM.object$metaclustering <- factor(fSOM.object$metaclustering)
  levels(fSOM.object$metaclustering) <- c(1:length(levels(fSOM.object$metaclustering)))
  return(fSOM.object)
}
