#' markernames.flow
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

markernames.flow <- function(fcs) {
  if(class(fcs) == "flowFrame") {
    colnames(fcs)[which(!is.na(fcs@parameters@data$desc))] <- markernames(fcs)
  } else if(class(fcs) == "flowSet") {
    colnames(fcs)[which(!is.na(fcs[[1]]@parameters@data$desc))] <- markernames(fcs)
  }
  colnames(fcs) <- sub("-", ".", colnames(fcs))
  colnames(fcs) <- sub(" ", ".", colnames(fcs))
  #colnames(fcs) <- sapply(colnames(fcs), function(i) strsplit(i, " ")[[1]][1])
  fcs
}
