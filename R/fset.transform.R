#' fset.transform
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fset.transform <- function(compensated.set, elgcl) {
  ## transform fcs data using ^ estimated logicile
  fsApply(compensated.set, function(frame) {
    print(paste0("transforming ", frame@description$`$FIL`))
    frame_trans <- transform(frame, elgcl)
    frame_trans
  })
}
