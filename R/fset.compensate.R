#' fset.compensate
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fset.compensate <- function(flowSet.samples){fsApply(flowSet.samples, function(frame) {
  ## compensate each 'frame'(sample) using stored compensation matrix (FCS header; keyword = $SPILL or SPILLOVER)
  print(paste0("compensating ", frame@description$`$FIL`))
  comp <- keyword(frame)[grep("SPILL", names(keyword(frame)), ignore.case = T)][[1]]
  frame_comped <- compensate(frame, comp)
  frame_comped
})
}
