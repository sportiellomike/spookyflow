#' elgcl.pars
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

elgcl.pars <- function(compensated.set) {
  dat <- fsApply(compensated.set, exprs)
  channels.fluor <- as.vector(grep("FSC|SSC|Time", colnames(dat), ignore.case = T, value = T, invert = T))
  dat <- dat[, channels.fluor]
  elgcl <- estimateLogicle(new("flowFrame", dat), channels = channels.fluor)
  elgcl
}
