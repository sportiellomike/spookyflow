#' fsom.ridges
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fsom.ridges <- function(fsom, cluster.select = NULL) {
  dat <- reshape2::melt(fsom.dat(fsom)[, c(fsom$dims.used, "cluster")],
                        id.vars = "cluster", value.name = "expression",
                        variable.name = "marker")
  dat$reference <- "no"
  dat.ref <- dat
  dat.ref$cluster <- "reference"
  dat.ref$reference <- "yes"

  if(!is.null(cluster.select)) {
    dat_plot <- rbind(dat[grep(paste0("^", cluster.select, "$", collapse = "|"), dat$cluster), ], dat.ref)
  } else {
    dat_plot <- rbind(dat, dat.ref)
  }

  ggplot() +
    ggridges::geom_density_ridges(data = dat_plot, aes(x = expression, y = cluster,
                                                       color = reference, fill = reference), alpha = 0.3) +
    facet_wrap( ~ marker, scales = "free_x", nrow = 3) +
    ggridges::theme_ridges() +
    theme(axis.text = element_text(size = 7),
          strip.text = element_text(size = 7), legend.position = "none")
}
