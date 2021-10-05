#' fsom.heatmap
#'
#' @description
#' @param
#' @return
#' @export
#'
#' @examples

fsom.heatmap <- function(fsom,
                         dat.type,
                         dims,
                         row_order = NULL,
                         clust.freq = T,
                         clust.count = F,
                         display_numbers = FALSE,
                         color.cut = 100,
                         color.option = "brewer", ...) {

  if (!color.option %in% c("brewer", "viridis")) {
    stop("Use 'color.option' = 'brewer' or 'viridis'")
  }

  cluster.medians <- as.data.frame(fsom.cluster.medians(fsom, dat.type = dat.type, ranged = TRUE))
  rownames(cluster.medians) <- rownames(cluster.medians) # some weird bug here, so have to 'rename' rownames

  ## Calculate cluster frequencies
  clustering_table <- as.numeric(table(fsom$metaclustering[fsom$map$mapping[, 1]]))
  clustering_prop <- round(clustering_table / sum(clustering_table) * 100, 2)

  ## Sort the cell clusters with hierarchical clustering
  col.index <- fsom$markers[fsom$map$colsUsed]
  cluster_rows <- hclust(dist(cluster.medians[, -grep("cluster", colnames(cluster.medians))][, col.index], method = "euclidean"), method = "average")

  # custom row ordering
  if(!is.null(row_order)){
    cluster.medians <- cluster.medians[row_order, ]
    clustering_table <- clustering_table[row_order]
    clustering_prop <- clustering_prop[row_order]
    cluster_rows <- FALSE
  }

  # Colors for the heatmap
  if(color.option == "brewer"){
    color_heat <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(color.cut)
  } else {
    color_heat <- viridis(color.cut)
  }

  # appened cluster frequencies and or total counts
  if(clust.freq&clust.count){
    labels_row <- paste0(cluster.medians$cluster, " (", clustering_prop, " %)", " (", clustering_table, ")")
  } else if(clust.freq == TRUE&clust.count == FALSE) {
    labels_row <- paste0(cluster.medians$cluster, " (", clustering_prop , " %)")
  } else if(clust.freq == FALSE&clust.count == TRUE) {
    labels_row <- paste0(cluster.medians$cluster, " (", clustering_table , ")")
  } else {
    labels_row <- cluster.medians$cluster
  }

  # Annotation for the original clusters
  annotation_row <- cluster.medians["cluster"]
  annotation_colors <- list(cluster = setNames(colorRampPalette(viridis(9))(nlevels(fsom$metaclustering)), levels(annotation_row$cluster)))

  legend_breaks = seq(from = 0, to = 1, by = 0.2)

  pheatmap(cluster.medians[, dims], color = plasma(50),
           cluster_rows = cluster_rows, labels_row = labels_row,
           display_numbers = display_numbers, number_color = "black",
           fontsize = 8, fontsize_number = 6,  legend_breaks = legend_breaks,
           annotation_row = annotation_row, annotation_colors = annotation_colors,
           ...)

}
