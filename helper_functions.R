# Common helper functions
mem_used <- function() lobstr::mem_used() %>% as.numeric() %>% R.utils::hsize()
obj_size <- function(x) object.size(x) %>% print(units = "auto")

## Setting and modifying default theme for plots
theme_set(theme_gray(base_size = 12, base_family = "Arial") +
            theme(
              panel.border = element_rect(colour="black", fill = "transparent"), 
              plot.title = element_text(face="bold", hjust = 0),
              axis.text = element_text(color="black", size = 14), 
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_blank(),
              strip.background = element_blank(), # facet label borders
              legend.key=element_blank(), legend.background=element_blank() # remove grey bg from legend
            ))

## Copy previous report (this needs more generalization)
if (list.files() %>% 
    str_subset(paste0(out_file_prefix %>% str_remove("_$"), ".html")) %>% 
    length > 0) { # checks if result is not empty
  if (list.files() %>% # 2nd if only run when vector not empty
      str_subset(paste0(out_file_prefix %>% str_remove("_$"), ".html")) %>% 
      file.exists()) {
    dir.create("./previous_reports")
    prev_report <- list.files() %>% 
      str_subset(paste0(out_file_prefix %>% str_remove("_$"), ".html"))
    mtime <- list.files() %>% 
      str_subset(paste0(out_file_prefix %>% str_remove("_$"), ".html")) %>% 
      file.mtime() %>% 
      stringr::str_replace("\\s", "_") %>% stringr::str_replace_all(":", "-")
    file.copy(prev_report, paste(here(), "/previous_reports/",  mtime, "_", prev_report, sep = ""), copy.date = T)
  }
}

## Density color function (modified for CyTOF)
getDenCols <- function(x, y, transform = "log2", cofactor = 1) { # set to log2/asinh; cofactor should always be 1 for log2
  if(transform == "log2") cofactor = 1 # ensure cofactor = 1 for log2
  if(transform != FALSE) {
    df <- data.frame(transform(x/cofactor), transform(y/cofactor))
  } else{
    df <- data.frame(x, y)
  }
  z <- grDevices::densCols(df, bandwidth = c(5,5), colramp = grDevices::colorRampPalette(c("black", "white"))) # modified bandwidth
  df$dens <- grDevices::col2rgb(z)[1,] + 1L
  cols <-  grDevices::colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]
  return(df$dens)
} # End of function

## Excel export function
export_excel <- function(named_list, filename = "") {
  wb <- openxlsx::createWorkbook()
  ## Loop through the list of split tables as well as their names
  ## and add each one as a sheet to the workbook
  Map(function(data, name){
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, data)
  }, named_list, names(named_list))
  ## Save workbook to working directory
  openxlsx::saveWorkbook(wb, file = here("results", paste0(out_file_prefix, filename, ".xlsx")), overwrite = TRUE)
  cat("Saved as:", here("results", paste0(out_file_prefix, filename, ".xlsx")))
}

# labelled Volcano plot function -----
volcano_plot_lab <- function(res, title = "",
                             n_labels = 5,
                                subtitle = "down in Pos.                                                  up in Pos.",
                                y_lim = c(0, NA)){
  theme_set(theme_gray(base_size=12, base_family="Arial") +
              theme(panel.border=element_rect(colour="black", fill="transparent"), 
                    plot.title=element_text(face="bold", hjust=0), # lineheight=.8, size=20,
                    axis.text=element_text(color="black", size=14), 
                    axis.text.x=element_text(angle=0, hjust=0.5),
                    panel.background=element_blank(),
                    panel.grid=element_blank(),
                    plot.background=element_blank(),
                    strip.background = element_blank(), # facet label borders
                    legend.key=element_blank(), legend.background=element_blank() # remove grey bg from legend
              )
  )
  res <- res %>% 
    mutate(
      color = if_else(BHadj_pval < 0.1, "padj < 0.1", "All")
    )
  # get max for x-axis
  x_lim <- res %>% 
    summarize(max = max(log2(FoldChange), na.rm = TRUE), min = min(log2(FoldChange), na.rm = TRUE)) %>% 
    abs() %>% 
    max() %>% 
    ceiling()
  res %>% 
    ggplot(aes(log2(FoldChange), -log10(BHadj_pval), color = color)) + 
    geom_hline(yintercept = -log10(0.1), linetype = 2) + 
    geom_vline(xintercept = 0, linetype = 2) + 
    geom_point() + 
    scale_color_manual(values = c("padj < 0.1" = "red", "All" = "black")) + 
    xlim(-x_lim, x_lim) +
    ylim(y_lim) +
    geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_max(order_by = FoldChange, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = 1, nudge_y = 0.1) +
    geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_min(order_by = FoldChange, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = -1, nudge_y = 0.1) +
    # geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_min(order_by = BHadj_pval, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = -0, nudge_y = 0.1) +
    theme(aspect.ratio=1.2) +
    labs(
      title = title,
      subtitle = subtitle
    )
} # end of function



# Modified functions from CATALYST ----
# Make modified version to fix some aspects if needed (eg rect_gp = gpar(col = "white")) (some non-exported functions need prepending with CATALYST:::)
plotExprHeatmap2 <- function (x, features = NULL, by = c("sample_id", "cluster_id", 
                                                         "both"), k = "meta20", m = NULL, assay = "exprs", fun = c("median", 
                                                                                                                   "mean", "sum"), scale = c("first", "last", "never"), q = 0.01, 
                              row_anno = TRUE, col_anno = TRUE, row_clust = TRUE, col_clust = TRUE, 
                              row_dend = TRUE, col_dend = TRUE, bars = FALSE, perc = FALSE, 
                              bin_anno = FALSE, hm_pal = rev(brewer.pal(11, "RdYlBu")), 
                              k_pal = CATALYST:::.cluster_cols, m_pal = k_pal, distance = c("euclidean", 
                                                                                            "maximum", "manhattan", "canberra", "binary", "minkowski"), 
                              linkage = c("average", "ward.D", "single", "complete", "mcquitty", 
                                          "median", "centroid", "ward.D2")) 
{
  args <- as.list(environment())
  CATALYST:::.check_args_plotExprHeatmap(args)
  distance <- match.arg(distance)
  linkage <- match.arg(linkage)
  scale <- match.arg(scale)
  fun <- match.arg(fun)
  by <- match.arg(by)
  x <- x[unique(CATALYST:::.get_features(x, features)), ]
  if (by != "sample_id") {
    CATALYST:::.check_k(x, k)
    x$cluster_id <- cluster_ids(x, k)
  }
  if (by == "both") 
    by <- c("cluster_id", "sample_id")
  .do_agg <- function() {
    z <- CATALYST:::.agg(x, by, fun, assay)
    if (length(by) == 1) 
      return(z)
    set_rownames(do.call("rbind", z), levels(x$cluster_id))
  }
  .do_scale <- function() {
    if (scale == "first") {
      z <- assay(x, assay)
      z <- CATALYST:::.scale_exprs(z, 1, q)
      assay(x, assay, FALSE) <- z
      return(x)
    }
    else CATALYST:::.scale_exprs(z, 1, q)
  }
  z <- switch(scale, first = {
    x <- .do_scale()
    .do_agg()
  }, last = {
    z <- .do_agg()
    .do_scale()
  }, never = {
    .do_agg()
  })
  if (length(by) == 1) 
    z <- t(z)
  if (scale != "never" && !(assay == "counts" && fun == "sum")) {
    qs <- round(quantile(z, c(0.01, 0.99)) * 5)/5
    lgd_aes <- list(at = seq(qs[1], qs[2], 0.2))
  }
  else lgd_aes <- list()
  lgd_aes$title_gp <- gpar(fontsize = 10, fontface = "bold", 
                           lineheight = 0.8)
  if (!isFALSE(row_anno)) {
    # left_anno <- switch(by[1], sample_id = CATALYST:::.anno_factors(x, 
    #                                                      levels(x$sample_id), row_anno, "row"), .anno_clusters(x, 
    #                                                                                                            k, m, k_pal, m_pal))
    left_anno <- switch(by[1], sample_id = anno_factors2(x, # MG fix
                                                         levels(x$sample_id), row_anno, "row"), anno_clusters2(x, 
                                                                                                               k, m, k_pal, m_pal))
  }
  else left_anno <- NULL
  if (!isFALSE(col_anno) && length(by) == 2) {
    top_anno <- CATALYST:::.anno_factors(x, levels(x$sample_id), col_anno, 
                                         "colum")
    top_anno <- CATALYST:::.anno_factors(x, levels(x$sample_id), col_anno, 
                                         "colum")
  }
  else top_anno <- NULL
  if (bars) {
    right_anno <- CATALYST:::.anno_counts(x[[by[1]]], perc)
  }
  else right_anno <- NULL
  if (bin_anno) {
    cell_fun <- function(j, i, x, y, ...) grid.text(gp = gpar(fontsize = 8), 
                                                    sprintf("%.2f", z[i, j]), x, y)
  }
  else cell_fun <- NULL
  a <- ifelse(assay == "exprs", "expression", assay)
  f <- switch(fun, median = "med", fun)
  hm_title <- switch(scale, first = sprintf("%s %s\n%s", fun, 
                                            "scaled", a), last = sprintf("%s %s\n%s", "scaled", fun, 
                                                                         a), never = paste(fun, a, sep = "\n"))
  if (length(by) == 2) {
    col_title <- features
  }
  else if (length(features) == 1 && features %in% c("type", 
                                                    "state")) {
    col_title <- paste0(features, "_markers")
  }
  else col_title <- ""
  Heatmap(matrix = z, name = hm_title, col = colorRamp2(seq(min(z), 
                                                            max(z), l = n <- 100), colorRampPalette(hm_pal)(n)), 
          column_title = col_title, column_title_side = ifelse(length(by) == 
                                                                 2, "top", "bottom"), cell_fun = cell_fun, cluster_rows = row_clust, 
          cluster_columns = col_clust, show_row_dend = row_dend, 
          show_column_dend = col_dend, clustering_distance_rows = distance, 
          clustering_method_rows = linkage, clustering_distance_columns = distance, 
          clustering_method_columns = linkage, show_row_names = (is.null(left_anno) || 
                                                                   isTRUE(by == "sample_id")) && !perc, row_names_side = ifelse(by[1] == 
                                                                                                                                  "cluster_id" || isFALSE(row_anno) && !row_dend || 
                                                                                                                                  isFALSE(row_clust), "left", "right"), top_annotation = top_anno, 
          left_annotation = left_anno, 
          right_annotation = right_anno, 
          # rect_gp = gpar(col = "white"), MG: fix white borders
          heatmap_legend_param = lgd_aes)
}
# fix white borders
anno_factors2 <- function (x, ids, which, type = c("row", "column")) 
{
  type <- match.arg(type)
  cd <- colData(x)
  df <- data.frame(cd, check.names = FALSE)
  df <- select_if(df, ~!is.numeric(.))
  df <- mutate_all(df, ~droplevels(factor(.x)))
  m <- match(ids, df$sample_id)
  ns <- split(df, df$sample_id) %>% lapply(mutate_all, droplevels) %>% 
    lapply(summarize_all, nlevels) %>% do.call(what = "rbind")
  keep <- names(which(colMeans(ns) == 1))
  keep <- setdiff(keep, c("sample_id", "cluster_id"))
  if (is.character(which)) 
    keep <- intersect(keep, which)
  if (length(keep) == 0) 
    return(NULL)
  df <- df[m, keep, drop = FALSE]
  lvls <- lapply(as.list(df), levels)
  nlvls <- vapply(lvls, length, numeric(1))
  pal <- brewer.pal(8, "Set3")[-2]
  if (any(nlvls > length(pal))) 
    pal <- colorRampPalette(pal)(max(nlvls))
  names(is) <- is <- colnames(df)
  cols <- lapply(is, function(i) {
    u <- pal[seq_len(nlvls[i])]
    names(u) <- lvls[[i]]
    u
  })
  HeatmapAnnotation(which = type, df = df, col = cols, gp = gpar(col = NA)) # MG fix white borders
}
anno_clusters2 <- function (x, k, m, k_pal, m_pal) 
{
  kids <- levels(x$cluster_id)
  nk <- length(kids)
  if (nk > length(k_pal)) 
    k_pal <- colorRampPalette(k_pal)(nk)
  k_pal <- k_pal[seq_len(nk)]
  names(k_pal) <- kids
  df <- data.frame(cluster_id = kids)
  col <- list(cluster_id = k_pal)
  if (!is.null(m)) {
    i <- match(kids, cluster_codes(x)[, k])
    mids <- droplevels(cluster_codes(x)[, m][i])
    nm <- nlevels(mids)
    if (nm > length(m_pal)) 
      m_pal <- colorRampPalette(m_pal)(nm)
    m_pal <- m_pal[seq_len(nm)]
    names(m_pal) <- levels(mids)
    df$merging_id <- mids
    col$merging_id <- m_pal
  }
  df <- mutate_all(df, function(u) factor(u, unique(u)))
  rowAnnotation(df = df, col = col, gp = gpar(col = NA)) # MG fix white borders
}
#




