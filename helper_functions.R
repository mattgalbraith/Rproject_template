############################################################
# Common helper functions                                  #
# Script original author: Matthew Galbraith                #
# version: 07_08_2022                                      #
############################################################

mem_used <- function() lobstr::mem_used() %>% as.numeric() %>% R.utils::hsize()
obj_size <- function(x) object.size(x) %>% print(units = "auto")

# Setting and modifying default theme for ggplot -----
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

# Backup previous html report  -------
# requires out_file_prefix to be set
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
} # End of function

# Density color function -----
# modified to allow for CyTOF transformation
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

# Excel export function -----
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
volcano_plot_lab <- function(
    res, 
    title = "",
    n_labels = 5,
    subtitle = NULL,
    y_lim = c(0, NA)
){
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
    # Edit as needed
    geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_max(order_by = FoldChange, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = 1, nudge_y = 0.1) +
    geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_min(order_by = FoldChange, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = -1, nudge_y = 0.1) +
    # geom_text_repel(data = res %>% filter(!is.na(BHadj_pval)) %>% slice_min(order_by = BHadj_pval, n = n_labels), aes(label = uniquePopulationName), min.segment.length = 0, show.legend = FALSE, nudge_x = -0, nudge_y = 0.1) +
    theme(aspect.ratio=1.2) +
    labs(
      title = title,
      subtitle = subtitle
    )
} # end of function




