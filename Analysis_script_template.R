#' ---
#' title: "`r params$title`"
#' author: "&nbsp;"
#' date: "&nbsp;"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'   fontsize: 11pt
#'   mainfont: arial
#' params:
#'   version: 0.1 # Do not edit except for new version
#'   title: TITLE OF THIS ANALYSIS
#'   project: PROJECT   
#'   date: !r strftime(Sys.Date(), format = "%B %d %Y")
#'   author:
#'     - Matthew Galbraith # can list additional authors
#'   email:
#'     - matthew.galbraith@cuanschutz.edu # can list additional emails
#'   affiliation: Department of Pharmacology & Linda Crnic Institute for Down Syndrome, University of Colorado Anschutz Medical Campus
#'   display_code: false #true/false to turn on/off display of R code as default
#'   collapse_output: true #true/false to turn on/off collapse of output as default
#' ---

#+ include=FALSE
###########################################################################################
# This YAML header section is used when producing html report using rmarkdown::render() ^ #
# Edit as needed                                                                          #
# TO PRODUCE REPORT: rmarkdown::render("script_name")                                     #
###########################################################################################

#' ***
#' ## Project: `r params$project`  
#' Date:  `r params$date`  
#' Report version: `r params$version`   
#' Author(s): `r params$author`  
#' `r params$email`  
#' `r params$affiliation`  

#+ knitr_setup, include=FALSE
# knitr options used in report generation
knitr::opts_chunk$set(echo = params$display_code)
knitr::opts_chunk$set(collapse = params$collapse_output)
knitr::opts_chunk$set(tidy = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.height = 6)
knitr::opts_chunk$set(fig.width = 10)

#+ include=FALSE
######################################
# ^ Do not edit this section above ^ # 
######################################
#
##########################################################################################
# Edit and customize section below as needed (but leave headings in place for uniformity #
##########################################################################################
#'
#' ***  
#' ### Summary  
#' Analyzing differences in XXXX
#'   
#' **Data type(s):**  
#'   
#' A. Data type 1  
#'    (see XXX for details)  
#'    
#' B. Data type 2  
#'    (see XXX for details)  
#'     
#' **Workflow:** 
#'    
#' 1. Step 1  
#' 2. Step 2  
#' 3. Step 3  
#'   
#' **Comments:** 
#'    
#' eg this version does something new  
#'   
#' ***


#+ include=FALSE
############################################################
# Script original author: Matthew Galbraith                #
# version: 0.1  Date: 07_08_2022                           #
############################################################
# Change Log:
# v0.1
# Initial version (+ mention if based on another script)
#
#####################################
# To Do for next version(s):  
# 1) need to add some analysis
#####################################


#+ general_setup, include=FALSE, message=FALSE, warning=FALSE
# general setup -----
# Clear workspace
if (exists("params")) { rm(list=ls()[! grepl("params", ls())]) } else { rm(list=ls()) } # This excludes "params" from the list for removal
# Load required libraries
library("tidyverse") # data manipulation
library("readxl") # Used to read .xlsx files
library("openxlsx") # used for data export as Excel workbooks
library("tictoc") # timer
library("skimr") # data summaries
library("janitor") # data cleaning etc
library("ggrepel") # required for labelling points
library("ggforce") # required for zooming and sina
library("plotly") # required for interactive plots
library("patchwork") # required for assembling multiple plots
library("conflicted") # required for managing conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
library("here") # generates path to current project directory
# detach("package:here", unload=TRUE) # run this to reset here()
###############################################
### FILE LOCATIONS (Should all be in /data) ###
# Make sure these files exist!!
meta_data_file <- here("data", "metadata.txt") # comments/notes on this file?
#
data_type1_file <- here("data", "data_type_1.tsv.gz") # comments/notes on this file?
#
standard_colors <- c("Control" = "gray60", "T21" = "#009b4e")
#
out_file_prefix <- "Analysis_script_template_"
###############################
source(here("helper_functions.R")) # load helper functions etc (must be after defining out_file_prefix)


#' ***
#' **File locations and variables:**
#+ report_variables, echo=FALSE, collapse=TRUE
# this section lists file locations ect in html report - no need to run if not generating report
cat("Working directory:")
here()
cat("\nMeta data file:")
meta_data_file
#
cat("\nData type 1 file(s):")
data_type1_file
#
cat("\nPrefix for output files:")
out_file_prefix
#


#' ***
#' ### 1.1 Read in and inspect metadata  
#+ read_meta, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 1.1 Read in and inspect metadata  ------
# meta_data <- meta_data_file %>% 
#   read_tsv()
# #
# cat("Metadata summary:")
# meta_data %>%
#   skimr::skim()
#
# meta_data %>% distinct(Sampleid)
# meta_data %>% distinct(RecordID)
# meta_data %>% count(Source)
# meta_data %>% count(Event_name)
# meta_data %>% count(Sex)
# meta_data %>% count(Karyotype)
# meta_data %>% count(Sex, Karyotype)
# meta_data %>% count(Karyotype, Source)
# meta_data %>% 
#   group_by(Sex, Karyotype) %>% 
#   summarize(
#     n = n(),
#     median_Age = median(Age),
#     mean_Age = mean(Age),
#     sd = sd(Age),
#     range = paste0(min(Age), " - ", max(Age))
#   )
#


#' ***
#' ### 1.2 Read in and inspect data type 1  
#+ read_data1, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 1.2 Read in and inspect data type 1  ------
# data_type1 <- data_type1_file %>% 
#   read_tsv()
# #
# data_type1
# data_type1 %>%
#   skimr::skim()
# #


#' ***
#' ### 2 Check joining of metadata with data type 1  
#+ join_meta, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 2 Check joining of metadata with data type 1  ------


#' ***
#' ### 3 Check data distribution and outliers etc  
#+ check_data, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 3 Check data distribution and outliers etc  ------


#' ***
#' ### 4 Analysis  
#+ analysis, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 4 Analysis  ------


#' ***
#' ### 5 Assemble and export results  
#+ results, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 5 Assemble and export results  ------


#' ***
#' ### 6 Plot interesting features  
#+ plots, warning = FALSE, message = FALSE, collapse = FALSE
# 
# 6 Plot interesting features  ------



#+ save_workspace, include = FALSE
# ##################
# # Save workspace #
# ##################
save.image(file = here("rdata", paste0(out_file_prefix, ".RData")), compress = TRUE, safe = TRUE) # saves entire workspace (can be slow)
# ################
# # RESTART HERE #
# ################
# load(here("rdata", paste0(out_file_prefix, ".RData")))


#' 
#' ***
#' ### Session Info
#+ session_info, collapse=TRUE
# Report generated at:
date()

sessionInfo()