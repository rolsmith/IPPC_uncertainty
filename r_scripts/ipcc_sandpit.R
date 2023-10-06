##%######################################################%##
#                                                          #
####         IPCC Uncertainty: Sandpit for data         ####
####         extraction ~ Roland Smith ~ 6.x.23         ####
#                                                          #
##%######################################################%##

# 0: Overview ##################################################################
## 0.1: Overview ###############################################################

#### Sandpit programme for extracting key data from IPCC report chapters
getwd()

## 0.2 Installing packages #####################################################
#### Basics 
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('readxl')) install.packages('readxl'); library(readxl)
if (!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if (!require('xlsx')) install.packages('xlsx'); library(xlsx)

#### Others
if (!require('pdftools')) install.packages('pdftools'); library(pdftools)
if (!require('tabulizer')) install.packages('tabulizer'); library(tabulizer)

install.packages("pak")
pak::pkg_install('ropensci/tabulizer')

##%##########################################################################%##

tab.text <- extract_text("ipcc_pdf/ipcc_pdf_wg1/IPCC_AR6_WGI_Chapter01.pdf")
tab.text2 <- strsplit(tab.text,"\n")``

(head(tab.text2))

get_text <- function(url) {
  # Get nunber of pages of PDF
  p <- get_n_pages(url)
  # Initialize a list
  L <- vector(mode = "list", length = 1)
  # Extract text from pdf
  txt <- tabulizer::extract_text(url, pages = seq(1,p))
  # Output: character vector containing all pages
  return(txt)
}

tab.text <- get_text(url = "https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter01.pdf")
tab.text2 <- strsplit(tab.text,"\n")

head(tab.text2)

# 1: Uploading data ############################################################

#### uploading text using pdf tools
text <- pdf_text("ipcc_pdf/ipcc_pdf_wg1/IPCC_AR6_WGI_Chapter01.pdf")

#### character vector of text with spaces for spaces, \n linebreaks
str(text) #### chr [1:142] "Chapters\n" "" ...

#### using strsplit to separate lines
text2 <- strsplit(text,"\n")

pages.n <- length(text2) #### List of 142

#### loop here 

for(i in 1:length(pages.n)) {
  
  #### create a dataframe for that page
  page.df <- as.data.frame(text2[[6]])
  
  #### adding 'origin.line' column name
  colnames(page.df) <- "origin.line"
  
  view(page.df)

p5.df <- p5.df %>%
  mutate(uncertainty = ifelse(grepl("Uncertainty", origin.text), TRUE,
                              ifelse(grepl("uncertainty", origin.text), TRUE, FALSE)))

view(p5.df)
         