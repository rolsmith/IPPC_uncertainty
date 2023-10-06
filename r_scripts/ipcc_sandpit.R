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
if (!require('tm')) install.packages('tm'); library(tm)

install.packages("rJava")
library(rJava) # load and attach 'rJava' now
install.packages("devtools")
devtools::install_github("ropensci/tabulizer")

library(tabulizer)

library(pdftools)

##%##########################################################################%##

download.file("https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter01.pdf",
              "WGI_Chapter01",
              mode="wb")

text <- pdf_text("WGI_Chapter01")

p.23 <- (text[23])

text.23 <- strsplit(text, "\n")

text.23.df <- as.data.frame(text.23)

##%##########################################################################%##




read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource("/Users/rolsmith/Library/CloudStorage/OneDrive-UniversityofEastAnglia/IPCC_uncertainty/IPCC_uncertainty_data/IPPC_uncertainty/ipcc_pdf/ipcc_pdf_wg1/IPCC_AR6_WGI_Chapter01.pdf"), readerControl = list(reader = read))
doc <- content(document[[1]])

text <- doc

str(text)

text[[23]]

text.df <- as.data.frame(text[[23]])

view(text.df)

colnames(text.df) <- "origin.line"

text.df.split <- text.df %>%
  separate(origin.line, into = c("l", "r"), sep = "\n")

view(text.df.split)

##%##########################################################################%##

#### holding df
chapter.df <- as.data.frame(matrix(ncol=5))
colnames(chapter.df) <- c("chapter.n",
                          "page.n",
                          "line.n",
                          "origin.line",
                          "uncertainty")

#### chapter url
url <- "https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter01.pdf"

#### number of pages and page sequence
(p <- get_n_pages(url))
pseq <- 1:p

#### extracting function
for (i in 1:length(pseq)) {
  
  txt <- tabulizer::extract_text(url, pages = pseq[i])
  
  #### split by line breaks
  page.text <- strsplit(txt,"\n")
  
  #### transform to df
  page.df<- as.data.frame(page.text)
  
  #### add first column name
  colnames(page.df) <- "origin.line"
  
  #### adding columns for line, page and uncertainty (logical)
  page.df <- page.df %>%
    mutate(across(where(is.character), tolower)) %>%
    mutate(page.n = origin.line[1],
           chapter.n = origin.line[3],
           uncertainty = ifelse(grepl("Uncertainty", origin.line), TRUE, FALSE),
           line.n = row_number()) %>%
    dplyr::select(chapter.n,
                  page.n,
                  line.n,
                  origin.line,
                  uncertainty)
  
  #### adding page.df to chapter.df
  chapter.df <- rbind(chapter.df,
                      page.df)
  
  print(paste("page",
              pseq[i],
              "of chapter",
              page.df$origin.line[3],
              "complete",
              sep = " "))  
}

view(chapter.df)
