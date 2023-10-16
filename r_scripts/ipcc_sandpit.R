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
if (!require('stringr')) install.packages('stringr'); library(stringr)
if (!require('tm')) install.packages('tm'); library(tm)

##%##########################################################################%##

#### search term we are after
search.term <- "low confidence"

#### total number of chapters
number.chapters <- 12

#### set working group
working_group <- "WGI"

#### total chapters
(total.chapters <- seq(1:number.chapters))

#### setting holding df for report
report.df <- tibble()

for (j in 1:length(total.chapters)){

#### set chapter
chapter <- ifelse(j < 10, paste("0", j, sep=""), j)

#### set filename
(filename <- paste(working_group,
                  "_Chapter",
                  chapter,
                  sep=""))

#### set filepath
filepath <- paste("https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_",
                  filename,
                  ".pdf",
                  sep="")

#### download chapter file
download.file(filepath,
              filename,
              mode="wb")

#### extract chapter text
text <- pdf_text(filename) %>% strsplit(split = "\n")

#### create chapter holding file
chapter.df <- tibble()

#### loop function for each page
for(i in 1:length(text)) {
  
  chap.pages <- seq(1:length(text))
  
  (chapter_page <- chap.pages[i])
  
  #### extracting page text
  (pg.text <- as_tibble(text[[i]]))
  
  #### naming main column
  (colnames(pg.text) <- "raw_text")

  #### trim whitespaces
  (pg.text$raw_text <- trimws(pg.text$raw_text))
  
  #### changing to lower case for match
  (pg.text <- pg.text%>% mutate(raw_text = tolower(raw_text)))
  
  #### removing whitespaces between columns
  (pg.text$raw_text <- gsub("(\\s)\\s{2,}(?=\\s)", "_", pg.text$raw_text, perl=TRUE))
  
  #### splitting column into two text columns
  (pg.text[c("col1","col2")] <- str_split_fixed(pg.text$raw_text, "_", 2))
  
  #### extracting col1
  pg.text.col1 <- pg.text %>% select(col1, raw_text) %>%
    rename(final_text = col1) %>%
    mutate(col.index = "col1")
  
  #### extracting col2
  pg.text.col2 <- pg.text %>% select(col2, raw_text) %>%
    rename(final_text = col2) %>%
    mutate(col.index = "col2")
  
  #### binding col1 and col2
  pg.text.bind <- rbind(pg.text.col1,
                        pg.text.col2)
  
  #### formatting final dataframe
  pg.text.bind <- pg.text.bind %>%
    #### removing empty rows
    filter(final_text != "") %>%
    #### creating search term column with lead and lag for lines either side
    mutate(search.term = ifelse(str_detect(final_text, search.term), TRUE, FALSE)) %>%
    #### filtering only relevant text lines - line with lead and lag
    filter(lead(search.term == TRUE) | search.term == TRUE | lag(search.term == TRUE)) %>%
    #### adding column for line indicator
    mutate(line_reference = ifelse((search.term == TRUE), ">>>","|")) %>%
    #### adding column for working group reference
    mutate(WG = working_group,
           #### adding column for chapter reference
           chapter = chapter,
           #### adding column for report page reference
           report_page = (pg.text$raw_text[length(pg.text$raw_text)]),
           #### adding column for chapter page reference
           chapter_page = chapter_page,
           #### adding column for line number reference
           line = row_number()) %>%
    #### selecting final columns for table
    select(WG,
           chapter,
           report_page,
           chapter_page,
           line,
           col.index,
           line_reference,
           final_text,
           raw_text)
  
  #### bind to holding dataframe
  chapter.df <- rbind(chapter.df,
                      pg.text.bind)
  
  #### clear pg text for next loop
  rm(pg.text)
  
  #### message for each loop
  print(paste("Completed page",
              chapter_page,
              "out of",
              length(text),
              "for chapter",
              chapter,
              "of",
              working_group,
              sep=" "))
  }

report.df <- rbind(report.df,
                   chapter.df)

print(paste("Completed chapter",
            chapter,
            "of",
            working_group,
            sep=" "))

}  

view(report.df)

getwd()

write_csv(report.df,
          "ipcc_raw_outputs/WG1_low_confidence.csv")



##%##########################################################################%##

pg.text$page <- chap.pages[23]

pg.text$col <- gsub("(\\s)\\s{2,}(?=\\s)", "_", pg.text$col, perl=TRUE)

pg.text$col

pg.text[c("col1","col2")] <- str_split_fixed(pg.text$col, "_", 2)

view(pg.text)


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
