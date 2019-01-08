#setwd("//Users//paulragan//Dropbox//web_scraping")
rm(list=ls)

# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)

# Verbose regular expressions
library(rebus)     

# Needed to test 404s
library(httr)

# format table
library(pander)

# Eases DateTime manipulation
library(lubridate)

# Define Variables
output_file <- 'cftnt.html'
base_url <- "http://www.crossfit-tnt.com/blog/post/"
url_variations <- c("skill--wod-", "weight-vest-wednesday-", "rehab-saturday-", "sunday-funday-")

# Web scraping function
scrape_cftnt <- function(url) {
  wods <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[(@id = "ctl01_ComponentItem")]') %>%
    html_text() %>%
    str_trim
  wods2 <- wods[[1]]
  # Clean Up HTML
  # Convert All \r\n Newlines to \n and Remove Multiple Newlines
  wods2 <- str_replace_all(str_replace_all(wods2,'([\\r\\n])','\\\n'),'(\\n)\\s+','\\\n')
  # Remove "Posted On..." & "Email It" from Output
  wods2 <- str_replace(str_replace(wods2,'\nEmail It', ''),'\\nPosted .*', '')
  return(wods2)
}

# Get Start and End Dates
beginning_date <- as.Date(floor_date(Sys.time(), unit="week")) # Get date for Sunday of the current week
end_date <- beginning_date + 6 
cftnt_data <- as.data.frame(seq(beginning_date, end_date, "days")) # Populate a data frame with a row for each date in the week
colnames(cftnt_data) <- c("date")

cftnt_data$url <- "http://www.crossfit-tnt.com/errorPage.aspx?error=404"

cftnt_data$workout <- NA

# Format dates - remove leading zeros from month/day
# Result should match format mdy: 1119 for 2019-01-01, 1219 for 2019-01-02, etc.
cftnt_data$url_date_format <- paste0(gsub("0(\\d)", "\\1", format(as.Date(cftnt_data$date), format="%m")),
                              gsub("0(\\d)", "\\1", format(as.Date(cftnt_data$date), format="%d")),
                              gsub("0(\\d)", "\\1", format(as.Date(cftnt_data$date), format="%y")))

# Scrape workout data
i <- 1
for (i in 1:nrow(cftnt_data)) {
  # Get correct url for each date - test each url, and repeat until a non-404 page is returned
  j <- 1
  url <- paste0(base_url,url_variations[j],cftnt_data[i,4])
    while (httr::HEAD(url)$url == "http://www.crossfit-tnt.com/errorPage.aspx?error=404" && j <= length(url_variations)) {
      url <- paste0(base_url,url_variations[j],cftnt_data[i,4])
      j = j + 1
    }
  
  # If a valid url was returned, then scrape the workout text
  if(j > length(url_variations) && httr::HEAD(url)$url == "http://www.crossfit-tnt.com/errorPage.aspx?error=404") {
    url <- "No Valid Url Found"
  } else {
    cftnt_data[i,]$workout <- scrape_cftnt(url)
    cftnt_data[i,]$url <- url
    }
  
  i = i + 1
}

# Separate post title from workout
get_title <- function(string) {unlist(strsplit(string,"\n"))[1]}
cftnt_data$title <- sapply(cftnt_data$workout, get_title)

# Clean up data
cftnt_data <- subset(cftnt_data,!(is.na(cftnt_data["workout"])))
cftnt_data$url <- trimws(cftnt_data$url)
row.names(cftnt_data) <- NULL

# Create HTML
cftnt_data$workout2 <- str_replace_all(cftnt_data$workout,"\n","</p>")
html <- paste0('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><html xmlns="http://www.w3.org/1999/xhtml"><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><meta name="viewport" content="width=device-width, initial-scale=1.0"/></head><body><pre>',
                        paste0('<h2><a href="',trimws(cftnt_data[1,]$url),'\">',trimws(cftnt_data[1,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[1,]$workout2),"</p>",collapse=''),
                        paste0('<h2><a href="',trimws(cftnt_data[2,]$url),'\">',trimws(cftnt_data[2,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[2,]$workout2),"</p>",collapse=''),
                        paste0('<h2><a href="',trimws(cftnt_data[3,]$url),'\">',trimws(cftnt_data[3,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[3,]$workout2),"</p>",collapse=''),
                        paste0('<h2><a href="',trimws(cftnt_data[4,]$url),'\">',trimws(cftnt_data[4,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[4,]$workout2),"</p>",collapse=''), 
                        paste0('<h2><a href="',trimws(cftnt_data[5,]$url),'\">',trimws(cftnt_data[5,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[5,]$workout2),"</p>",collapse=''),
                        paste0('<h2><a href="',trimws(cftnt_data[6,]$url),'\">',trimws(cftnt_data[6,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[6,]$workout2),"</p>",collapse=''),
                        paste0('<h2><a href="',trimws(cftnt_data[7,]$url),'\">',trimws(cftnt_data[7,]$title),"</a></h2>",
                          "<p>",trimws(cftnt_data[7,]$workout2),"</p>",collapse=''),
                        '</pre></body></html>')

# Export HTML
fileConn <- file(output_file)
writeLines(html,fileConn)
close(fileConn)