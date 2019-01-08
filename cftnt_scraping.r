setwd("C:\\Users\\pragan\\Dropbox\\web_scraping")

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

# Send Email
library(mailR) 

library(rmarkdown) 

# Eases DateTime manipulation
library(lubridate)

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
#gsub("0(\\d)", "\\1",format(floor_date(Sys.time(), unit="week"), format="%m%d%y"))
beginning_date <- as.Date(floor_date(Sys.time(), unit="week"))
end_date <- beginning_date + 6
date_sequence <- as.data.frame(seq(beginning_date, end_date, "days"))
colnames(date_sequence) <- c("date")

base_url <- "http://www.crossfit-tnt.com/blog/post/"

url_variations <- c("skill--wod-", "weight-vest-wednesday-", "rehab-saturday-", "sunday-funday-")

date_sequence$url <- "http://www.crossfit-tnt.com/errorPage.aspx?error=404"

date_sequence$workout <- NA

# Get correct url for each date
i <- 1
for (i in 1:nrow(date_sequence)) {
  j <- 1
  url <- paste0(base_url,url_variations[j],gsub("0(\\d)", "\\1",format(date_sequence[i,1], format="%m%d%y")))
  while (httr::HEAD(url)$url == "http://www.crossfit-tnt.com/errorPage.aspx?error=404" && j <= length(url_variations)) {
    url <- paste0(base_url,url_variations[j],gsub("0(\\d)", "\\1",format(date_sequence[i,1], format="%m%d%y")))
    j = j + 1
  }
  
  if(j > length(url_variations) && httr::HEAD(url)$url == "http://www.crossfit-tnt.com/errorPage.aspx?error=404") {
    url <- "No Valid Url Found"
  } else {
    date_sequence[i,]$workout <- scrape_cftnt(url)
    date_sequence[i,]$url <- url
    }
  
  i = i + 1
}

get_title <- function(string) {
  unlist(strsplit(string,"\n"))[1]
}

#date_sequence$title <- sapply(date_sequence$workout, get_title)
#date_sequence$title <- paste0("[",date_sequence$title,"](",date_sequence$url,")")
#date_sequence$title <- NULL
date_sequence <- subset(date_sequence,!(is.na(date_sequence["workout"])))

date_sequence$url <- trimws(date_sequence$url)
row.names(date_sequence) <- NULL
#output_table <- as.data.frame(cbind(date_sequence$date, date_sequence$url, date_sequence$workout))
#colnames(output_table) <- c("date","url',workout")
# output_table <- subset(output_table,!(is.na(output_table["workout"])))
#render("cftnt_workouts.rmd")

#body <- pander(date_sequence, style='grid', keep.line.breaks = TRUE)
t <- pander_return(date_sequence, style='grid', keep.line.breaks = TRUE)
send.mail(from = c("Paul Ragan <paul.ragan@gmail.com>"), 
          to = c("pragan@sccmo.org"), 
          subject = "CFTNT Workouts", 
          #body = paste(pander(date_sequence, style='grid', keep.line.breaks = TRUE),collapse='\n'),
          #body = paste(date_sequence,collapse='\n'),
          body = msg,
          smtp = list(host.name = "10.10.41.216"),
          html = TRUE,
          #inline = TRUE,
          authenticate = FALSE,
          #attach.files = c(all_county_file),
          send = TRUE)


library(sendmailR)
library(xtable)
from    <- '<pragan@sccmo.org>'
to      <- '<pragan@sccmo.org>'
subject <- paste0("CFTNT Workouts for the week of ",beginning_date)
msg <- mime_part(paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                       <html xmlns="http://www.w3.org/1999/xhtml">
                       <head>
                       <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                       <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                       </head>
                       <body><pre>',paste(pander_return(date_sequence, style='grid', keep.line.breaks = TRUE),collapse='\n'),'</pre></body>
</html>'))
msg[["headers"]][["Content-Type"]] <- "text/html"
sendmail(from, to, subject, list(msg), control=list(smtpServer="10.10.41.216"))
