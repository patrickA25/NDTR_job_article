
library(tidyverse)
library(rvest)
library(xml2)

 
 page_result_start <- 1 # starting page 
 page_result_end <- 310 # last page results
 page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
 
 
 

 full_df <- NULL 
 for(i in seq_along(page_results)) {
   print(i)
   first_page_url <-  "https://www.indeed.com/jobs?q=Nutrition+and+Dietetics+Technician&l="
   url <- paste0(first_page_url, "&start=", page_results[i])
   page <- xml2::read_html(url)
   # Sys.sleep pauses R for two seconds before it resumes
   # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
   Sys.sleep(2)
   
   #get the job title
   job_title <- page %>% 
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//*[@data-tn-element = "job Title"]') %>%
      rvest::html_attr("title")
   
   #get the company name
   company_name <- page %>% 
     rvest::html_nodes("span")  %>% 
     rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
     rvest::html_text() %>%
     stringi::stri_trim_both() -> company.name 
   
   
   #get job location
   job_location <- page %>% 
     rvest::html_nodes("span") %>% 
     rvest::html_nodes(xpath = '//*[@class="location "]')%>% 
     rvest::html_text() %>%
     stringi::stri_trim_both()
   
   # get links
   links <- page %>% 
     rvest::html_nodes("div") %>%
     rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
     rvest::html_attr("href")
   
   job_description <- c()
   for(i in seq_along(links)) {
     
     url <- paste0 ("https://ca.indeed.com/", links[i])
     page <- xml2::read_html(url)
     
     job_description[[i]] <- page %>%
       rvest::html_nodes("span")  %>% 
       rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description  icl-u-xs-mt--md  "]') %>% 
       rvest::html_text() %>%
       stringi::stri_trim_both()
   }
   
   df <- data.frame(job_title, company_name, job_location, job_description)
   full_df <- rbind(full_df, df)
 }



 
 