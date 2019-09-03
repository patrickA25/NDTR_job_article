#install.packages()

library(knitr)
library(tidyverse)
library(rvest)
library(xml2)

#read in full data set
full_df <- read_csv("https://raw.githubusercontent.com/patrickA25/NDTR_job_article/master/Indeed_raw_data.csv")

#have looked at all job names and removed jobs that can not be filled by NDTR
#there are a total of 64 jobs names
job_list <- read_csv("https://raw.githubusercontent.com/patrickA25/NDTR_job_article/master/job_names.csv")
jobs_remove <- job_list %>% filter(`cant use` == 'X')

full_df$X1 <- NULL

#wanting to see if there are any doubles of jobs

dim(unique(full_df))

#there are  224 doubles, so they will be removed
full_df_2 <- unique(full_df)

full_df_2$job_location

#job description is slowing things down. being removed
full_df_2_small <- full_df_2

#cleaning up the data into city and state and remaining data

full_df_3 <-  full_df_2_small %>% separate(job_location,c('city','other data'), sep = ',')

#removing any extra white space in the new cols

full_df_3$`other data` <-  str_trim(full_df_3$`other data`,side = "both")

#there are some new NA values and will want to look at them
full_df_3[is.na(full_df_3$`other data`),]
#the missing data can be filled in after the next step

full_df_4 <- full_df_3 %>% separate(`other data`,c("state","other"),extra = "merge", sep = "\\s")

#checking for NA in the state col
full_df_4[is.na(full_df_4$state),]

#for Nutrition Care Specialist (68M) no city or state is listed can not be included
#for Dietary Technician           Kaleida Health no city is listed can only fill in state data NY
full_df_4$state[59] <- 'NY'

#compnay is Scotland county NC, no city, but state is NC
full_df_4$state[305] <- 'NC'
full_df_4$city[305] <- NA

#sperating data to city and state is completed.
#wanting to convert factors into strings

str(full_df_4)
full_df_4$job_title <-  as.character(full_df_4$job_title)
full_df_4$company_name <- as.character(full_df_4$company_name)

full_df_5 <- full_df_4[,1:4]

#removing jobs that cant be used by NDTR
full_df_6 <- full_df_5 %>% filter(!(job_title %in% jobs_remove$Vars))

state_count <- count(full_df_6,vars= state ) 
sort(state_count$n,decreasing = TRUE)