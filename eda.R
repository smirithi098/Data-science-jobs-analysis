# Import packages
library(tidyverse)
library(ggplot2)


# Load the dataset
ds_data = read.csv('S:\\R practice\\Portfolio project\\Data science jobs analysis\\Data\\Uncleaned_DS_jobs.csv', 
                   header=TRUE)

# Data cleaning and transformation

# 1. Remove unnecessary columns
ds_data <- subset(ds_data, select = -c(index, Competitors))

# 2. Rename column names
colnames(ds_data) <- c('job_title', 'salary', 'job_description', 'rating', 'company_name',
                       'location', 'HQ', 'size', 'founded', 'ownership', 'industry',
                       'sector', 'revenue')

# Remove all non-numeric characters from salary column
ds_data$salary <- str_split(ds_data$salary, " ")[[1]][1]
ds_data$salary <- str_replace_all(ds_data$salary, c("\\$" = "", "K" = "000"))

# Create new columns based on salary column
ds_data <- ds_data %>% 
  mutate(min_salary = as.numeric(str_split(salary, '-')[[1]][1]),
         max_salary = as.numeric(str_split(salary, '-')[[1]][2])) %>% 
  mutate(avg_salary = (min_salary + max_salary)/2)

# Calculate the number of years each company has been in business

current_year = as.numeric(format(Sys.Date(), format="%Y"))

ds_data <- ds_data %>% 
  mutate(years = if_else(founded == -1, founded, current_year - founded))

# Remove characters from ownership column

ds_data <- ds_data %>% 
  mutate(ownership = if_else(grepl('-', ownership), 
                             str_remove(ownership, pattern = "^.*-"), 
                             ownership))

ds_data$ownership <- str_trim(ds_data$ownership)

# remove the word 'employees' from size column

ds_data <- ds_data %>% 
  mutate(size = if_else(size == -1, 
                        size, 
                        word(size , 1  , -2)))

ds_data$size <- str_trim(ds_data$size)

# remove the numbers from the `company name` column

ds_data$company_name <- gsub("(\n\\d+\\.?\\d*)", "", ds_data$company_name)

# find the unique job titles and frequency of their occurrence

job_titles <- ds_data %>% 
  count(job_title, sort = TRUE)



# Analysis questions

