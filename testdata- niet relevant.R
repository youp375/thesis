library(readr)
library(tm)
library(dplyr)
library(stringr)
library(tibble)
library(magrittr)
library(purrr)
library(tidyr)
library(tidytext)
library(pdftools)
library(tidyverse)

setwd("~/Documents/BAM/Thesis/Data")

startupstest <-read_csv("testcompanies.csv")

colnames(startupstest) <- c('name', 'cbprofile', 'totalfunding', 'totalfundingcurrency', 'totalfundingusd', 'ipostatus', 'lastfundingtype', 'employees', 'industries', 'headquarters', 'description',
                            'transactionname', 'tnamelink', 'founders', 'website', 'companytype', 'email', 'linkedin', 'fulldescription', 'revenue', 'founded', 'precision',  'lastfunding', 'lastfundingcurrency','lastfundingusd',  'industrygroup', 'status',
                            'lastfundingdate')


startupstest$totalfunding <- ifelse(is.na(startupstest$totalfunding), 0, startupstest$totalfunding)
startupstest$totalfundingcurrency <- ifelse(is.na(startupstest$totalfundingcurrency), 'None', startupstest$totalfundingcurrency)
startupstest$lastfundingtype <- ifelse(is.na(startupstest$lastfundingtype), 'None', startupstest$lastfundingtype)
startupstest$totalfundingusd <- ifelse(is.na(startupstest$totalfundingusd), 0, startupstest$totalfundingusd)
startupstest$lastfunding <- ifelse(is.na(startupstest$lastfunding), 0, startupstest$lastfunding)
startupstest$lastfundingcurrency <- ifelse(is.na(startupstest$lastfundingcurrency), 'None', startupstest$lastfundingcurrency)
startupstest$lastfundingusd <- ifelse(is.na(startupstest$lastfundingusd), 0, startupstest$lastfundingusd)

startupstest <- startupstest  %>% 
  mutate(
    headquarters = sapply(strsplit(headquarters, ","), function(x) trimws(x[1]))
  )

#new var
startupstest %>% 
  count(lastfundingtype, sort=T)

success <- c("Series A", "Series B", "Venture - Series Unknown", "Private Equity", "Series C",
             "Series D", "Series E", "Series G", "Series H")

full_data <- full_data %>% 
  mutate(
    succeeded = ifelse(lastfundingtype %in% success, 1, 0),
    num_founders = str_count(founders, ",") + 1,
    contains_email = ifelse(email != "—", 1, 0),
    contains_description = ifelse(description != "—", 1, 0),
    yearfunding = str_sub(lastfundingdate, start= 1,4),
    yearfunding = as.factor(yearfunding)
  ) %>% 
  select(-X)

full_data <- full_data %>% 
  separate(industries, sep = ",", into = paste0("Industry", 1:5), extra = "drop") %>% 
  separate(industrygroup, sep = ",", into = paste0("Category", 1:5), extra = "drop")

startupsUK <- full_data %>% 
  group_by(headquarters) %>% 
  filter(n() >10) %>% 
  ungroup()