#MERGING DATASETS

#RELEVANT LIBRARIES
library(stargazer)
library(Hmisc)
library(tidyverse)
library(reshape)
library(stargazer)
library(car)
library(lm.beta)
library(AER)
library(plyr)
library(reshape2)
library(xtable)
library(plm)
library(wbstats)
library(pscl)
library(MASS)
library(sandwich)  
library(extraDistr)

#MERGE THE DATAFRAMES INTO ONE
merged_dataset <- merge(panel_startups_final, panel_patents_city, by = c("city", "year"))
merged_dataset <- merge(merged_dataset, panel_patents_industry, by = c("industry", 'year'), all.x = TRUE)

#CLEAN THE DATA > AT LEAST 10K IN FUNDING
merged_dataset_clean <- merged_dataset %>% 
  filter(
    totalfunding > 10000,
  ) %>% 
  arrange(name, year)


#MUTATE LOGARITHMS
merged_dataset_clean <- merged_dataset_clean %>% 
  mutate(
    logtotalfunding = log((totalfunding+1)),
    logseed = log((seed_amount+1)),
    logseriesa = log((seriesa_amount+1))
  )

#CREATE FINAL ANALYSIS DATASET
analysis_data <- merged_dataset_clean %>% 
  select('year', 'name', 'city', 'description', 'industry', 'logtotalfunding', 'ipostatus',
         'employees', 'description', 'founders',
         'companytype', 'status', 'num_founders', 'seedfunding', 'seriesafunding',
         'logseed', 'logseriesa', 'n_patents','cum_patents_city', 'n_patents_ind', 'cum_patents_industry'
  )

analysis_data <- analysis_data %>% 
  dplyr::select('name', 'year', 'city', 'description', 'industry', 'logtotalfunding', 'ipostatus',
         'employees', 'description', 'founders',
         'companytype', 'status', 'num_founders', 'seedfunding', 'seriesafunding',
         'logseed', 'logseriesa', 'n_patents','cum_patents_city', 'n_patents_ind', 'cum_patents_industry'
  )

write.csv(analysis_data, 'data/analysisdata.csv')

###EXPLORATORY ANALYSIS:

analysis_data <- read.csv('data/analysisdata.csv')

#CREATE EXPLORATORY STATISTICS
stargazer(analysis_data)

#MAKE SURE IT'S THE RIGHT DATA:
analysis_data %>% 
  filter(logseed != 0) %>% 
  summarise(mean(logseed),
            sd(logseed))

analysis_data %>% 
  filter(logseriesa != 0) %>% 
  summarise(mean(logseriesa),
            sd(logseriesa))            

#INDUSTRY CATEGORY:
industrydata <- analysis_data %>% 
  group_by(name) %>% 
  summarise(industry) %>% 
  distinct()

table(industrydata$industry)

#FINAL STARTUPS
analysis_data %>% 
  group_by(name) %>% 
  distinct(name)

analysis_data %>% 
  group_by(seriesafunding) %>%
  summarise(sum(seriesafunding))

merged_dataset_clean %>% 
  filter(seed_amount != 0) %>% 
  summarise(mean(seed_amount),
            sd(seed_amount))

merged_dataset_clean %>% 
  filter(seriesa_amount != 0) %>% 
  summarise(mean(seriesa_amount),
            sd(seriesa_amount))         




  

