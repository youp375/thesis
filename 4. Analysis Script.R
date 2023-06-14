####ANALYSIS SCRIPT####

#LIBRARIES
library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(magrittr)
library(tidyr)
library(tidyverse)
library(base)
library(corpus)
library(stargazer)
library(plyr)
library(ggplot2)
library(car)
library(plm)

#WORKING DIRECTORY


#READ IN DATA
analysis_data <- read.csv('data/analysisdata.csv')
analysis_data <- analysis_data %>% 
  dplyr::select('name', 'year', 'city', 'description', 'industry', 'logtotalfunding', 'ipostatus',
                'employees', 'description', 'founders',
                'companytype', 'status', 'num_founders', 'seedfunding', 'seriesafunding',
                'logseed', 'logseriesa', 'n_patents','cum_patents_city', 'n_patents_ind', 'cum_patents_industry'
  )

#CUM PATENTS 0 OR NOT
analysis_data1 <- analysis_data %>% 
  mutate(n_patents_ind = coalesce(n_patents_ind, 0),
         cum_patents_industry = coalesce(cum_patents_industry, 0))

analysis_data1 <- distinct(analysis_data1)

#CORRELATION MATRIX
corvars <- analysis_data1 %>% dplyr::select(num_founders, seedfunding, seriesafunding, logseed, logseriesa, 
                                     cum_patents_city, cum_patents_industry)
cor_matrix <- cor(corvars, use = 'complete.obs')
stargazer(cor_matrix, no.space=TRUE)



#MODELLEN 
mdlprob <- seriesafunding ~ cum_patents_city + cum_patents_industry + log(num_founders) + industry 
mdllog <- logseriesa ~ cum_patents_city + cum_patents_industry + log(num_founders) + logseed + industry


#Models 1
rsltprobFE <- 
  plm(mdlprob, data = analysis_data1, 
      index=c("year"), model = "within")

#MULTICOLLINEARITY
vif_values_pool <- vif(rsltprobFE)


#Models 2
rsltlogFE <- 
  plm(mdllog, data = analysis_data1, 
      index=c("year"), model = "within")

#VIF Analyis
vif_values_log <- vif(rsltlogPOOL)
print(vif_values_log)

#Effects per country > handig om te checken:
summary(fixef(rsltprobFE, type="dmean"))

stargazer(rsltprobFE, rsltlogFE, title = 'Regression Results', 
          align=TRUE, no.space =TRUE, intercept.bottom=FALSE, type='text')


#ROBUSTNESS CHECKS

#Including startup effects

mdlprobfe <- seriesafunding ~ cum_patents_city + cum_patents_industry 
mdllogfe <- logseriesa ~ cum_patents_city + cum_patents_industry 

m1 <- plm(mdlprobfe, data = analysis_data1, 
      index=c("name", "year"), model = "within")

m2 <-plm(mdllogfe, data = analysis_data1, 
      index=c("name", "year"), model = "within")

nolondon <- analysis_data1 %>% 
  filter(
    city != 'London'
  )
onlyht <- analysis_data1 %>% 
  filter(
    industry == 'Energy'| industry == 'Deep Tech'|industry =='Healthcare'
  )

m3 <- 
  plm(mdlprob, data = nolondon, 
      index=c("year"), model = "within")

m4 <- 
  plm(mdllog, data = nolondon, 
      index=c("year"), model = "within")

m5 <-  plm(mdlprob, data = onlyht, 
                   index=c("year"), model = "within")

m6 <- 
  plm(mdllog, data = onlyht, 
      index=c("year"), model = "within")

stargazer(m1,m3,m5,m2,m4,m6, title = 'Robustness Check',
          align = TRUE, no.space = TRUE, intercept.bottom = FALSE)



