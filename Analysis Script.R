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
#setwd("~/Documents/BAM/Thesis/Data")

#READ IN DATA
analysis_data <- read.csv('analysisdata.csv')
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
rsltprobPOOL <- 
  plm(mdlprob, data = analysis_data1, 
      model = "pooling")
rsltprobFE <- 
  plm(mdlprob, data = analysis_data1, 
      index=c("name", "year"), model = "within")

rsltprobRE <- 
  plm(mdlprob, data = analysis_data1,
      index=c('name', 'year'), model = 'random')


#MULTICOLLINEARITY
vif_values_pool <- vif(rsltprobPOOL)


#Models 2
rsltlogPOOL <- 
  plm(mdllog, data = analysis_data1, 
      model = "pooling")
rsltlogFE <- 
  plm(mdllog, data = analysis_data1, 
      index=c("name", "year"), model = "within")

rsltlogRE <- 
  plm(mdllog, data = analysis_data1,
      index=c('name', 'year'), model = 'random')

#VIF Analyis

vif_values_log <- vif(rsltlogPOOL)
print(vif_values_log)

#Effects per country > handig om te checken:
summary(fixef(rsltprobFE, type="dmean"))

#Kiezen:
## TESTS H1/3
#Pooled to FE
pFtest(rsltprobFE, rsltprobPOOL)

#FE to RE
phtest(rsltprobFE, rsltprobRE)

##TESTS H2/4
#Pooled to FE
pFtest(rsltlogFE, rsltlogPOOL)

#FE to RE
phtest(rsltprobFE, rsltprobRE)

stargazer(rsltprobPOOL, rsltprobFE, rsltprobRE, title = 'Regression Results', 
          align=TRUE, no.space =TRUE, intercept.bottom=FALSE )

stargazer(rsltlogPOOL, rsltlogFE, rsltlogRE, align=TRUE, no.space =TRUE, intercept.bottom=FALSE)


