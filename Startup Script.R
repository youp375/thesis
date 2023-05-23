##THIS FILE SERVES AS THE DATA ENGINEERING FILE FOR MY STARTUP DATA.
#FOR THE PATENT DATA SEE THE FILE 'OPEN INNOVATION SCRIPT'
#FOR THE MERGING FILE SEE THE FILE 'MERGING FURTHER'
#FOR THE FINAL ANALYSIS SEE THE FILE 'ANALYSES'

##LIBRARIES NEEDED
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
library(base)
library(corpus)

#WORKING DIRECTORY: 
#setwd("~/Documents/BAM/Thesis/Data")

#READING IN MY DATA
data1314 <- read_csv("DATA 13:14 SCRIPS.csv")
data1415 <- read_csv("DATA 14:15 scrips.csv")
data1516 <- read_csv("DATA 15:16.csv")
data1617 <- read_csv("DATA 16:17.csv")
data1718 <- read_csv("DATA 17:18.csv")
data1819 <- read_csv("DATA 18:19.csv")
#startupstest <-read_csv("testcompanies.csv")

#SOME DATA HAS A DIFFERENT STRUCTURE: 
data1316 <- rbind(data1314, data1415, data1516)
colnames(data1316) <- c('index', 'name', 'founders', 'profilefounder', 'link1', 'website', 'companytype', 'email', 
                        'description', 'revenue', 'revenuelink', 'founded', 'lastfunding', 'lastfundinglink',
                        'industrygroup', 'categorylink', 'status', 'cbprofile', 'totalfunding', 'lastfundingdate', 
                        'lastfundinglink', 'totalfundinglink', 'ipostatus', 'lastfundingtype', 'fundingtypelink', 'employeelink',
                        'employees','industries', 'industrylink', 'headquarters', 'headquarterslink', 'description',
                        'transactionname', 'tnamelink')

#GETTING THE RIGHT STARTUP NAMES:
data1316$name <- removeNumbers(data1316$name)
data1316$name <- trimws(data1316$name)
data1316$name <- gsub("[[:punct:]]", "", data1316$name)

#GETTING THE RIGHT CURRENCY
data1316$totalfunding <- str_replace_all(data1316$totalfunding, "\\$", "USD")
data1316$totalfunding <- str_replace_all(data1316$totalfunding, "\\£", "GBP")
data1316$totalfunding <- str_replace_all(data1316$totalfunding, "\\€", "EUR")
data1316$totalfundingcurrency <- str_sub(data1316$totalfunding, start = 1, end = 3)
data1316$totalfunding <- substring(data1316$totalfunding, 4)

#GETTING THE RIGHT CURRENCY FOR LASTFUNDING
data1316$lastfunding <- str_replace_all(data1316$lastfunding, "\\$", "USD")
data1316$lastfunding <- str_replace_all(data1316$lastfunding, "\\£", "GBP")
data1316$lastfunding <- str_replace_all(data1316$lastfunding, "\\€", "EUR")
data1316$lastfundingcurrency <- str_sub(data1316$lastfunding, start = 1, end = 3)
data1316$lastfunding <- substring(data1316$lastfunding, 4)

#MAKING IT A DOUBLE INSTEAD OF CHARACTER
data1316$lastfunding <- as.numeric(gsub(",","",data1316$lastfunding))
data1316$totalfunding <- as.numeric(gsub(",","",data1316$totalfunding))

#FINDING THE RIGHT YEAR OF THE DATA
data1316$yearfounded <- str_sub(data1316$founded, start= -4)
data1316$lastfundingdate <- as.Date(data1316$lastfundingdate, format = "%B %d, %Y")

#FINALIZING ALL DATA! 
data1316 <- data1316[, c('name', 'cbprofile', 'totalfunding', 'totalfundingcurrency', 'ipostatus', 'lastfundingtype', 'employees', 'industries', 'headquarters', 'description',
                        'transactionname', 'tnamelink', 'founders', 'website', 'companytype', 'email', 'revenue', 'yearfounded', 'lastfunding', 'lastfundingcurrency', 'industrygroup', 'status',
                         'lastfundingdate')]
data1619 <- rbind(data1617, data1718, data1819)
colnames(data1619) <- c('name', 'cbprofile', 'totalfunding', 'totalfundingcurrency', 'totalfundingusd', 'ipostatus', 'lastfundingtype', 'employees', 'industries', 'headquarters', 'description',
                        'transactionname', 'tnamelink', 'founders', 'website', 'companytype', 'email', 'linkedin', 'fulldescription', 'revenue', 'founded', 'precision',  'lastfunding', 'lastfundingcurrency','lastfundingusd',  'industrygroup', 'status',
                        'lastfundingdate')
data1619$yearfounded <- str_sub(data1619$founded, start= 1,4)
data1619 <- data1619[, c('name', 'cbprofile', 'totalfunding', 'totalfundingcurrency', 'ipostatus', 'lastfundingtype', 'employees', 'industries', 'headquarters', 'description',
                        'transactionname', 'tnamelink', 'founders', 'website', 'companytype', 'email', 'revenue', 'yearfounded', 'lastfunding', 'lastfundingcurrency', 'industrygroup', 'status',
                         'lastfundingdate')]
full_data <- bind_rows(data1316, data1619)
full_data$name <- trimws(full_data$name)
write.csv(full_data, 'Startup Data.csv')

################################################################################

#READ IN THE DATA
full_data <- read.csv('Startup Data.csv')

#CHANGE HEADQUARTERS TO CITY NAMES
full_data <- full_data  %>% 
  mutate(
    headquarters = sapply(strsplit(headquarters, ","), function(x) trimws(x[1]))
)

#SEE IF THERE ARE DUPLICATES
full_data[duplicated(full_data)]

#CREATE NUM OF FOUNDERS VARIABLE + YEARFUNDING VARIABLE
full_data <- full_data %>% 
  mutate(
    num_founders = str_count(founders, ",") + 1,
    yearfunding = str_sub(lastfundingdate, start= 1,4),
    yearfunding = as.factor(yearfunding)
  ) %>% 
  select(-X)

#SEPARATE THE INDUSTRIES AND INDUSTRYGROUPS
full_data <- full_data %>% 
  separate(industries, sep = ",", into = paste0("Industry", 1:5), extra = "drop") %>% 
  separate(industrygroup, sep = ",", into = paste0("Industrygroup", 1:5), extra = "drop")

#DELETE THE CITIES THAT OCCUR LESS THAN 10 TIMES
startupsUK <- full_data %>% 
  group_by(headquarters) %>% 
  filter(n() >10) %>% 
  ungroup()

#CREATE A NEW DATAFILE OF STARTUPS BEYOND PRE-SEED. 
matrixuk <- startupsUK %>% 
  filter(
    lastfundingtype != 'Pre-Seed'
  )
seriesa <- matrixuk %>% 
  filter(
    lastfundingtype == 'Series A'
  ) 
seed <- matrixuk %>% 
  filter(
    lastfundingtype == 'Seed'
  )
other <- matrixuk %>% 
  filter(
    lastfundingtype != 'Seed'
  ) %>% 
  filter(
    lastfundingtype != 'Series A'
  )

#ENRICH THE SERIES A FILE WITH INFORMATION ABOUT THEIR SEED ROUNDS. 
seedrounds <- read_csv('seedround.csv')

#CHANGE NAMES
seedrounds2 <- seedrounds %>% 
  select("Organization Name", "Money Raised", "Money Raised Currency",
         "Announced Date", "Money Raised Currency (in USD)"
  )
colnames(seedrounds2) <- c('name', 'seedamount', 'currency', 'date','seedamountusd')

#MERGE THE DATA WITH INFORMATION ABOUT THE SEED ROUNDS
seriesa2 = merge(seriesa, seedrounds2, by='name')

#CHANGE THE NAMES
names(seriesa2)[names(seriesa2) == 'date'] <- 'saseedround'
names(seriesa2)[names(seriesa2) == 'yearfunding'] <- 'yearlastfunding'

#CREATE A NEW VARIABLE CALLED 'YEARSEED'
seriesa2$yearseed <- as.numeric(as.character(str_sub(seriesa2$saseedround, 1,4)))

#FILTER SOME OBSERVATIONS WITH 'NA'
seriesa3 <- seriesa2 %>% 
  filter(
  seedamount != 'NA'
)

#MAKE SURE ONLY THE FIRST SEED ROUND IS COUNTED IN THE ANALYSIS
seriesa4 <- seriesa3 %>%
  group_by(name) %>%
  slice(which.min(yearseed)) %>%
  ungroup()

#COMBINE THE DATA ABOUT SEED COMPANIES AND SERIES A COMPANIES
dataset3 <- bind_rows(seriesa4, seed)

#CHANGE THE YEARFUNDING VARIABLE TO A NUMERIC
dataset3$yearfunding <- as.numeric(as.character(dataset3$yearfunding))

#COMBINE YEARFUNDING AND YEAR SEED AND DELETE 'YEARLASTFUNDING'
dataset3 <- dataset3 %>%
  mutate(yearseed = coalesce(yearseed, yearfunding)) %>%
  select(-yearfunding)

#CHANGE THE NAME
names(dataset3)[names(dataset3) == 'yearlastfunding'] <- 'yearseriesA'

#OTHER ROUNDS:
otherrounds <- read_csv('furtherfunding.csv')

#MAKE SURE TO ONLY KEEP THE RELEVANT VARIABLES
otherrounds <- otherrounds %>% 
  select("Organization Name", "Funding Type", "Money Raised", "Money Raised Currency",
         "Announced Date", "Money Raised Currency (in USD)"
  )

#CHANGE THE NAME OF THE VARIABLES FOR MERGING
colnames(otherrounds) <- c('name', 'fundingtype', 'amount', 'currency', 'date','amountusd')

#MERGE THE DATAFRAME
extra_rounds <- merge(other, otherrounds, by='name')

#CREATING A DATAFRAME SIMILAR TO DATASET3
extra_rounds <- extra_rounds %>% 
  mutate(
    seedamount = ifelse(fundingtype == "Seed", amount, NA),
    yearseed = as.double(as.character(ifelse(fundingtype == 'Seed', substr(date, 1,4), NA))),
    seriesaamount = ifelse(fundingtype == 'Series A', amount, NA),
    yearseriesa = as.double(as.character(ifelse(fundingtype == 'Series A', substr(date, 1,4), NA)))
  )

#SPLIT THE DATAFRAME:
extra_rounds_seed <- extra_rounds %>% 
  filter(
    fundingtype == 'Seed'
  )
extra_rounds_seriesa <- extra_rounds %>% 
  filter(
    fundingtype == 'Series A'
  )

#MAKE SURE THAT ONLY 1 FUNDINGROUND REMAINS:
extra_rounds_seed <- extra_rounds_seed %>%
  group_by(name) %>%
  slice(which.min(yearseed)) %>%
  ungroup()

extra_rounds_seriesa <- extra_rounds_seriesa %>%
  group_by(name) %>%
  slice(which.min(yearseriesa)) %>%
  ungroup()

extra_rounds2 <- bind_rows(extra_rounds_seed, extra_rounds_seriesa)

#KEEP ONLY COMPANIES WITH BOTH ROUNDS:
extra_rounds2 <- extra_rounds2 %>%
  group_by(name) %>%
  filter(all(c('Seed', 'Series A') %in% fundingtype)) %>%
  ungroup() %>% 
  arrange(name) %>% 
  select(-fundingtype, -amount, -amountusd, -date, -currency)

##KEEP ONLY ONE OBSERVATION PER COMPANY

extra_rounds2 <- extra_rounds2 %>%
  group_by(name) %>%
  fill(seedamount, yearseed, seriesaamount, yearseriesa) %>%
  slice(2) %>% 
  ungroup()



#CREATING THE PANEL DATA FOR 1 
years <- seq(min(dataset3$yearfounded), 2023)

dataset3$industrycategory <- ifelse(dataset3$Industrygroup1 %in% c("Artificial Intelligence", "Machine Learning", "Apps", "Information Technology",
                                                                   "Data and Analytics", "Internet Services", "Mobile",
                                                                   "Software"), "Digital Technology",
                                    ifelse(dataset3$Industrygroup1 %in% c("Financial Services", "Real Estate"), "Finance",
                                           ifelse(dataset3$Industrygroup1 %in% c("Hardware", "Biotechnology", "Agriculture and Farming"), "Deep Tech",
                                                  ifelse(dataset3$Industrygroup1 %in% c("Health Care", "Pharmaceuticals"), "Healthcare",
                                                         ifelse(dataset3$Industrygroup1 %in% c("Manufacturing", "Energy"), "Energy",
                                                                ifelse(dataset3$Industrygroup1 %in% c("Food and Beverage"), "Food and Beverages",
                                                                       ifelse(dataset3$Industrygroup1 %in% c("Consumer Electronics", "Clothing and Apparel", "Commerce and Shopping", "Community and Lifestyle"
                                                                       ), "Consumer Goods",
                                                                       ifelse(dataset3$Industrygroup1 %in% c("Advertising", "Administrative Services"), "Marketing and Advertising", "Other"))))))))




#FIRST PANEL DATASET
panel_data <- expand.grid(name = unique(dataset3$name), year = years)

#MERGE IN ALL INFORMATION
panel_data <- merge(panel_data, dataset3, by = "name", all.x = TRUE)

#CREATING SEEDFUNDING AND SERIESAFUNDING DUMMIES
panel_data$seedfunding <- ifelse(panel_data$year == panel_data$yearseed, 1, 0)
panel_data$yearseriesA <- as.numeric(as.character(panel_data$yearseriesA))
panel_data$seriesafunding <- ifelse(panel_data$year == panel_data$yearseriesA & !is.na(panel_data$yearseriesA), 1, 0)
panel_data <- panel_data %>% 
  arrange(name, year)

panel_data$headquarters <- gsub('Newcastle Upon Tyne', "Newcastle", panel_data$headquarters)
names(panel_data)[names(panel_data) == 'headquarters'] <- 'city'

#CREATING SEED VARIABLE
panel_data$seed_amount <- 0 # initialize variable to NA
panel_data$seed_amount[panel_data$lastfundingtype == "Seed" & panel_data$year == panel_data$yearseed] <- panel_data$lastfunding[panel_data$lastfundingtype == "Seed" & panel_data$year == panel_data$yearseed] # assign seed funding amount if last funding type is Seed and year matches yearseed
panel_data$seed_amount[panel_data$lastfundingtype == "Series A" & panel_data$year == panel_data$yearseed] <- panel_data$seedamount[panel_data$lastfundingtype == "Series A" & panel_data$year == panel_data$yearseed] # assign seed funding amount if last funding type is Series A and year matches yearseed

# create seriesa_amount variable
panel_data$seriesa_amount <- 0 # initialize variable to NA
panel_data$seriesa_amount[panel_data$lastfundingtype == "Series A" & 
                            panel_data$year == panel_data$yearseriesA] <- panel_data$lastfunding[panel_data$lastfundingtype == "Series A" & panel_data$year == panel_data$yearseriesA] 











##CREATE THE SECOND BATCH OF PANEL DATA

extra_rounds2$industrycategory <- ifelse(extra_rounds2$Industrygroup1 %in% c("Artificial Intelligence", "Machine Learning", "Apps", "Information Technology",
                                                                   "Data and Analytics", "Internet Services", "Mobile",
                                                                   "Software"), "Digital Technology",
                                    ifelse(extra_rounds2$Industrygroup1 %in% c("Financial Services", "Real Estate"), "Finance",
                                           ifelse(extra_rounds2$Industrygroup1 %in% c("Hardware", "Biotechnology", "Agriculture and Farming"), "Deep Tech",
                                                  ifelse(extra_rounds2$Industrygroup1 %in% c("Health Care", "Pharmaceuticals"), "Healthcare",
                                                         ifelse(extra_rounds2$Industrygroup1 %in% c("Manufacturing", "Energy"), "Energy",
                                                                ifelse(extra_rounds2$Industrygroup1 %in% c("Food and Beverage"), "Food and Beverages",
                                                                       ifelse(extra_rounds2$Industrygroup1 %in% c("Consumer Electronics", "Clothing and Apparel", "Commerce and Shopping", "Community and Lifestyle"
                                                                       ), "Consumer Goods",
                                                                       ifelse(extra_rounds2$Industrygroup1 %in% c("Advertising", "Administrative Services"), "Marketing and Advertising", "Other"))))))))




# Create a panel dataset with all possible combinations of startup and year
panel_data_extra <- expand.grid(name = unique(extra_rounds2$name), year = years)

# Merge in the yearseed and yearseriesa information from dataset3
panel_data_extra <- merge(panel_data_extra, extra_rounds2, by = "name", all.x = TRUE)

# Create the seedfunding and seriesafunding variables
panel_data_extra$seedfunding <- ifelse(panel_data_extra$year == panel_data_extra$yearseed, 1, 0)
panel_data_extra$seriesafunding <- ifelse(panel_data_extra$year == panel_data_extra$yearseriesa & !is.na(panel_data_extra$yearseriesa), 1, 0)
panel_data_extra <- panel_data_extra %>% 
  arrange(name, year)

panel_data_extra$headquarters <- gsub('Newcastle Upon Tyne', "Newcastle", panel_data_extra$headquarters)
names(panel_data_extra)[names(panel_data_extra) == 'headquarters'] <- 'city'

#CREATING SEED AND SERIESA VARIABLE
panel_data_extra$seed_amount <- 0 # initialize variable to NA
panel_data_extra$seed_amount[panel_data_extra$year == panel_data_extra$yearseed] <- panel_data_extra$seedamount[panel_data_extra$year == panel_data_extra$yearseed] # assign seed funding amount if last funding type is Seed and year matches yearseed
panel_data_extra$seriesa_amount <- 0 # initialize variable to NA
panel_data_extra$seriesa_amount[panel_data_extra$year == panel_data_extra$yearseriesa] <- panel_data_extra$seriesa_amount[panel_data_extra$year == panel_data_extra$yearseriesa]


#MERGE ALL THIS DATA TOGETHER FOR THE FINAL STARTUPS DATASET. 
panel_data_pre_merge1 <- panel_data %>% 
  select('year', 'name', 'city', 'description', 'industrycategory', 'totalfunding', 'ipostatus',
        'employees', 'description', 'founders','companytype', 'status', 'num_founders', 'seedfunding', 'seriesafunding','seed_amount', 'seriesa_amount')               
panel_data_pre_merge2 <- panel_data_extra %>% 
  select('year', 'name', 'city', 'description', 'industrycategory', 'totalfunding', 'ipostatus',
         'employees', 'description', 'founders','companytype', 'status', 'num_founders', 'seedfunding', 'seriesafunding','seed_amount', 'seriesa_amount')

panel_startups_final <- bind_rows(panel_data_pre_merge1, panel_data_pre_merge2)

names(panel_startups_final)[names(panel_startups_final) == 'industrycategory'] <- 'industry'

