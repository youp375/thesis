#STARTUP TEST V2
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
panel_data$seedfunding <- ifelse(panel_data$year >= panel_data$yearseed, 1, 0)
panel_data$yearseriesA <- as.numeric(as.character(panel_data$yearseriesA))
panel_data$seriesafunding <- ifelse(panel_data$year >= panel_data$yearseriesA & !is.na(panel_data$yearseriesA), 1, 0)
panel_data <- panel_data %>% 
  arrange(name, year)

panel_data$headquarters <- gsub('Newcastle Upon Tyne', "Newcastle", panel_data$headquarters)
names(panel_data)[names(panel_data) == 'headquarters'] <- 'city'

#CREATING SEED VARIABLE
panel_data$seed_amount <- 0 # initialize variable to NA
panel_data$seed_amount[panel_data$lastfundingtype == "Seed" & panel_data$year >= panel_data$yearseed] <- 
  panel_data$lastfunding[panel_data$lastfundingtype == "Seed" & panel_data$year == panel_data$yearseed] # assign seed funding amount if last funding type is Seed and year matches yearseed
panel_data$seed_amount[panel_data$lastfundingtype == "Series A" & panel_data$year >= panel_data$yearseed] <- 
  panel_data$seedamount[panel_data$lastfundingtype == "Series A" & panel_data$year == panel_data$yearseed] # assign seed funding amount if last funding type is Series A and year matches yearseed

# create seriesa_amount variable
panel_data$seriesa_amount <- 0 # initialize variable to NA
panel_data$seriesa_amount[panel_data$lastfundingtype == "Series A" & 
                            panel_data$year >= panel_data$yearseriesA] <- panel_data$lastfunding[panel_data$lastfundingtype == "Series A" & panel_data$year == panel_data$yearseriesA] 
