#CREATING THE PATENTDATA

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
library(readxl)


#READING IN PATENTDATA
bath <- read_csv("Patentdata/bath.csv", skip=1)
bath <- bath %>% 
  mutate(
    city = 'Bath'
  )
belfast <- read_csv("Patentdata/belfast.csv", skip=1) 
belfast <- belfast %>% 
  mutate(
    city = 'Belfast'
  )
birmingham <- read_csv("Patentdata/birmingham.csv", skip=1)
birmingham <- birmingham %>% 
  mutate(
    city = 'Birmingham'
  )
brighton <- read_csv("Patentdata/brighton.csv", skip=1)
brighton <- brighton %>% 
  mutate(
    city = 'Brighton'
  )
bristol <- read_csv("Patentdata/bristol.csv", skip=1)
bristol <- bristol %>% 
  mutate(
    city = 'Bristol'
  )
cambridge <- read_csv("Patentdata/cambridge.csv", skip=1)
cambridge <- cambridge %>% 
  mutate(
    city = 'Cambridge'
  )
cardiff <- read_csv("Patentdata/cardiff.csv", skip=1)
cardiff <- cardiff %>% 
  mutate(
    city = 'Cardiff'
  )
edinburgh <- read_csv("Patentdata/edinburgh.csv", skip=1)
edinburgh <- edinburgh %>% 
  mutate(
    city = 'Edinburgh'
  )
glasgow <- read_csv("Patentdata/glasgow.csv", skip=1)
glasgow <- glasgow %>% 
  mutate(
    city = 'Glasgow'
  )
leeds <- read_csv("Patentdata/leeds.csv",  skip=1)
leeds <- leeds %>% 
  mutate(
    city = 'Leeds'
  )
liverpool <- read_csv("Patentdata/liverpool.csv", skip=1)
liverpool <- liverpool %>% 
  mutate(
    city = 'Liverpool'
  )
london <- read_csv("Patentdata/london.csv", skip=1)
london <- london %>% 
  mutate(
    city = 'London'
  )
manchester <- read_csv("Patentdata/manchester.csv", skip=1)
manchester <- manchester %>% 
  mutate(
    city = 'Manchester'
  )
oxford <- read_csv("Patentdata/oxford.csv", skip=1)
oxford <- oxford %>% 
  mutate(
    city = 'Oxford'
  )
newcastle <- read_csv("Patentdata/newcastle.csv", skip=1)
newcastle <- newcastle %>% 
  mutate(
    city = 'Newcastle'
  )
nottingham <- read_csv("Patentdata/nottingham.csv", skip=1)
nottingham <- nottingham %>% 
  mutate(
    city = 'Nottingham'
  )
reading <- read_csv("Patentdata/reading.csv", skip=1)
reading <- reading %>% 
  mutate(
    city = 'Reading'
  )
sheffield <- read_csv("Patentdata/sheffield.csv", skip=1)
sheffield <- sheffield %>% 
  mutate(
    city = 'Sheffield'
  )
southampton <- read_csv("Patentdata/southampton.csv", skip=1)
southampton <- southampton %>% 
  mutate(
    city = 'Southampton'
  )

#CREATING A TOTAL DATAFRAME FOR THE DATA
patentdata <- bind_rows(bath, belfast, birmingham, brighton, bristol, cambridge, cardiff, 
                        edinburgh, glasgow, leeds, liverpool, london, manchester, oxford,
                        newcastle, nottingham, reading, sheffield, southampton)
colnames(patentdata) <- c('id', 'title', 'assignee', 'author', 'prioritydate', 'filingdate',
                          'publicationdate', 'grantdate', 'link', 'representativelink', 'city')
write.csv(patentdata, 'data/patentdata.csv')

###START FROM HERE
patentdata2 <- read_csv('data/patentdata.csv')
names(patentdata2)[names(patentdata2) == '...1'] <- 'index'

##FURTHER CODING

##ASSIGNING AN INDUSTRY BASED ON THEIR TITLE
#UNNEST TOKENS FOR KEYWORD MATCHING
patentcopy <- patentdata2 %>% 
  unnest_tokens(word, title)
#CHECK STOP WORDS
data(stop_words)
patentcopy <- patentcopy %>% 
  anti_join(stop_words)
#STEM PATENT DATA
patentcopy$word <- text_tokens(patentcopy$word, stemmer = 'en')
patentcopy$word <- as.character(patentcopy$word)

#READ IN KEYWORDS
keywords <- read_excel("data/keywords.xlsx")
keywords$word <- as.character(keywords$word)
keywords <- keywords %>% 
  unnest_tokens(word, word)
#CHECK STOPWORDS FOR KEYWORDS
keywords <- keywords %>% 
  anti_join(stop_words)
#STEM KEYWORD DATA
keywords$word <- text_tokens(keywords$word, stemmer = 'en')
#MAKE SURE THERE ARE ONLY UNIQUE VALUES
keywords <- distinct(keywords)

#CREATE A SUBDATASET THAT MATCHES THE KEYWORDS TO THE PATENT DATA
check <- merge(patentcopy, keywords, by = "word", all.x = TRUE)


#IF THE DATA DOES NOT FIT, MAKE SURE ITS DEEP TECH
check$industry <- ifelse(is.na(check$industry), "Deep Tech", check$industry)

#CREATE A NEW DATASET WITH INDUSTRY CLASSIFICATIONS
patent_dataset <- check %>%
  group_by(id) %>%
  summarise(title = paste(word, collapse = " "), 
            assignee = first(assignee),
            author = first(author),
            city = first(city),
            industry = first(industry)) %>%
  spread(industry, industry, fill = "NA")

#CREATE INDUSTRY VARIABLE
patent_dataset <- patent_dataset %>%
  mutate(industry = case_when(
    Energy == "Energy" ~ "Energy",
    Healthcare == "Healthcare" ~ "Healthcare",
    'Deep Tech' == "Deep Tech" ~ "Deep Tech",
    TRUE ~ "Other"
  ))

#MAKE SURE TO MERGE THE DATA
patent_merge <- patent_dataset %>% 
  select(id, industry)

#FINAL DATASET THAT CAN BE CONVERTED INTO PANEL DATA
patentfinal <- merge(patentdata2, patent_merge, by='id')
patentfinal

##CREATE PANELDATA INCLUDING CUMULATIVE PATENTS PER CITY

#CREATE A YEARS SEQUENCE:
years <- seq(2010, 2023)

#CONVERT THE PUBLICATION YEAR TO 'YEAR'
patentfinal <- patentfinal %>%
  mutate(Pub_Year = as.numeric(format(as.Date(publicationdate), "%Y"))) %>%
  filter(!is.na(Pub_Year)) %>%
  filter(Pub_Year %in% years)


#CREATE A PANEL DATASET FOR CITY
panel_patents_city <- expand.grid(city = unique(patentfinal$city)) %>%
  left_join(patentfinal %>%
              group_by(city, Pub_Year) %>%
              summarize(n_patents = n()))

names(panel_patents_city)[names(panel_patents_city) == 'Pub_Year'] <- 'year'

panel_patents_city <- panel_patents_city %>%
  group_by(city) %>%
  mutate(cum_patents_city = cumsum(n_patents)) %>% 
  ungroup()

#CREATE A PANEL DATASET FOR INDUSTRY
panel_patents_industry <- expand.grid(industry = unique(patentfinal$industry)) %>%
  left_join(patentfinal %>%
              group_by(industry, Pub_Year) %>%
              summarize(n_patents_ind = n()))

names(panel_patents_industry)[names(panel_patents_industry) == 'Pub_Year'] <- 'year'

panel_patents_industry <- panel_patents_industry %>%
  group_by(industry) %>%
  mutate(cum_patents_industry = cumsum(n_patents_ind)) %>% 
  ungroup()


















