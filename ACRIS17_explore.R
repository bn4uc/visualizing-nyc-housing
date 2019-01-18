#Brianna Noonan
#Exploring ACRIS Real Property Master Data
#Data (filtered to doc dates in 2017):
#https://data.cityofnewyork.us/City-Government/ACRIS-Real-Property-Master/bnx9-e6tj

library(readr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)


options("scipen"=100, "digits"=4)
ACRIS_2017 <- read_csv("/Users/snsnsn1/Documents/UChicago/classes_winter_2019/data_viz/ACRIS_-_Real_Property_Master17.csv")

#need to make things dates
ACRIS_2017 <- ACRIS_2017 %>% 
  mutate(`DOC. DATE` = mdy(`DOC. DATE`),
         `RECORDED / FILED` = mdy(`RECORDED / FILED`),
         `GOOD THROUGH DATE` = mdy(`GOOD THROUGH DATE`),
         `MODIFIED DATE` = mdy(`MODIFIED DATE`))

#does good through date = doc date?
ACRIS_2017 %>% 
  mutate(no_match = ifelse(`DOC. DATE`!=`GOOD THROUGH DATE`, 1, 0)) %>%
  group_by(no_match) %>%
  summarise(n = n())

#sort into different types of documents
doc_control_codes <- read_csv("/Users/snsnsn1/Documents/UChicago/classes_winter_2019/data_viz/ACRIS_-_Document_Control_Codes.csv")
ACRIS_2017 <- ACRIS_2017 %>% left_join(doc_control_codes, by = "DOC. TYPE" )

#removing record type columns, since they are obsolete, ans only indicate where data came from
ACRIS_2017$`RECORD TYPE.x` <- NULL
ACRIS_2017$`RECORD TYPE.y` <- NULL

#investigate different types of documents being recorded
#look at sales and mortgages, amounts, and percentage transfers
deeds <- ACRIS_2017 %>% filter(str_detect(`DOC. TYPE DESCRIPTION`, "DEED"))
ggplot(deeds) + geom_point(aes(x = month(`DOC. DATE`), y = `DOC. AMOUNT`))
#winsorize amounts
deeds <- deeds %>% mutate(`DOC. AMOUNT` = statar::winsorize(deeds$`DOC. AMOUNT`, probs = c(.01, .99)))

#need to combine with real property legals in order to use property type
#could color that plot by property type^

#number of recordings for each type of document
ACRIS_2017 %>% group_by(`DOC. TYPE DESCRIPTION`) %>% summarise(n = n()) %>% View()

#number of documents for each borough (do this for a few years and a few types of docs and compare)
ACRIS_2017 %>% group_by(BOROUGH) %>% summarise(n = n())
#reassign borough numbers as names
#1 = Manhattan
#2 = Bronx
#3 = Brooklyn
#4 = Queens 

#number of deeds recorded each month, with different lines for borough
deeds %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE), 
         BOROUGH = as.character(BOROUGH), 
         BOROUGH = recode(BOROUGH, "1"= "Manhattan" , "2" = "Bronx" , "3" = "Brooklyn", "4" ="Queens")) %>% 
  group_by(month, BOROUGH) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = month, y = n, group = BOROUGH)) + 
  geom_point(aes(color = BOROUGH)) + geom_line(aes(color = BOROUGH)) + 
  labs(title = "Manhattan has significantly more deeds recorded each month",
       subtitle = "Deeds recorded by month in 2017",
       x = "Months", y = "Number of Deeds", caption = "Data Source: ACRIS")
