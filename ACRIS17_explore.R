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
ACRIS_master <- read_csv("ACRIS_real_property_master.csv")
ACRIS_legals <- read_csv("ACRIS_real_property_legals.csv")
ACRIS_parties <- read_csv("ACRIS_real_property_parties.csv")
ACRIS17 <- read_csv("ACRIS_-_Real_Property_Master17.csv")

#this won't run - too big
#make dates and change boroughs
ACRIS_master <- ACRIS_master %>% 
  mutate(`DOC. DATE` = mdy(`DOC. DATE`),
         `RECORDED / FILED` = mdy(`RECORDED / FILED`),
         `GOOD THROUGH DATE` = mdy(`GOOD THROUGH DATE`),
         `MODIFIED DATE` = mdy(`MODIFIED DATE`))

#this works, but doesn't change the size much, which means most docs are in this period
#subset to a five year period(2013 - 2017)
ACRIS_master_5yr <- ACRIS_master %>% 
  filter(str_detect(`GOOD THROUGH DATE`, ("2013|2014|2015|2016|2017")))

#switch to using 1yr of data (2017) because the others are too large to work with

#merge with document codes 
doc_control_codes <- read_csv("ACRIS_-_Document_Control_Codes.csv")
ACRIS17 <- ACRIS17 %>% left_join(doc_control_codes, by = "DOC. TYPE" )

ACRIS17$`RECORD TYPE.x` <- NULL
ACRIS17$`RECORD TYPE.y` <- NULL

ACRIS17 <- ACRIS17 %>% 
  mutate(`DOC. DATE` = mdy(`DOC. DATE`),
         `RECORDED / FILED` = mdy(`RECORDED / FILED`),
         `GOOD THROUGH DATE` = mdy(`GOOD THROUGH DATE`),
         `MODIFIED DATE` = mdy(`MODIFIED DATE`), 
         BOROUGH = as.character(BOROUGH), 
         BOROUGH = recode(BOROUGH, "1"= "Manhattan" ,
                          "2" = "Bronx" , 
                          "3" = "Brooklyn",
                          "4" ="Queens")) 

#merge with legals
ACRIS17 <- ACRIS17 %>% mutate(`DOCUMENT ID` = as.character(`DOCUMENT ID`))
ACRIS17 <- left_join(ACRIS17, ACRIS_legals, by = "DOCUMENT ID")

#merge with property
property_codes <- read_csv("ACRIS_-_Property_Types_Codes.csv")
ACRIS17 <- ACRIS17 %>% left_join(property_codes, by = "PROPERTY TYPE")

#removing duplicates
ACRIS17$BOROUGH.y <- NULL
ACRIS17$`RECORD TYPE.x` <- NULL
ACRIS17$`RECORD TYPE.y` <- NULL

#check to see if good through dates are the same
ACRIS17 <- ACRIS17 %>% mutate(`GOOD THROUGH DATE.y` = mdy(`GOOD THROUGH DATE.y`))
ACRIS17 %>% filter(`GOOD THROUGH DATE.x` != `GOOD THROUGH DATE.y`) %>% nrow()

#create df for deed transers only
deed_transfers17 <- ACRIS17 %>% 
  filter(str_detect(`DOC. TYPE DESCRIPTION`, "DEED") & `% TRANSFERRED`== 100)

deed_transfers17$BOROUGH.y<- NULL
deed_transfers17 <- deed_transfers17 %>% rename(BOROUGH = BOROUGH.x)

####plotting number of deeds transfers (100%) per month, by borough####
graph_deedtransfers_permonth <- deed_transfers17 %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>% 
  group_by(month, BOROUGH) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = month, y = n, group = BOROUGH)) + 
  geom_point(aes(color = BOROUGH)) + geom_line(aes(color = BOROUGH)) + 
  labs(title = "Manhattan has significantly more deeds recorded each month",
       subtitle = "Deeds recorded by month in 2017",
       x = "Months", y = "Number of Deeds", 
       caption = "Data Source: NYC Dept. of Finance, ACRIS")
ggsave("graph_deedtransfers_permonth.pdf")

####plotting average amounts per deed transfer per borough####
deed_transfers17 %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>% 
  group_by(month, BOROUGH) %>% 
  summarise(n = n(), avg_amount = mean(`DOC. AMOUNT`)) %>%
  ggplot() + geom_point(aes(x = month, y = avg_amount, color = BOROUGH))

#deed amount by month with color as doc type
deed_transfers17 %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>% 
  group_by(month, `DOC. TYPE`) %>% 
  summarise(n = n(), avg_amount = mean(`DOC. AMOUNT`)) %>% 
  ggplot() + geom_point(aes(x = month, y = avg_amount, color = `DOC. TYPE`))

#deed amounts by month with color as the borough
deed_transfers17 %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>% 
  group_by(month, BOROUGH) %>% 
  summarise(n = n(), avg_amount = mean(`DOC. AMOUNT`)) %>% 
  ggplot() + geom_point(aes(x = month, y = avg_amount, color = BOROUGH)) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Month", y = "Average Document Amount ($)", 
       title = "Highest Property Values are in Manhattan", 
       subtitle= "Average amounts listed on deed transfers in 2017, by month", 
       caption = "Data Source: NYC Dept. of Finance, ACRIS", color = "Borough")

#need to bin the amounts because there are really low ones
deed_transfers17 <- deed_transfers17 %>% 
  mutate(`DOC. AMOUNT` = as.numeric(`DOC. AMOUNT`) ,amounts_bin = ifelse(`DOC. AMOUNT`>= 0 & `DOC. AMOUNT` <=50000, "0-50,000",
                                                                         ifelse(`DOC. AMOUNT`>50000 & `DOC. AMOUNT` <=300000, "50,000-300,000", 
                                                                                ifelse(`DOC. AMOUNT`>300000 & `DOC. AMOUNT` <=550000, "300,000-550,000", 
                                                                                       ifelse(`DOC. AMOUNT`>550000 & `DOC. AMOUNT` <=800000, "550,000-800,000", 
                                                                                              ifelse(`DOC. AMOUNT`>800000 & `DOC. AMOUNT` <=1050000, "800,000-1,050,000", 
                                                                                                     ifelse(`DOC. AMOUNT`>1050000 & `DOC. AMOUNT` <=1300000, "1,050,000-1,300,000", 
                                                                                                            ifelse(`DOC. AMOUNT`>1300000 & `DOC. AMOUNT` <=1550000, "1,300,000-1,550,000", 
                                                                                                                   ifelse(`DOC. AMOUNT`>1550000 & `DOC. AMOUNT` <=1800000, "1,550,000-1,800,000", 
                                                                                                                          ifelse(`DOC. AMOUNT`>1800000, "Over 1,800,000", NA))))))))))

#plot of # of sales by grouped amounts, per borough 
ggplot(deed_transfers17) + 
  geom_bar(aes(x = amounts_bin, fill = BOROUGH)) + coord_flip()

deed_transfers17 %>% 
  group_by(BOROUGH, amounts_bin) %>% 
  summarise(count = n()) %>% ggplot() + 
  geom_bar(aes(x = amounts_bin, y = count, fill = BOROUGH), stat = "identity", position = position_dodge()) + 
  coord_flip()

#number of deed transfers in each amounts bin, color = borough
deed_transfers17 %>% 
  group_by(BOROUGH, amounts_bin) %>% 
  summarise(count = n()) %>% ggplot() + 
  geom_jitter(aes(x = amounts_bin, y = count, color = BOROUGH), size = 2.5) + 
  coord_flip() + scale_y_continuous(labels = scales::comma) + labs(x = "Amount ($)", y =  "Number of deeds transfers",
                                                                   title = "Large number of transfers made for low amounts", 
                                                                   subtitle = "Number of 100% interest deed transfers in 2017 by amount",
                                                                   caption = "Data Source: NYC Dept. of Finance, ACRIS", 
                                                                   color = "Borough")
ggsave("deedtransfers_binned_borough.pdf")

#looking at the different types of docs recorded
ACRIS17 %>% group_by(`DOC. TYPE DESCRIPTION`) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% View()

#stacked bar chart of mortgages and satisfactions of mortgages filed each month
ACRIS17 %>% filter(`DOC. TYPE DESCRIPTION` %in% 
                     c("MORTGAGE", "SATISFACTION OF MORTGAGE")) %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>%
  group_by(`DOC. TYPE DESCRIPTION`, month) %>%
  summarise(n = n()) %>% 
  ggplot() + geom_bar(aes(x = month, 
                          y = n, fill = `DOC. TYPE DESCRIPTION`), stat = "identity")

ACRIS17 %>% filter(`DOC. TYPE DESCRIPTION` %in% 
                     c("MORTGAGE", "SATISFACTION OF MORTGAGE")) %>% 
  mutate(month = month(`DOC. DATE`, label = TRUE)) %>%
  group_by(`DOC. TYPE DESCRIPTION`, BOROUGH, month) %>%
  summarise(n = n()) %>% 
  ggplot() + geom_histogram(aes(x = month, 
                                y = n, fill = `DOC. TYPE DESCRIPTION`), stat = "identity") +
  facet_wrap(vars(BOROUGH))

#lets look at property types and amounts of sales 

#top 10 types of properties re deed transfers
descriptions_top10 <- c("SINGLE RESIDENTIAL CONDO UNIT",            
                        "DWELLING ONLY - 1 FAMILY",                 
                        "DWELLING ONLY - 2 FAMILY",                 
                        "DWELLING ONLY - 3 FAMILY",                  
                        "COMMERCIAL CONDO UNIT(S)",                  
                        "1-2 FAMILY DWELLING WITH ATTACHED GARAGE",  
                        "COMMERCIAL REAL ESTATE",                    
                        "TIMESHARE",                                 
                        "APARTMENT BUILDING",                        
                        "1-3 FAMILY WITH STORE / OFFICE")

descriptions_top5 <- c("SINGLE RESIDENTIAL CONDO UNIT",            
                       "DWELLING ONLY - 1 FAMILY",                 
                       "DWELLING ONLY - 2 FAMILY",                 
                       "DWELLING ONLY - 3 FAMILY",                  
                       "COMMERCIAL CONDO UNIT(S)")

#property types and the number of each by borough
ACRIS17 %>% 
  filter(str_detect(`DOC. TYPE DESCRIPTION`, "DEED") & `% TRANSFERRED`== 100 &
           DESCRIPTION %in% descriptions_top10) %>%
  group_by(DESCRIPTION, BOROUGH.x) %>% 
  summarise(n = n()) %>% ggplot() + 
  geom_jitter(aes(x = reorder(DESCRIPTION, -n), y = n, color = BOROUGH.x)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#property values by type of property in 2017, for deed transfers top 5 n
deed_transfers17 %>% filter(DESCRIPTION %in% descriptions_top5) %>% 
  group_by(DESCRIPTION, BOROUGH) %>% 
  summarise(n = n(), avg_amount = mean(`DOC. AMOUNT`, na.rm = TRUE)) %>%
  ggplot(aes(x = DESCRIPTION, y = avg_amount)) +
  geom_bar(aes(fill = DESCRIPTION), alpha = .4, stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = (seq(0,8000000,1000000)), labels = scales::dollar) +
  labs( x = "Property Type", y = "Average Sale Value ($)",
        title = "Highest value properties are Condo Units, \nwhich also have largest range of prices", 
        subtitle = "Average sale values for the top 5 property types sold in 2017", 
        caption = "Data Source: NYC Dept. of Finance, ACRIS", color = "Borough")
ggsave("propertyvalues_by_type.pdf")
