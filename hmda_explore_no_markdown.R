#exploring before cleaning and putting in markdown
#HMDA data

options("scipen"=100, "digits"=4)

hmda17_ny <- read_csv("https://api.consumerfinance.gov/data/hmda/slice/hmda_lar.csv?&$where=as_of_year=2017+AND+(msamd=%2235614%22)&$select=tract_to_msamd_income,rate_spread,population,minority_population,number_of_owner_occupied_units,number_of_1_to_4_family_units,loan_amount_000s,hud_median_family_income,applicant_income_000s,state_name,state_abbr,sequence_number,respondent_id,purchaser_type_name,property_type_name,preapproval_name,owner_occupancy_name,msamd_name,loan_type_name,loan_purpose_name,lien_status_name,hoepa_status_name,edit_status_name,denial_reason_name_3,denial_reason_name_2,denial_reason_name_1,county_name,co_applicant_sex_name,co_applicant_race_name_5,co_applicant_race_name_4,co_applicant_race_name_3,co_applicant_race_name_2,co_applicant_race_name_1,co_applicant_ethnicity_name,census_tract_number,as_of_year,application_date_indicator,applicant_sex_name,applicant_race_name_5,applicant_race_name_4,applicant_race_name_3,applicant_race_name_2,applicant_race_name_1,applicant_ethnicity_name,agency_name,agency_abbr,action_taken_name&$limit=0")
hmda17_ny <- hmda17_ny %>% filter(state_name == "New York")
write_csv(hmda17_ny, "hmda17_ny.csv")

hmda16_ny <- read_csv("https://api.consumerfinance.gov/data/hmda/slice/hmda_lar.csv?&$where=as_of_year=2016+AND+(msamd=%2235614%22)&$select=tract_to_msamd_income,rate_spread,population,minority_population,number_of_owner_occupied_units,number_of_1_to_4_family_units,loan_amount_000s,hud_median_family_income,applicant_income_000s,state_name,state_abbr,sequence_number,respondent_id,purchaser_type_name,property_type_name,preapproval_name,owner_occupancy_name,msamd_name,loan_type_name,loan_purpose_name,lien_status_name,hoepa_status_name,edit_status_name,denial_reason_name_3,denial_reason_name_2,denial_reason_name_1,county_name,co_applicant_sex_name,co_applicant_race_name_5,co_applicant_race_name_4,co_applicant_race_name_3,co_applicant_race_name_2,co_applicant_race_name_1,co_applicant_ethnicity_name,census_tract_number,as_of_year,application_date_indicator,applicant_sex_name,applicant_race_name_5,applicant_race_name_4,applicant_race_name_3,applicant_race_name_2,applicant_race_name_1,applicant_ethnicity_name,agency_name,agency_abbr,action_taken_name&$limit=0")
hmda16_ny <- hmda16_ny %>% filter(state_name == "New York")
write_csv(hmda16_ny, "hmda16_ny.csv")

hmda15_ny <- read_csv("https://api.consumerfinance.gov/data/hmda/slice/hmda_lar.csv?&$where=as_of_year=2015+AND+(msamd=%2235614%22)&$select=tract_to_msamd_income,rate_spread,population,minority_population,number_of_owner_occupied_units,number_of_1_to_4_family_units,loan_amount_000s,hud_median_family_income,applicant_income_000s,state_name,state_abbr,sequence_number,respondent_id,purchaser_type_name,property_type_name,preapproval_name,owner_occupancy_name,msamd_name,loan_type_name,loan_purpose_name,lien_status_name,hoepa_status_name,edit_status_name,denial_reason_name_3,denial_reason_name_2,denial_reason_name_1,county_name,co_applicant_sex_name,co_applicant_race_name_5,co_applicant_race_name_4,co_applicant_race_name_3,co_applicant_race_name_2,co_applicant_race_name_1,co_applicant_ethnicity_name,census_tract_number,as_of_year,application_date_indicator,applicant_sex_name,applicant_race_name_5,applicant_race_name_4,applicant_race_name_3,applicant_race_name_2,applicant_race_name_1,applicant_ethnicity_name,agency_name,agency_abbr,action_taken_name&$limit=0")
hmda15_ny <- hmda15_ny %>% filter(state_name == "New York")
write_csv(hmda15_ny, "hmda15_ny.csv")

hmda14_ny <- read_csv("https://api.consumerfinance.gov/data/hmda/slice/hmda_lar.csv?&$where=as_of_year=2014+AND+(msamd=%2235614%22)&$select=tract_to_msamd_income,rate_spread,population,minority_population,number_of_owner_occupied_units,number_of_1_to_4_family_units,loan_amount_000s,hud_median_family_income,applicant_income_000s,state_name,state_abbr,sequence_number,respondent_id,purchaser_type_name,property_type_name,preapproval_name,owner_occupancy_name,msamd_name,loan_type_name,loan_purpose_name,lien_status_name,hoepa_status_name,edit_status_name,denial_reason_name_3,denial_reason_name_2,denial_reason_name_1,county_name,co_applicant_sex_name,co_applicant_race_name_5,co_applicant_race_name_4,co_applicant_race_name_3,co_applicant_race_name_2,co_applicant_race_name_1,co_applicant_ethnicity_name,census_tract_number,as_of_year,application_date_indicator,applicant_sex_name,applicant_race_name_5,applicant_race_name_4,applicant_race_name_3,applicant_race_name_2,applicant_race_name_1,applicant_ethnicity_name,agency_name,agency_abbr,action_taken_name&$limit=0")
hmda14_ny <- hmda14_ny %>% filter(state_name == "New York")
write_csv(hmda14_ny, "hmda14_ny.csv")

hmda13_ny <- read_csv("https://api.consumerfinance.gov/data/hmda/slice/hmda_lar.csv?&$where=as_of_year=2013+AND+((state_code=36+AND+county_code+IN+(005,047,061,081,085)))&$select=tract_to_msamd_income,rate_spread,population,minority_population,number_of_owner_occupied_units,number_of_1_to_4_family_units,loan_amount_000s,hud_median_family_income,applicant_income_000s,state_name,state_abbr,sequence_number,respondent_id,purchaser_type_name,property_type_name,preapproval_name,owner_occupancy_name,msamd_name,loan_type_name,loan_purpose_name,lien_status_name,hoepa_status_name,edit_status_name,denial_reason_name_3,denial_reason_name_2,denial_reason_name_1,county_name,co_applicant_sex_name,co_applicant_race_name_5,co_applicant_race_name_4,co_applicant_race_name_3,co_applicant_race_name_2,co_applicant_race_name_1,co_applicant_ethnicity_name,census_tract_number,as_of_year,application_date_indicator,applicant_sex_name,applicant_race_name_5,applicant_race_name_4,applicant_race_name_3,applicant_race_name_2,applicant_race_name_1,applicant_ethnicity_name,agency_name,agency_abbr,action_taken_name&$limit=0")
hmda13_ny <- hmda13_ny %>% filter(state_name == "New York")
write_csv(hmda13_ny, "hmda13_ny.csv")

#they are now csv files
hmda13_ny <- read_csv("hmda13_ny.csv")
hmda14_ny <- read_csv("hmda14_ny.csv")
hmda15_ny <- read_csv("hmda15_ny.csv")
hmda16_ny <- read_csv("hmda16_ny.csv")
hmda17_ny <- read_csv("hmda17_ny.csv")

hmda_ny_3yrs <- rbind(hmda15_ny, hmda16_ny, hmda17_ny) %>%
  mutate(census_tract_number = gsub("\\.", "", census_tract_number))

hmda_ny_5yrs <- rbind(hmda13_ny, hmda14_ny, hmda15_ny, hmda16_ny, hmda17_ny) %>%
  mutate(census_tract_number = gsub("\\.", "", census_tract_number)) %>% 
  filter(county_name %in% c("Bronx County", "Kings County", "Queens County", "New York County", "Richmond County"))

#create a new column for approval/denial
hmda_ny_3yrs <- hmda_ny_3yrs %>% 
  mutate(outcome = ifelse(action_taken_name %in% 
                            c("Application approved but not accepted",
                              "Loan originated", "Loan purchased by the institution"),"Approved", 
                          ifelse(action_taken_name %in%
                                   c("Application denied by financial institution",
                                     "Preapproval request denied by financial institution"),
                                 "Denied", "Other")))

hmda_ny_5yrs <- hmda_ny_5yrs %>% 
  mutate(outcome = ifelse(action_taken_name %in% 
                            c("Application approved but not accepted",
                              "Loan originated", "Loan purchased by the institution"),"Approved", 
                          ifelse(action_taken_name %in%
                                   c("Application denied by financial institution",
                                     "Preapproval request denied by financial institution"),
                                 "Denied", "Other")))

#rename the values for applicant race
hmda_ny_3yrs$applicant_race_name_1[which(hmda_ny_3yrs$applicant_race_name_1 %in%
                                           c("Information not provided by applicant in mail, Internet, or telephone application",
                                             "Not applicable"))] <- "Unknown"
hmda_ny_5yrs$applicant_race_name_1[which(hmda_ny_5yrs$applicant_race_name_1 %in%
                                           c("Information not provided by applicant in mail, Internet, or telephone application",
                                             "Not applicable"))] <- "Unknown"

#regroup races
hmda_ny_3yrs <- hmda_ny_3yrs %>% mutate(race_alternative = applicant_race_name_1)
hmda_ny_3yrs$race_alternative[which(hmda_ny_3yrs$race_alternative %in% 
                                      c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander"))] <- "Other"
hmda_ny_5yrs <- hmda_ny_5yrs %>% mutate(race_alternative = applicant_race_name_1)
hmda_ny_5yrs$race_alternative[which(hmda_ny_5yrs$race_alternative %in% 
                                      c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander"))] <- "Other"


#dealing just with loan originations####
loan_originated_3yrs <- hmda_ny_3yrs %>% 
  filter(action_taken_name == "Loan originated")

#loan_purpose per year (number originated)
loan_originated_3yrs %>% 
  group_by(loan_purpose_name, as_of_year) %>% 
  summarise(n = n()) %>% 
  ggplot()+ geom_bar(aes(x = as_of_year, y = n, fill = loan_purpose_name),
                     stat = "identity", position = "dodge")

#loan_purpose per year, by rate of originated
loan_purpose_per_yr <- loan_originated_3yrs %>% 
  group_by(loan_purpose_name, as_of_year) %>% 
  summarise(n= n())
loan_orig_per_year <- loan_originated_3yrs %>% 
  group_by(as_of_year) %>% summarise(tot_year = n())

#percent of originated loans for each use by year
loan_purpose_per_yr %>% left_join(loan_orig_per_year, by = "as_of_year") %>% mutate(proportion = n/tot_year) %>% ggplot() + geom_bar(aes(x = as_of_year, y = proportion, fill = loan_purpose_name), stat = "identity", position = "dodge")


#percent of loans approved by race ####

counts_race <- hmda_ny_5yrs %>% 
  group_by(applicant_race_name_1) %>% 
  summarise(tot_apps_race = n())

hmda_ny_3yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race)

#outcome of application as a rate within race, graph
hmda_ny_3yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race) %>% filter(outcome == "Approved") %>% ggplot() + geom_bar(aes(x = applicant_race_name_1, y = rate), stat = "identity")


#approval rates by race by census tract
approval_counts_by_race <- hmda_ny_5yrs %>% 
  filter(outcome == "Approved") %>% 
  group_by(census_tract_number, applicant_race_name_1) %>% 
  summarise(total_approved = n())

df_for_chloropleth_race_approval <- hmda_ny_5yrs %>% 
  group_by(census_tract_number, applicant_race_name_1) %>% 
  summarise(total_apps = n()) %>% 
  left_join(approval_counts_by_race, 
            by = c("census_tract_number", "applicant_race_name_1")) %>%
  mutate(approval_rate = total_approved/total_apps)

df_for_chloropleth_black_approval <- hmda_ny_5yrs %>% 
  group_by(census_tract_number, applicant_race_name_1) %>% 
  summarise(total_apps = n()) %>% 
  left_join(approval_counts_by_race, 
            by = c("census_tract_number", "applicant_race_name_1")) %>%
  mutate(approval_rate = total_approved/total_apps) %>%
  filter(applicant_race_name_1== "Black or African American")

df_for_chloropleth_black_approval <- df_for_chloropleth_black_approval %>% 
  mutate(approval_rate_round = round(approval_rate, 2))

#upload shapefiles
census_tracts <- rgdal::readOGR("/Volumes/SPACESHIP/data_viz/data_scripts/visualizing-nyc-housing/2010_Census_Tracts", "geo_export_6e8e8432-67b3-4029-a002-fa59bbd3d5d3")
plot(census_tracts)

census_tract_merged_black <- sp::merge(x = census_tracts,
                                       y = df_for_chloropleth_black_approval,
                                       by.x ="ct2010", by.y= "census_tract_number", all.x = TRUE)

#dealing with ggplot
#tidy the spatial dataframe, make it into a dataframe
census_tracts_merged_black_df <- tidy(census_tract_merged_black)
#need to re-ad datat to it, by create a category it can merge with our data
census_tract_merged_black$polyID <- sapply(slot(census_tract_merged_black, "polygons"),
                               function(x) slot(x, "ID"))
#merge back with our data
census_tracts_merged_black_df <- merge(census_tracts_merged_black_df, 
                          census_tract_merged_black, 
                          by.x = "id", by.y="polyID")

census_tracts_merged_black_df$breaks <- cut(census_tracts_merged_black_df$approval_rate, c(0,.25,.5,.75,1)) 

#plot- black loan approval rates map ####
ggplot() + 
  geom_polygon(data = census_tracts_merged_black_df,
               aes(x = long, y = lat, group = group, fill = breaks)) + 
  scale_fill_brewer("Approval Rate", palette = "OrRd") +
  labs(title = "Highest loan approval rates for Blacks are in \nStaten Island and Queens", 
       subtitle = "When compared to approval rates that do not consider race, \nmost census tracts have lower approval rates for Blacks", 
       caption = "Home Mortgage Disclosure Act Data, CFPB") + 
  theme(line = element_blank(),                          
        axis.text=element_blank(),                      
        axis.title=element_blank(),                      
        panel.background = element_blank())

#doing same for white
df_for_chloropleth_white_approval <- hmda_ny_3yrs %>% 
  group_by(census_tract_number, applicant_race_name_1) %>% 
  summarise(total_apps = n()) %>% 
  left_join(approval_counts_by_race, 
            by = c("census_tract_number", "applicant_race_name_1")) %>%
  mutate(approval_rate = total_approved/total_apps) %>%
  filter(applicant_race_name_1== "White")

census_tract_merged_white <- sp::merge(x = census_tracts,
                                       y = df_for_chloropleth_white_approval,
                                       by.x ="ct2010", by.y= "census_tract_number", all.x = TRUE)

#dealing with ggplot
#tidy the spatial dataframe, make it into a dataframe
census_tracts_merged_white_df <- tidy(census_tract_merged_white)
#need to re-ad datat to it, by create a category it can merge with our data
census_tract_merged_white$polyID <- sapply(slot(census_tract_merged_white, "polygons"),
                                           function(x) slot(x, "ID"))
#merge back with our data
census_tracts_merged_white_df <- merge(census_tracts_merged_white_df, 
                                       census_tract_merged_white, 
                                       by.x = "id", by.y="polyID")

census_tracts_merged_white_df$breaks <- cut(census_tracts_merged_white_df$approval_rate, c(0, .25, .5, .75, 1)) 

#plot
ggplot() + 
  geom_polygon(data = census_tracts_merged_white_df,
               aes(x = long, y = lat, group = group, fill = breaks)) + 
  scale_fill_brewer("Approval Rate", palette = "OrRd") +
  labs(title = "Highest loan approval rates for Whites are \nspread across NYC", 
       subtitle = "Loan approval rates by census tract for combined years 2015, 2016, 2017", 
       caption = "Home Mortgage Disclosure Act Data, CFPB") + 
  theme(line = element_blank(),                          
        axis.text=element_blank(),                      
        axis.title=element_blank(),                      
        panel.background = element_blank())

#approval rates without race

#number of loans apps by census tract
apps_by_tract <- hmda_ny_5yrs %>% 
  group_by(census_tract_number) %>% 
  summarise(total_apps = n())

#approvals by tract
approval_counts_by_tract <- hmda_ny_5yrs %>% 
  filter(outcome == "Approved") %>% 
  group_by(census_tract_number) %>% 
  summarise(total_approved = n())

approval_rates_by_tract <- 
  approval_counts_by_tract %>% 
  left_join(apps_by_tract, by = "census_tract_number") %>% 
  mutate(approval_rate = total_approved/total_apps)

census_tract_merged <- sp::merge(x = census_tracts,
                                       y = approval_rates_by_tract,
                                       by.x ="ct2010", by.y= "census_tract_number", all.x = TRUE)

#dealing with ggplot
#tidy the spatial dataframe, make it into a dataframe
census_tracts_merged_df <- tidy(census_tract_merged)
#need to re-ad datat to it, by create a category it can merge with our data
census_tract_merged$polyID <- sapply(slot(census_tract_merged, "polygons"),
                                           function(x) slot(x, "ID"))
#merge back with our data
census_tracts_merged_df <- merge(census_tracts_merged_df, 
                                       census_tract_merged, 
                                       by.x = "id", by.y="polyID")

census_tracts_merged_df$breaks <- cut(census_tracts_merged_df$approval_rate, c(0, .25, .5, .75, 1)) 

#plot- approval rates by census block #### 
ggplot() + 
  geom_polygon(data = census_tracts_merged_df,
               aes(x = long, y = lat, group = group, fill = breaks)) + 
  scale_fill_brewer("Approval Rate", palette = "OrRd") +
  labs(title = "Very few census tracts in New York City have average loan approval rates above 75%", 
       subtitle = "Most census tracts have approval rates between 50% and 75%", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017") + 
  theme(line = element_blank(),                          
        axis.text=element_blank(),                      
        axis.title=element_blank(),                      
        panel.background = element_blank())

#looking at average loan amounts 
hmda_ny_3yrs <- hmda_ny_3yrs %>% 
  mutate(applicant_income_000s = as.numeric(applicant_income_000s))


#mean income by race, with outcome [this doesnt say anything interesting]
hmda_ny_3yrs %>% group_by(race_alternative, outcome) %>% 
     summarise(mean_applicant_income_000s = mean(applicant_income_000s, na.rm = TRUE)) %>%
     ggplot() + 
     geom_point(aes(x = outcome, y = mean_applicant_income_000s, color = race_alternative))

#is there a correlation between income, amount, and outcome? [this doesnt say anything interesting]
hmda_ny_3yrs %>% filter(!is.na(applicant_income_000s) & !is.na(loan_amount_000s)) %>% 
  ggplot(aes(x = applicant_income_000s, y = loan_amount_000s, color = outcome)) +
  geom_point() + geom_smooth()

#reasons for denial (I want rates per type not numbers)
num_denied_by_race <- hmda_ny_5yrs %>% 
  filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>%
  group_by(applicant_race_name_1) %>% 
  summarise(tot_apps_denied = n())

denial_reasons_race <- hmda_ny_5yrs %>% 
  filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>% 
  group_by(applicant_race_name_1, denial_reason_name_1) %>% 
  summarise(denied_by_reason = n())

#denial rates for reasons and by race
denial_reasons_race %>% 
  left_join(num_denied_by_race, by = "applicant_race_name_1") %>%
  mutate(denial_rate = denied_by_reason/tot_apps_denied) %>% 
  ggplot() + 
  geom_point(aes(x = applicant_race_name_1, y = denial_rate, color = denial_reason_name_1)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) + 
  labs(title = "The most frequent reason Whites are denied loans is \ninadequate debt-to-income ratios, \nwhile Blacks are denied most frequently for credit history", x = "Applicant Race", y = "Denial Rate", 
       subtitle = "Rates of denial for each racial group, by reason for denial", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017")

#denial reasons rates race ####
denial_reasons_race %>% 
  left_join(num_denied_by_race, by = "applicant_race_name_1") %>%
  mutate(denial_rate = denied_by_reason/tot_apps_denied) %>% 
  ggplot() + 
  geom_bar(aes(x = applicant_race_name_1, 
               y = denial_rate, 
               fill = denial_reason_name_1),
           stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Credit History and Debt-to-Income Ratio are the two most frequent loan denial reasons", 
       subtitle = "However, the distribution of reasons differ significantly by race", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", 
       x = "Applicant Race", y = "Denial Rate") + scale_fill_discrete(name = "Denial Reason")

#income, approval, denial [doesnt seem to say anything interesting]
hmda_ny_5yrs %>% 
  group_by(race_alternative, outcome) %>% 
  summarise(avg_income = mean(applicant_income_000s, na.rm = TRUE)) %>% 
  ggplot() + geom_bar(aes(x = race_alternative, y = avg_income, fill = outcome), stat = "identity")

#denial rates over time by racial group, has it changed betwen 2015-2017
#need to amke this rates 
apps_per_year_outcome_race <- hmda_ny_5yrs %>% group_by(as_of_year, race_alternative, outcome) %>%
  summarise(apps_per_year_outcome_race = n())

apps_per_year_outcome <- hmda_ny_5yrs %>% group_by(as_of_year, race_alternative) %>% 
  summarise(apps_per_year_outcome = n())

#approval rates over the five years ####
approval_rate_year_race <- apps_per_year_outcome_race %>% 
  left_join(apps_per_year_outcome, 
            by = c("as_of_year", "race_alternative")) %>% 
  mutate(outcome_rate = apps_per_year_outcome_race/apps_per_year_outcome) %>%
  filter(outcome == "Approved") 

ggplot(approval_rate_year_race, 
       aes(x = as_of_year, y = outcome_rate, color = race_alternative)) +
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017)) + 
  labs(title = "Loan approval rates for Blacks are consistently below Whites and Asians", 
       subtitle = "Despite a sharp decrease for American Indians in 2016, \nloan approval rates have remained relatively consistent over the last 5 years", 
       x = "Year", y = "Approval Rate", caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017") + 
  geom_text_repel(data = filter(approval_rate_year_race, as_of_year == 2017),
                  aes(x = 2017, label=race_alternative)) + 
  theme(legend.position = "none")

#top reasons for denial by borough [doesnt really say anything interesting]
county_denial_reasons <- hmda_ny_5yrs %>% 
  filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>%
  group_by(county_name, denial_reason_name_1) %>% 
  summarise(count_per_reason = n())

county_total_denials <- hmda_ny_5yrs %>% 
  filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>% 
  group_by(county_name) %>% summarise(tot_denied = n())

county_denial_reasons %>% 
  left_join(county_total_denials, by = "county_name") %>% 
  mutate(denial_rate = count_per_reason/tot_denied) %>% 
  ggplot() + 
  geom_bar(aes(x = county_name, y = denial_rate, fill = denial_reason_name_1), stat = "identity")

#denial reasons over time
denial_reasons_year <- hmda_ny_5yrs %>% filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>% 
  group_by(as_of_year, denial_reason_name_1) %>% summarise(count_per_reason = n())

denial_count_year <- hmda_ny_5yrs %>% filter(outcome == "Denied", !is.na(denial_reason_name_1)) %>% 
  group_by(as_of_year) %>% summarise(tot_denied = n())

#denial reasons changing over time ####
denial_reasons_year %>% 
  left_join(denial_count_year, by = "as_of_year") %>% 
  mutate(denial_rate = count_per_reason/tot_denied) %>% 
  ggplot(aes(x = as_of_year, y = denial_rate, color = denial_reason_name_1)) + 
  geom_point() + geom_line() + labs(title = "Debt-to-Income Ratio is consistently the most frequent reason for denial", 
                                    subtitle = "Insufficient Collateral and Incomplete Applications have been decreasing as denial reasons \n while Credit History-related denials have been increasing", 
                                    caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", 
                                    x = "Year", y = "Denial Rate", color = "Denial Reason") + scale_fill_discrete(name = "Denial Reason")

#something about income and approval rate


#circle packing for approval and denial by race for each year 
install.packages("packcircles")
library(packcircles)

data <- data.frame(group = paste("Group", letters[1:20]), value = sample(seq(1,100), 20))
packing <- circleProgressiveLayout(data$value, sizetype = 'area')
data <- cbind(data, packing)
plot(data$radius, data$value)
dat.gg <- circleLayoutVertices(packing, npoints = 50)
ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id), color = "black", alpha = .6)) +
  geom_text(data = data, aes(x,y,size = value, label = group)) + 
  scale_size_continuous(range = c(1,4))



outcome_race_rate <- hmda_ny_5yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race)

#these are rates that do not add up to 1, 
#because they are the number of approved apps/number of total apps per race
approved_race_rate <- hmda_ny_5yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race) %>% filter(outcome == "Approved")

approved_race_rate <- as.data.frame(approved_race_rate)
packing <- circleProgressiveLayout(approved_race_rate$rate, sizetype = 'area')
approved_race_rate <- cbind(approved_race_rate, packing)
dat.gg <- circleLayoutVertices(packing, npoints =50 )
dat.gg$id[which(dat.gg$id == 1)] <- "American Indian or Alaska Native"
dat.gg$id[which(dat.gg$id == 2)] <- "Asian"
dat.gg$id[which(dat.gg$id == 3)] <- "Black or African American"
dat.gg$id[which(dat.gg$id == 4)] <- "Native Hawaiian or Other Pacific Islander"
dat.gg$id[which(dat.gg$id == 5)] <- "Unknown"
dat.gg$id[which(dat.gg$id == 6)] <- "White"

ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id)), color = "black") +
  geom_text_repel(data = approved_race_rate, aes(x,y,label = applicant_race_name_1)) + 
  scale_size_continuous(range = c(1,4)) + labs(title = "Denied") + 
  labs(title = "White and Asian applicants receive higher rates of loan approval",
       subtitle = "Areas represent the percent of loan application approved within racial groups", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", fill = "Race") + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

denied_race_rate <- hmda_ny_5yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race) %>% filter(outcome == "Denied")

denied_race_rate <- as.data.frame(denied_race_rate)
packing <- circleProgressiveLayout(denied_race_rate$rate, sizetype = 'area')
denied_race_rate <- cbind(denied_race_rate, packing)
dat.gg <- circleLayoutVertices(packing, npoints =50 )
dat.gg$id[which(dat.gg$id == 1)] <- "American Indian or Alaska Native"
dat.gg$id[which(dat.gg$id == 2)] <- "Asian"
dat.gg$id[which(dat.gg$id == 3)] <- "Black or African American"
dat.gg$id[which(dat.gg$id == 4)] <- "Native Hawaiian or Other Pacific Islander"
dat.gg$id[which(dat.gg$id == 5)] <- "Unknown"
dat.gg$id[which(dat.gg$id == 6)] <- "White"

ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id)), color = "black") +
  geom_text_repel(data = denied_race_rate, aes(x,y,label = applicant_race_name_1)) + 
  scale_size_continuous(range = c(1,4)) +
  labs(title = "Historically Disadvantaged Racial Groups Have Higher Rates of Denial",
       subtitle = "Areas represent the percent of loan application denied within racial groups", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", fill = "Race") + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

#[doesnt say anything interesting, just shows us populations]
#approved, what percent of approvals are black, white, etc, BUT this will just reflect who applies?
hmda_ny_5yrs %>% filter(outcome == "Approved") %>% nrow()

percent_of_approvals_by_race <- hmda_ny_5yrs %>% 
  filter(outcome == "Approved") %>% 
  group_by(applicant_race_name_1) %>% 
  summarise(n = n()) %>% mutate(rate = n/513733)

percent_of_approvals_by_race <- as.data.frame(percent_of_approvals_by_race)
packing <- circleProgressiveLayout(percent_of_approvals_by_race$rate, sizetype = 'area')
percent_of_approvals_by_race <- cbind(percent_of_approvals_by_race, packing)
dat.gg <- circleLayoutVertices(packing, npoints =50 )
dat.gg$id[which(dat.gg$id == 1)] <- "American Indian or Alaska Native"
dat.gg$id[which(dat.gg$id == 2)] <- "Asian"
dat.gg$id[which(dat.gg$id == 3)] <- "Black or African American"
dat.gg$id[which(dat.gg$id == 4)] <- "Native Hawaiian or Other Pacific Islander"
dat.gg$id[which(dat.gg$id == 5)] <- "Unknown"
dat.gg$id[which(dat.gg$id == 6)] <- "White"

ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id)), color = "black") +
  geom_text_repel(data = percent_of_approvals_by_race, 
                  aes(x,y,label = applicant_race_name_1)) + 
  scale_size_continuous(range = c(1,4)) + 
  labs(title= "Who gets approved for a loan?", 
          subtitle= "Of all applicants who apply, Whites, Asians, \nand people whose race is not known receive more approvals",
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", fill = "Race") + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

#denied [doesnt tell us anything interesting, looks the same as approved]
hmda_ny_5yrs %>% filter(outcome == "Denied") %>% nrow()

percent_of_denials_by_race <- hmda_ny_5yrs %>% 
  filter(outcome == "Denied") %>% 
  group_by(applicant_race_name_1) %>% 
  summarise(n = n()) %>% mutate(rate = n/148349)

percent_of_denials_by_race <- as.data.frame(percent_of_denials_by_race)
packing <- circleProgressiveLayout(percent_of_denials_by_race$rate, sizetype = 'area')
percent_of_denials_by_race <- cbind(percent_of_denials_by_race, packing)
dat.gg <- circleLayoutVertices(packing, npoints =50 )
dat.gg$id[which(dat.gg$id == 1)] <- "American Indian or Alaska Native"
dat.gg$id[which(dat.gg$id == 2)] <- "Asian"
dat.gg$id[which(dat.gg$id == 3)] <- "Black or African American"
dat.gg$id[which(dat.gg$id == 4)] <- "Native Hawaiian or Other Pacific Islander"
dat.gg$id[which(dat.gg$id == 5)] <- "Unknown"
dat.gg$id[which(dat.gg$id == 6)] <- "White"

ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id)), color = "black") +
  geom_text_repel(data = percent_of_denials_by_race, 
                  aes(x,y,label = applicant_race_name_1)) + 
  scale_size_continuous(range = c(1,4)) + 
  labs(title= "Who gets denied for a loan?", 
       subtitle= "Of all applicants who apply, Whites, Asians, \nand people whose race is not known receive more approvals",
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", fill = "Race") + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

#circle packing with 2 levels 
library(ggraph)
library(igraph)

counts_race <- hmda_ny_5yrs %>% 
  group_by(applicant_race_name_1) %>% 
  summarise(tot_apps_race = n())

outcome_race_rate <- hmda_ny_5yrs %>% 
  group_by(outcome, applicant_race_name_1) %>%
  summarise(n = n()) %>%
  left_join(counts_race, by = "applicant_race_name_1") %>% 
  mutate(rate= n/tot_apps_race)

outcome_race_rate <- as.data.frame(outcome_race_rate)
test1 <- as.data.frame(outcome_race_rate %>% select(outcome, applicant_race_name_1))
test2<- as.data.frame(outcome_race_rate %>% select(applicant_race_name_1, rate))

mygraph <- graph_from_data_frame(test1, vertices = test2)


#trying to facetwrap 
outcome_race_rate <- as.data.frame(outcome_race_rate)
packing <- circleProgressiveLayout(outcome_race_rate$rate, sizetype = 'area')
outcome_race_rate <- cbind(outcome_race_rate, packing)
dat.gg <- circleLayoutVertices(packing, npoints =50 )
dat.gg$id[which(dat.gg$id == 1)] <- "American Indian or Alaska Native"
dat.gg$id[which(dat.gg$id == 2)] <- "Asian"
dat.gg$id[which(dat.gg$id == 3)] <- "Black or African American"
dat.gg$id[which(dat.gg$id == 4)] <- "Native Hawaiian or Other Pacific Islander"
dat.gg$id[which(dat.gg$id == 5)] <- "Unknown"
dat.gg$id[which(dat.gg$id == 6)] <- "White"

ggplot() + 
  geom_polygon(data = dat.gg, aes(x,y, group = id, fill = as.factor(id)), color = "black") +
  geom_text_repel(data = outcome_race_rate, 
                  aes(x,y,label = applicant_race_name_1)) + 
  scale_size_continuous(range = c(1,4)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())








#overlapping histogram of loan amounts (need to winsorize or something) ####
library(statar)
hmda_ny_5yrs <- hmda_ny_5yrs %>% 
  mutate(win_loan_amount = winsorize(loan_amount_000s, probs = c(.01, .99)))

hmda_ny_5yrs <- hmda_ny_5yrs %>% 
  mutate(win_income = winsorize(applicant_income_000s, probs = c(.01, .99)))

ggplot(hmda_ny_5yrs, aes(x = win_loan_amount, fill = county_name)) +
  geom_histogram(alpha = .5, bins = 100) + 
  scale_x_continuous(breaks= c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)) +
  labs(title = "Most loans applications are made for under $1 million, \n and most of those made over $1 million are in New York County or Kings County", 
       subtitle = "The distribution of loan application amounts by county \nshows large spikes around $400,000", 
       caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017", 
       fill = "County", x = "Loan amount (in $100s)", y = "Number of Loan Applications")

ggplot(hmda_ny_5yrs, aes(x = win_loan_amount, fill = as.factor(as_of_year))) +
  geom_histogram(alpha = .5, bins = 100)

ggplot(hmda_ny_5yrs, aes(x = win_loan_amount, fill = applicant_sex_name)) + 
    geom_histogram(alpha = .5, bins = 100)

ggplot(hmda_ny_5yrs, aes(x = win_loan_amount, y = win_income)) + 
  geom_jitter() + facet_wrap(hmda_ny_5yrs$outcome)

#looking at differences in approval rates by male/female
ggplot(filter(hmda_ny_5yrs, applicant_sex_name %in% c("Female","Male")),
       aes(y=win_income, x=applicant_sex_name, fill=outcome), alpha = .3) +
  geom_boxplot() + scale_y_log10(expand=c(0,0)) + 
  scale_fill_manual(values = c('#1f8a70', '#E8291A', '#FFE11A')) + 
  theme(text = element_text(family = "Meiryo"), 
         panel.background = element_rect(fill = '#F2F5F2')) +
  labs(fill = "Outcome", x = "Gender of Applicant", y = "Income", 
       title = "There is a difference in the median income of men and women who are approved for loans", 
       subtitle = "While approval relies on other factors, the distribution \nof incomes within outcomes differs between genders reflects systemic inequality in approvals or loans, incomes, or both.")

#looking at approval rates (should these ce rates or numbers?) for different income levels and races 
#make quantiles 
summary(hmda_ny_5yrs$applicant_income_000s)

hmda_ny_5yrs <-  hmda_ny_5yrs %>% mutate(income_bins = ifelse(applicant_income_000s %in% c(1:73), "Q1", 
                                             ifelse(applicant_income_000s %in% c(74:108), "Q2", 
                                                    ifelse(applicant_income_000s %in% c(109:180), "Q3", 
                                                           ifelse(applicant_income_000s %in% c(181:133549), "Q4", NA)))))

#for people in first quartile of income, need rate 

#total loans per quartile, year, race
quartile_race_year <- hmda_ny_5yrs %>% 
  group_by(income_bins, race_alternative, as_of_year, county_name) %>% 
  summarise(total = n())

quartile_race_year_outcome <- hmda_ny_5yrs %>% 
  group_by(income_bins, race_alternative, outcome, as_of_year, county_name) %>% 
  summarise(per_outcome = n())


#outcome rates for blacks in the first earnings quartile in brooklyn
quartile_race_year_outcome %>% 
  left_join(quartile_race_year, 
            by = c("income_bins", "race_alternative", "as_of_year", "county_name")) %>% 
  mutate(outcome_rate = per_outcome/total) %>%
  filter(income_bins == "Q1", race_alternative == "Black or African American", county_name == "Kings County") %>% 
  ggplot() + geom_line(aes(x = as_of_year, y = outcome_rate, color = outcome))

#outcome rates for blacks in the first earnings quartile in brooklyn####
quartile_race_year_outcome %>% 
  left_join(quartile_race_year, 
            by = c("income_bins", "race_alternative", "as_of_year", "county_name")) %>% 
  mutate(outcome_rate = per_outcome/total) %>% filter(race_alternative %in% c("Asian", "White", "Black or African American", "Other"), county_name == "Kings County") %>% 
  ggplot() + geom_line(aes(x = as_of_year, y = outcome_rate, color = outcome)) + 
  facet_wrap(race_alternative~income_bins) + scale_color_manual(values = c('#1f8a70', '#E8291A', '#FFE11A'))

#line plot of total apps by year for each census tract
hmda_ny_5yrs %>% 
  filter(income_bins == "Income Quartile 1") %>% 
  group_by(census_tract_number, as_of_year) %>% 
  summarise(tot_apps = n()) %>% ggplot() + 
  geom_line(aes(x = as_of_year, y = tot_apps, color = census_tract_number)) + 
  guides(color = FALSE)

#number of apps per year faceted by income quartiles
hmda_ny_5yrs %>% 
  group_by(census_tract_number, as_of_year, income_bins) %>% 
  summarise(tot_apps = n()) %>% 
  ggplot() + geom_line(aes(x = as_of_year, y = tot_apps, color = census_tract_number)) + facet_grid(~income_bins) + guides(color = FALSE)

#random line plot that looks at changes in total number of apps per nta (need to add labels to top ones)
hmda_ny_5yrs %>% 
  left_join(census_tracts_bk, by = c("census_tract_number" = "ct2010")) %>% 
  group_by(ntaname, as_of_year) %>% 
  summarise(n = n()) %>% 
  filter(!is.na(ntaname)) %>% 
  ggplot() + geom_line(aes(x = as_of_year, y = n, color = ntaname)) + 
  guides(color = FALSE) + geom_text(aes(label = ntaname))


#symbol map of change in number of applications per census tract
#using census tracts, since we will only have centroids for census tracts
num_apps_year <- hmda_ny_5yrs %>% group_by(census_tract_number, as_of_year) %>% summarise(tot_apps = n()) %>% filter(as_of_year %in% c(2013, 2017))

num_apps_year$prior <- lag(num_apps_year$tot_apps)
num_apps_year$change <-as.numeric(num_apps_year$tot_apps - num_apps_year$prior)

census_tracts_bk <- census_tracts %>% 
  filter(boro_name == "Brooklyn")
census_tracts_bk$centroids <- st_centroid(census_tracts_bk$geometry)
census_tracts_bk <- census_tracts_bk %>% 
  mutate(lon = substr(centroids, 3, 18), lat = substr(centroids, 21,36))

num_apps_year <- num_apps_year %>% filter(as_of_year== 2017) %>% 
  right_join(census_tracts_bk, by = c("census_tract_number" = "ct2010"))

ggplot() + geom_sf(data = census_tracts_bk,fill = "grey", color = "white") + geom_point(data = num_apps_year, aes(x = lon, y = lat, size = change), alpha = .2)

#attempt at symbol map of income to loan ratio
census_tracts_trans <- st_transform(census_tracts, 26918)

census_tracts_trans$centroids <- st_centroid(census_tracts_trans$geometry)

test2 <- hmda_ny_5yrs %>% filter(as_of_year == 2017, co_applicant_ethnicity_name == "No co-applicant") %>% 
  group_by(census_tract_number) %>% 
  summarise(mean_loan_income_ratio = 
              mean(win_loan_income_ratio, na.rm = TRUE)) %>%
  left_join(census_tracts_trans, by = c("census_tract_number" = "ct2010"))

#need to split centroids into lat/lon
test2 <- test2 %>% 
  mutate(lon = substr(centroids, 3, 18), lat = substr(centroids, 21,36))

ggplot(test) + 
  geom_sf() + geom_point(data = test2, aes(x = lon, y = lat, size = mean_loan_income_ratio), alpha = .2)
#sort of works 


#map of loan approval rates
df_for_chloropleth_black_approval <- hmda_ny_5yrs %>% 
  group_by(census_tract_number, applicant_race_name_1) %>% 
  summarise(total_apps = n()) %>% 
  left_join(approval_counts_by_race, 
            by = c("census_tract_number", "applicant_race_name_1")) %>%
  mutate(approval_rate = total_approved/total_apps) %>%
  filter(applicant_race_name_1== "Black or African American")

df_for_chloropleth_black_approval <- df_for_chloropleth_black_approval %>% 
  mutate(approval_rate_round = round(approval_rate, 2))

#upload shapefiles
census_tracts <- rgdal::readOGR("/Volumes/SPACESHIP/data_viz/data_scripts/visualizing-nyc-housing/2010_Census_Tracts", "geo_export_6e8e8432-67b3-4029-a002-fa59bbd3d5d3")

census_tract_merged_black <- sp::merge(x = census_tracts,
                                       y = df_for_chloropleth_black_approval,
                                       by.x ="ct2010", by.y= "census_tract_number", all.x = TRUE)

#dealing with ggplot
#tidy the spatial dataframe, make it into a dataframe
census_tracts_merged_black_df <- tidy(census_tract_merged_black)
#need to re-ad datat to it, by create a category it can merge with our data
census_tract_merged_black$polyID <- sapply(slot(census_tract_merged_black, "polygons"),
                                           function(x) slot(x, "ID"))
#merge back with our data
census_tracts_merged_black_df <- merge(census_tracts_merged_black_df, 
                                       census_tract_merged_black, 
                                       by.x = "id", by.y="polyID")

census_tracts_merged_black_df$breaks <- cut(census_tracts_merged_black_df$approval_rate, c(0, .2, .4, .6, .8, 1))

#plot- black loan approval rates map ####
ggplot() + 
  geom_polygon(data = census_tracts_merged_black_df,
               aes(x = long, y = lat, group = group, fill = breaks)) + 
  scale_fill_brewer("Approval Rate", palette = "OrRd") +
  labs(title = "Highest loan approval rates for Blacks are in \nStaten Island and Queens", 
       subtitle = "When compared to approval rates that do not consider race, \nmost census tracts have lower approval rates for Blacks", 
       caption = "Home Mortgage Disclosure Act Data, CFPB") + 
  theme(line = element_blank(),                          
        axis.text=element_blank(),                      
        axis.title=element_blank(),                      
        panel.background = element_blank(), 
        text = element_text(family = "Meiryo")) + guides(fill=guide_legend(title="Approval Rate")) + scale_fill_manual(values = c('#ffffd4', '#fdd2c0', '#f3a683', '#e27a48', '#cc4c02'))


#slopegraph of NTA area approval rates

outcome_tot_nta <- hmda_ny_5yrs %>% filter(startsWith(ntacode, "BK")) %>%
  group_by(ntaname, as_of_year, outcome) %>% summarise(apps = n())

tot_apps_nta <- hmda_ny_5yrs %>%filter(startsWith(ntacode, "BK")) %>% 
  group_by(ntaname, as_of_year) %>% summarise(totals = n())

tot_apps_nta <- outcome_tot_nta %>% 
  left_join(tot_apps_nta, by = c("ntaname", "as_of_year")) %>% 
  mutate(rates = apps/totals) %>% 
  filter(as_of_year %in% c(2013, 2017), outcome == "Approved")

tot_apps_nta$prior <- lag(tot_apps_nta$rates)
tot_apps_nta$change <- tot_apps_nta$rates - tot_apps_nta$prior

tot_apps_nta %>% 
  ggplot(aes(x = as_of_year, y = rates, group = ntaname)) + 
  geom_line(aes(color = ntaname)) + 
  geom_point(aes(color = ntaname)) + 
  theme(legend.position = "none") + 
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) + 
  geom_text_repel(data = tot_apps_nta %>% filter(as_of_year == 2017, change > .05 | change < -.05), 
          aes(label = ntaname), 
            hjust = 1.35, fontface = "bold", size = 4)
  
#violin plot of incomes and gender -trying to add summary statistics 
violin_data <- filter(hmda_ny_5yrs, applicant_sex_name %in% c("Female","Male"))

ggplot(data = violin_data) +
  geom_violin(aes(y=win_income, x=applicant_sex_name, fill=outcome)) + 
  scale_y_log10(expand=c(0,0)) + 
  scale_fill_manual(values = c('#1f8a70', '#E8291A', '#FFE11A')) + 
  theme(text = element_text(family = "Meiryo"), 
        panel.background = element_rect(fill = '#F2F5F2'),
        panel.grid.major = element_line(color = "#404040"), 
        panel.grid.minor = element_blank()) +
  labs(fill = "Outcome", x = "Gender of Applicant", 
       y = "Income (in $1,000s, Log Scale)", 
       title = "On Average, Male Applicants have Higher Incomes \nthan Female Applicants", 
       subtitle = "Income by Gender, grouped by Outcome", caption = "Home Mortgage Disclosure Act Data, CFPB, 2013-2017") 

#trying with treemapify
data_tree <- hmda_ny_5yrs %>% group_by(boro, race_alternative) %>% 
  summarise(n = n())
treemap_coords <- treemapify(data_tree, area = "n", subgroup2 = "race_alternative", subgroup = "boro")
ggplot(treemap_coords)

ggplot(hmda_ny_5yrs, aes(area =count, subgroup = boro, label = race_alternative)) +
  geom_treemap()
