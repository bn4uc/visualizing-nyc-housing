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
  mutate(census_tract_number = gsub("\\.", "", census_tract_number))

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

counts_race <- hmda_ny_3yrs %>% 
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
