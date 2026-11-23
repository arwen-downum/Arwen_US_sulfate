############################################################
# LAGOS-US: Environmental Drivers of Sulfate Concentrations
# Author: 
# Last Updated:
############################################################

#### 1. Project Setup ----
# install Libraries and load packages
install.packages("data.table")
library(data.table)
install.packages("tidydr")
library(tidyr)
library(dplyr)


# Uploading and creating data set from LAGOS US data

# all data can be found in https://lagoslakes.org/lagos-research-platforms/lagos-us/


# uploading US LAGOS LIMNO data

chemistry_limno<- fread(file.choose())

View(chemistry_limno)

# find the parameter id that corresponds with parameter sulfate

names(chemistry_limno)

chemistry_limno %>%
  distinct(parameter_id, parameter_name) %>%
  arrange(parameter_id)


chemistry_limno %>%
  filter(grepl("so4_mgl", parameter_name, ignore.case = TRUE)) %>%
  distinct(parameter_id, parameter_name)

#filter for sulfate only parameter

sulfate_data<- chemistry_limno[parameter_id==34]
View(sulfate_data)

# pivot from long to wide format
Sulfate_wide <- pivot_wider(sulfate_data,
                            names_from = parameter_name,
                            values_from = parameter_value)
View(Sulfate_wide)


depth<-Sulfate_wide %>%
  count(sample_depth_m)

View(depth)

years_counts<- Sulfate_wide%>%
  mutate(year = year(as.Date(sample_date))) %>%
  count(year)
View(years_counts)


###### Filter by year######

library(lubridate)

Sulfate_2017_data<-Sulfate_wide%>%
  mutate(sample_date=as.Date(sample_date))%>%
  filter(year(sample_date)==2017)
View(Sulfate_2017_data)


Counts_2017 <- Sulfate_2017_data%>%
  count(sample_depth_m)

View(Counts_2017)

install.packages("writexl")
library(writexl)

write_xlsx(Sulfate_2017_data, "Sulfate_2017_data.xlsx")
getwd()



write_xlsx(years_counts, "years_counts.xlsx")


max_depth<- max(Sulfate_2017_data$sample_depth_m)

top_25_depths <- subset(Sulfate_2017_data,sample_depth_m<= 0.25*max_depth)

View(top_25_depths)

write_xlsx(top_25_depths, "top_25_depths.xlsx")
getwd()


#######Wide_Getting data for trophic data sets#######

Chla_a_data<- Chemistry_limo[parameter_id==9]

View(Chla_a_data)

Wide_chla_a<- pivot_wider(Chla_a_data,
                          names_from = parameter_name,
                          values_from = parameter_value)
View(Wide_chla_a)


##### got data from Chla A now I need to see how much each dataset
##has per a year. 

chla_a_year_counts<- Wide_chla_a%>%
  mutate(year = year(as.Date(sample_date))) %>%
  count(year)
View(chla_a_year_counts)


Chla_a_2017<-Wide_chla_a%>%
  mutate(year = year(as.Date(sample_date)))%>%
  filter(year(sample_date)==2017)

View(Chla_a_2017)


####  getting top 25% of chla a depths 

max_depth_chla <- max(Chla_a_2017$sample_depth_m)

top_25_depth_chla<- subset(Chla_a_2017,sample_depth_m<= 0.25*max_depth_chla)

View(top_25_depth_chla)


### finding a representative chla-a value for each lake

Each_lakes_chla <- Chla_a_2017%>%
  group_by(lagoslakeid)%>%
  summarise(mean_chla= mean(chla_ugl, na.rm = TRUE),
            n=n())


View(Each_lakes_chla)


Lake_chla_trophic<- Each_lakes_chla%>%
  mutate(
    trophic_status= case_when(mean_chla <= 2 ~ "Oligotrophic",
                              mean_chla <= 7 ~ "Mesotrophic",
                              mean_chla <= 30 ~ "Eutrophic",
                              TRUE ~ "Hypereutrophic")
  )
View(Lake_chla_trophic)

######### understanding lake size data############

cluster_info<- fread(file.choose())
View(cluster_info)

# view sample depth max 

max_sample_depth<- max(chemistry_limno$sample_depth_m)
View(max_sample_depth)


#### load lagos depth data in

depth_data<- fread(file.choose())
View(depth_data)

unique(cluster_info$lake_namelagos)
### load reserviors lagos data in

reservoir_data<- fread(file.choose())
View(reservoir_data)


install.packages(dplyr)
library(dplyr)

sulfate_reservoir<- chemistry_limno%>%
  left_join(depth_data %>% select(lagoslakeid, lake_maxdepth_m, lake_meandepth_m), by= "lagoslakeid")%>%
  left_join(reservoir_data, by = "lagoslakeid")


View(sulfate_reservoir)


top20_depth <- sulfate_reservoir%>%
  filter(sample_depth_m<= lake_maxdepth_m*0.20)


View(top20_depth)


# get top depth year counts

top20_year_counts<- top20_depth%>%
  mutate(year=year(as.Date(sample_date)))%>%
  count(year)
View(top20_year_counts)  
### this entire data set above has been looking at each dataset as parameter id and i need just sulafte 
# from here i will create a joined table with deptha nd just sulfate data set 

sulfate_Joined_table<- Sulfate_wide%>%
  left_join(depth_data%>%
              select(lagoslakeid,lake_maxdepth_m,lake_meandepth_m),
            by = "lagoslakeid")
View(sulfate_Joined_table)

# filter for top 20 

top_20_sulafte<- sulfate_Joined_table%>%
  filter(sample_depth_m<= lake_maxdepth_m*0.2)
View(top_20_sulafte)

# see how much data per a year 

top_20sulfate_year_counts<- top_20_sulafte%>%
  mutate(year=year(as.Date(sample_date)))%>%
  count(year)
View(top_20sulfate_year_counts)
# export

install.packages("writexl")
library(writexl)

write.csv(top_20_sulafte, "top_20_sulafte.cvs", row.names = FALSE)
getwd()


library(writexl)

write_xlsx(top_20_sulafte, path = "sulfate_depth.xlsx")

getwd()


# filter for top ten 

top_10_sulfate<- sulfate_Joined_table%>%
  filter(sample_depth_m<= lake_maxdepth_m*.10)
View(top_10_sulfate)

# view yearly adat for top ten 
top_10_sulfate_year_counts<- top_10_sulfate%>%
  mutate(year=year(as.Date(sample_date)))%>%
  count(year)
View(top_10_sulfate_year_counts)

write_xlsx(top_10_sulfate, path = "sulfate_topten.xlsx")


getwd()



# making a line plot to show average changes in sulfate overtime throught teh years 

# average sulfate

library(readxl)
sulfate_topten <- read_excel("sulfate_topten.xlsx")
View(sulfate_topten)




library(dplyr)

sulfate_year <- sulfate_topten %>%
  mutate(year = format(as.Date(sample_date), "%Y")) %>%
  group_by(year) %>%
  summarize(
    mean_sulfate = mean(so4_mgl, na.rm = TRUE),
    sd_sulfate = sd(so4_mgl, na.rm = TRUE),
    n = n()
  )

head(sulfate_year)

View(sulfate_year)

################################
  ##### ADDING WATERSHED DATA#####
#######################################

install.packages("data.table")
library(data.table)

watersheds_data<- fread(file.choose())

View(watersheds_data)
names(watersheds_data)
lake_watersheds_clean <-watersheds_data[,c("lagoslakeid",
  "ws_zoneid",
  "ws_subtype",
  "ws_equalsnws",
  "ws_states",
  "ws_focallakewaterarea_ha",
  "ws_area_ha",
  "ws_lake_arearatio",
  "ws_lat_decdeg",
  "ws_lon_decdeg"
)]
View(lake_watersheds_clean)


#Upload topten depths data 

zip_file <- file.choose()

unzip(zip_file, list = TRUE)



library(readr)
sulfate_topten <- read_csv("sulfate_topten.xlsx")
View(sulfate_topten)

install.packages("readxl")  # Run only once
library(readxl)

lake_info <- read_excel(file.choose())



#joining sulfate top ten percent of depths with watershed data
install.packages(dpylr)
library(dpylr)

sulfate_watershed<- Sulfate_topten%>%
  left_join(lake_watersheds_clean,
            by="lagoslakeid")

dplyr::n_distinct(
  watersheds_data$nws_zoneid,
  na.rm = TRUE
)

nrow(watersheds_data)


library(dplyr)

watersheds_data %>%
  summarise(
    total_rows = n(),
    unique_lakes = n_distinct(lagoslakeid),
    unique_ws_ids = n_distinct(ws_zoneid, na.rm = TRUE),
    unique_nws_ids = n_distinct(nws_zoneid, na.rm = TRUE),
    missing_nws_ids = sum(is.na(nws_zoneid))
  )

###########################################
#####EXPLORING LAKE INFORMATION CSV########
###########################################

library(dplyr)
library(readr)

library(readr)
lake_info <- read_csv("excel tables/lake_information (1).csv")
View(lake_info)


dplyr::n_distinct(lake_info$hu4_zoneid, na.rm = TRUE)
names(lake_info)

######################################################################
##########Creating the data frame for my random forest model#########
######################################################################

# key ideas I need
# cut down to only a few variables I can justify 
# random Forest() cannot use rows with NAs, so the simplest fix is to remove
# any row that has a missing value in any column. (Fancier options exist,
# like filling gaps by interpolation, but removing rows is honest and simple.)

# upload my sulfate data I want data from top 10% depths table 

# uploads sulfate top ten 

library(readxl)
sulfate_topten <- read_excel("sulfate_topten.xlsx")
View(sulfate_topten)
names(sulfate_topten)
nrow(sulfate_topten)

######################################################################################
################Cleaning my data base sulfate top ten cleaning ###############
##################################################################################
library(dplyr)

sulfate_topten %>%
  summarise(
    total_rows = n(),
    unique_lakes = n_distinct(lagoslakeid),
    missing_sulfate = sum(is.na(so4_mgl)),
    missing_dates = sum(is.na(sample_date)),
    missing_depths = sum(is.na(sample_depth_m))
  )

library(dplyr)

sulfate_topten %>%
  count(lagoslakeid, name = "observations_per_lake") %>%
  summarise(
    lakes = n(),
    average = mean(observations_per_lake),
    median = median(observations_per_lake),
    minimum = min(observations_per_lake),
    maximum = max(observations_per_lake)
  )

library(dplyr)

lake_counts <- sulfate_topten %>%
  count(lagoslakeid, name = "n_observations") %>%
  arrange(desc(n_observations))

# Show the 20 most frequently sampled lakes
head(lake_counts, 20)

# See the distribution across lakes
summary(lake_counts$n_observations)

library(dplyr)

lake_counts %>%
  summarise(
    total_lakes = n(),
    total_observations = sum(n_observations),
    lakes_sampled_once = sum(n_observations == 1),
    lakes_sampled_5_or_fewer = sum(n_observations <= 5),
    lakes_sampled_over_20 = sum(n_observations > 20),
    lakes_sampled_over_100 = sum(n_observations > 100),
    top_20_observations = sum(
      head(n_observations, 20)
    ),
    top_20_percent = 100 * sum(
      head(n_observations, 20)
    ) / sum(n_observations)
  )

# Check which parameter IDs are present
unique(sulfate_data$parameter_id)

### Checking sensor codes to see whats flagged 

# Check censored observations
sulfate_topten %>%
  count(censorcode, sort = TRUE)

sulfate_topten %>%
  summarise(
    with_detection_limit =
      sum(!is.na(parameter_detectionlimit_value)),
    without_detection_limit =
      sum(is.na(parameter_detectionlimit_value))
  )

library(dplyr)

sulfate_topten %>%
  group_by(censorcode) %>%
  summarise(
    observations = n(),
    with_detection_limit =
      sum(!is.na(parameter_detectionlimit_value)),
    without_detection_limit =
      sum(is.na(parameter_detectionlimit_value)),
    median_sulfate = median(so4_mgl, na.rm = TRUE),
    .groups = "drop"
  )

library(dplyr)

sulfate_topten %>%
  group_by(censorcode) %>%
  summarise(
    observations = n(),
    
    sulfate_at_or_below_limit = sum(
      !is.na(parameter_detectionlimit_value) &
        so4_mgl <= parameter_detectionlimit_value
    ),
    
    sulfate_above_limit = sum(
      !is.na(parameter_detectionlimit_value) &
        so4_mgl > parameter_detectionlimit_value
    ),
    
    .groups = "drop"
  )

###### decided on data that will be kept
sulfate_rf_baseline <- sulfate_topten %>%
  filter(censorcode %in% c("NC2", "NC4"))

sulfate_rf_baseline

View(sulfate_rf_baseline)

# Check the remaining data
nrow(sulfate_rf_baseline)

sulfate_rf_baseline %>%
  count(censorcode)

# Check the date range
range(as.Date(sulfate_data$sample_date), na.rm = TRUE)

library(dplyr)

sulfate_rf_plus_baseline <- sulfate_rf_baseline %>%
  filter(!is.na(so4_mgl)) %>%
  mutate(
    sample_date = as.Date(sample_date),
    sample_year = as.integer(format(sample_date, "%Y")),
    sample_month = as.integer(format(sample_date, "%m"))
  ) %>%
  select(
    # Outcome
    so4_mgl,
    
    # Keep identifiers for tracking and validation
    lagoslakeid,
    eventid_epi,
    sample_id,
    cluster_id,
    source_id,
    
    # Potential predictors
    sample_year,
    sample_month,
    sample_depth_m,
    lake_maxdepth_m,
    lake_meandepth_m,
    cluster_lat_dd,
    cluster_lon_dd,
    
    # Quality control
    censorcode,
    parameter_detectionlimit_value,
    sample_depth_flag
  )
View(sulfate_rf_plus_baseline)

names(sulfate_rf_plus_baseline)



######################################################################################
################Adding lake characteristics to my data  ###############
##################################################################################

library(dplyr)
library(readr)

library(readr)
lake_info <- read_csv("excel tables/lake_information (1).csv")
View(lake_info)
names(lake_info)


# Create a smaller lake-information table in R

library(dplyr)

lake_info_model <- lake_info %>%
  select(
    lagoslakeid,
    lake_elevation_m,
    lake_huc12,
    ws_zoneid,
    nws_zoneid,
    hu4_zoneid,
    hu8_zoneid,
    hu12_zoneid,
    buff100_zoneid,
    buff500_zoneid,
    epanutr_zoneid,
    omernik3_zoneid,
    bailey_zoneid,
    lake_missingws
  )
View(lake_info_model)

#check for duplicated lake IDs:
lake_info_model %>%
  count(lagoslakeid) %>%
  filter(n > 1)
# join to my sulfate_rf_plus_baseline table

sulfate_rf_enriched <- sulfate_rf_plus_baseline %>%
  left_join(lake_info_model, by = "lagoslakeid")

View(sulfate_rf_enriched)
nrow(sulfate_rf_enriched)

# Confirm the join did not multiply observations
nrow(sulfate_rf_plus_baseline)
nrow(sulfate_rf_enriched)
# How many distinct lakes matched?
sum(
  unique(sulfate_rf_plus_baseline$lagoslakeid) %in%
    lake_info_model$lagoslakeid
)

####### Checking for unmatched lakes #########
library(dplyr)

unmatched_lakes <- sulfate_rf_plus_baseline %>%
  distinct(lagoslakeid) %>%
  anti_join(
    lake_info_model %>% distinct(lagoslakeid),
    by = "lagoslakeid"
  )

# Count unmatched lakes
nrow(unmatched_lakes)

# View their IDs
print(unmatched_lakes, n = 45)

class(sulfate_rf_plus_baseline$lagoslakeid)
class(lake_info_model$lagoslakeid)
n_distinct(sulfate_rf_plus_baseline$lagoslakeid)
# check what lakes were excluded
excluded_lakes <- sulfate_topten %>%
  distinct(lagoslakeid) %>%
  anti_join(
    sulfate_rf_plus_baseline %>%
      distinct(lagoslakeid),
    by = "lagoslakeid"
  )

nrow(excluded_lakes)
sulfate_topten %>%
  semi_join(excluded_lakes, by = "lagoslakeid") %>%
  count(censorcode)
sulfate_rf_plus_baseline %>%
  count(censorcode)

######################################################################################
################Adding land use data ##############
##################################################################################

library(readr)
zone_landuse <- read_csv("excel tables/zone_landuse.csv")
View(zone_landuse)

names(zone_landuse)

library(dplyr)
library(stringr)
library(readr)

# Which spatial divisions are present?
zone_landuse %>%
  count(spatial_division, sort = TRUE)

library(dplyr)
library(stringr)

nlcd_ws <- zone_landuse %>%
  mutate(
    spatial_division = str_to_lower(
      str_squish(as.character(spatial_division))
    ),
    zoneid = as.character(zoneid),
    year = as.integer(year)
  ) %>%
  filter(spatial_division == "ws") %>%
  rename(
    ws_zoneid = zoneid,
    nlcd_year = year
  ) %>%
  select(-spatial_division)
# How many unique watershed zones?
n_distinct(nlcd_ws$ws_zoneid)

# Which NLCD years are available?
sort(unique(nlcd_ws$nlcd_year))

# How many records exist for each year?
nlcd_ws %>%
  count(nlcd_year, sort = FALSE)

nlcd_ws %>%
  count(ws_zoneid, nlcd_year) %>%
  filter(n > 1)

sulfate_rf_enriched %>%
  summarise(
  earliest_year = min(sample_year, na.rm = TRUE),
    latest_year = max(sample_year, na.rm = TRUE),
    before_2001 = sum(sample_year < 2001, na.rm = TRUE),
    years_2001_to_2016 = sum(
      sample_year >= 2001 & sample_year <= 2016,
      na.rm = TRUE
    ),
    after_2016 = sum(sample_year > 2016, na.rm = TRUE)
  )

##### there is a gap in my data set. there is only data after 2001 for NLCD for now
### I will create a random forest with data that starts at 2001 but latter I will add in more NLCD data from another set

library(dplyr)

# Standardize ID formats
nlcd_ws <- nlcd_ws %>%
  mutate(
    ws_zoneid = as.character(ws_zoneid),
    nlcd_year = as.integer(nlcd_year)
  )

sulfate_rf_enriched <- sulfate_rf_enriched %>%
  mutate(
    ws_zoneid = as.character(ws_zoneid),
    sample_year = as.integer(sample_year)
  )

# Count unmatched watershed IDs
sulfate_rf_enriched %>%
  distinct(ws_zoneid) %>%
  filter(!is.na(ws_zoneid)) %>%
  anti_join(
    nlcd_ws %>% distinct(ws_zoneid),
    by = "ws_zoneid"
  ) %>%
  nrow()
# assign correct year for NLCD data 
sulfate_with_nlcd_year <- sulfate_rf_enriched %>%
  mutate(
    nlcd_year_used = case_when(
      sample_year >= 2001 & sample_year <= 2003 ~ 2001L,
      sample_year >= 2004 & sample_year <= 2005 ~ 2004L,
      sample_year >= 2006 & sample_year <= 2007 ~ 2006L,
      sample_year >= 2008 & sample_year <= 2010 ~ 2008L,
      sample_year >= 2011 & sample_year <= 2012 ~ 2011L,
      sample_year >= 2013 & sample_year <= 2015 ~ 2013L,
      sample_year >= 2016 ~ 2016L,
      TRUE ~ NA_integer_
    )
  )
nlcd_ws %>%
  count(ws_zoneid, nlcd_year) %>%
  filter(n > 1)

# Keep a separate copy of the snapshot year for validation
nlcd_ws_join <- nlcd_ws %>%
  mutate(nlcd_year_matched = nlcd_year)

sulfate_nlcd_A_full <- sulfate_with_nlcd_year %>%
  left_join(
    nlcd_ws_join,
    by = c(
      "ws_zoneid",
      "nlcd_year_used" = "nlcd_year"
    )
  ) %>%
  mutate(
    nlcd_year_gap = sample_year - nlcd_year_matched
  )

sulfate_nlcd_A_full <- sulfate_nlcd_A_full %>%
  mutate(
    pct_openwater = nlcd_openwater11_pct,
    
    pct_developed =
      nlcd_devopen21_pct +
      nlcd_devlow22_pct +
      nlcd_devmed23_pct +
      nlcd_devhi24_pct,
    
    pct_forest =
      nlcd_fordec41_pct +
      nlcd_forcon42_pct +
      nlcd_formix43_pct,
    
    pct_agriculture =
      nlcd_past81_pct +
      nlcd_cultcrop82_pct,
    
    pct_wetland =
      nlcd_wetwood90_pct +
      nlcd_wetemerg95_pct,
    
    pct_shrub = nlcd_shrub52_pct,
    pct_grassland = nlcd_grass71_pct,
    pct_barren = nlcd_barren31_pct,
    pct_ice_snow = nlcd_icesnow12_pct
  )


sulfate_nlcd_A_model <- sulfate_nlcd_A_full %>%
  filter(!is.na(nlcd_year_matched))

# Check dataset size and coverage
sulfate_nlcd_A_model %>%
  summarise(
    total_rows = n(),
    unique_lakes = n_distinct(lagoslakeid),
    first_sample_year = min(sample_year),
    last_sample_year = max(sample_year),
    unmatched_forest_pct = sum(is.na(pct_forest)),
    unmatched_agriculture_pct = sum(is.na(pct_agriculture)),
    max_nlcd_year_gap = max(nlcd_year_gap, na.rm = TRUE)
  )


View(sulfate_nlcd_A_model)

#### checking for four unmatched datapoints 

sulfate_nlcd_A_model %>%
  summarise(
    total_rows = n(),
    unique_lakes = n_distinct(lagoslakeid),
    first_sample_year = min(sample_year, na.rm = TRUE),
    last_sample_year = max(sample_year, na.rm = TRUE),
    unmatched_forest_pct = sum(is.na(pct_forest)),
    unmatched_agriculture_pct = sum(is.na(pct_agriculture)),
    max_nlcd_year_gap = max(nlcd_year_gap, na.rm = TRUE)
  ) %>%
  print(width = Inf)

unmatched_nlcd <- sulfate_with_nlcd_year %>%
  filter(!is.na(nlcd_year_used)) %>%
  anti_join(
    nlcd_ws_join,
    by = c(
      "ws_zoneid",
      "nlcd_year_used" = "nlcd_year"
    )
  )

nrow(unmatched_nlcd)

unmatched_nlcd %>%
  select(
    lagoslakeid,
    sample_year,
    ws_zoneid,
    nlcd_year_used
  ) %>%
  print(n = Inf)

library(dplyr)

lake_info %>%
  filter(lagoslakeid %in% c(482073, 479040)) %>%
  select(
    lagoslakeid,
    lake_missingws,
    ws_zoneid,
    nws_zoneid,
    hu4_zoneid,
    hu8_zoneid
  )

library(dplyr)
library(tidyr)

predictor_cols <- c(
  "sample_year",
  "sample_month",
  "sample_depth_m",
  "lake_maxdepth_m",
  "lake_meandepth_m",
  "lake_elevation_m",
  "cluster_lat_dd",
  "cluster_lon_dd",
  "pct_forest",
  "pct_agriculture",
  "pct_developed",
  "pct_wetland",
  "pct_openwater"
)



install.packages("tidyr")  # Run once, if needed
library(tidyr)


sulfate_nlcd_A_model %>%
  summarise(
    across(
      all_of(predictor_cols),
      ~ sum(is.na(.x)),
      .names = "missing_{.col}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  print(n = Inf)

View(sulfate_nlcd_A_model)

names(sulfate_nlcd_A_model)

########### removing landcover variables 


library(dplyr)

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  select(
    -nlcd_openwater11_pct,
    -nlcd_icesnow12_pct,
    -nlcd_devopen21_pct,
    -nlcd_devlow22_pct,
    -nlcd_devmed23_pct,
    -nlcd_devhi24_pct,
    -nlcd_barren31_pct,
    -nlcd_fordec41_pct,
    -nlcd_forcon42_pct,
    -nlcd_formix43_pct,
    -nlcd_shrub52_pct,
    -nlcd_grass71_pct,
    -nlcd_past81_pct,
    -nlcd_cultcrop82_pct,
    -nlcd_wetwood90_pct,
    -nlcd_wetemerg95_pct
  )


sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  select(
    -pct_openwater,
    -pct_wetland,
    -pct_shrub,
    -pct_grassland,
    -pct_barren,
    -pct_ice_snow
  )

View(sulfate_nlcd_A_model)






########################
###### atmosphere data###
############################

library(readr)
zone_atmosphere <- read_csv("excel tables/zone_atmosphere.csv")
View(zone_atmosphere)


unique(zone_atmosphere$variable_name)
zone_atmosphere %>%
  filter(variable_name == "totaldepsulfur_kgperha") %>%
  nrow()

zone_atmosphere %>%
  filter(variable_name == "totaldepsulfur_kgperha") %>%
  summarise(
    total_rows = n(),
    non_missing = sum(!is.na(value)),
    missing = sum(is.na(value))
  )
zone_atmosphere %>%
  filter(variable_name == "totaldepsulfur_kgperha") %>%
  count(year, sort = FALSE)
names(zone_atmosphere)

unique(zone_atmosphere$variable_name)

# filtering out for wet sulfate

library(dplyr)

wet_sulfate_deposition <- zone_atmosphere %>%
  filter(variable_name == "wetdepso4_kgperha")

View(wet_sulfate_deposition)

wet_sulfate_deposition %>%
  summarise(
    total_rows = n(),
    missing_values = sum(is.na(value)),
    unique_zones = n_distinct(zoneid),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )

#Data points per year
wet_sulfate_deposition %>%
  count(year, name = "data_points") %>%
  arrange(year)

# check units 
wet_sulfate_deposition %>%
  distinct(variable_name, units)
wet_sulfate_deposition %>%
  filter(year >= 2001, year <= 2021) %>%
  group_by(year) %>%
  summarise(
    data_points = n(),
    missing_values = sum(is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(year)

names(wet_sulfate_deposition)
##### 
########### Join wet_sulfate data to A model

names(sulfate_nlcd_A_model)

library(dplyr)

wet_sulfate_deposition_clean <- wet_sulfate_deposition %>%
  select(
    zoneid,
    year,
    value,
    datacoveragepct
  ) %>%
  rename(
    bailey_zoneid = zoneid,
    sample_year = year,
    wetdepso4_kgperha = value
  )

View(wet_sulfate_deposition_clean)

class(sulfate_nlcd_A_model$bailey_zoneid)
class(wet_sulfate_deposition_clean$bailey_zoneid)

class(sulfate_nlcd_A_model$sample_year)
class(wet_sulfate_deposition_clean$sample_year)
# convert both to charcter 
sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  mutate(bailey_zoneid = as.character(bailey_zoneid))

wet_sulfate_deposition_clean <- wet_sulfate_deposition_clean %>%
  mutate(bailey_zoneid = as.character(bailey_zoneid))

# join tables 

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  left_join(
    wet_sulfate_deposition_clean,
    by = c("bailey_zoneid", "sample_year")
  )
View(sulfate_nlcd_A_model)
# check join 

sulfate_nlcd_A_model %>%
  summarise(
    total_rows = n(),
    matched_deposition = sum(!is.na(wetdepso4_kgperha)),
    unmatched_deposition = sum(is.na(wetdepso4_kgperha)),
    percent_matched =
      mean(!is.na(wetdepso4_kgperha)) * 100
  )

# investigating unmatched rows

unmatched_deposition <- sulfate_nlcd_A_model %>%
  filter(is.na(wetdepso4_kgperha))

unmatched_deposition %>%
  count(sample_year, sort = TRUE)


unmatched_deposition %>%
  distinct(bailey_zoneid) %>%
  anti_join(
    wet_sulfate_deposition_clean %>%
      distinct(bailey_zoneid),
    by = "bailey_zoneid"
  )

wet_sulfate_deposition_clean %>%
  count(sample_year) %>%
  arrange(sample_year)
`print(n = 2018-2021)`


unmatched_deposition %>%
  summarise(
    total_rows = n(),
    missing_deposition_values = sum(is.na(wetdepso4_kgperha))
  )

wet_sulfate_deposition_clean %>%
  count(sample_year) %>%
  filter(sample_year >= 2018) %>%
  arrange(sample_year)

exists("zone_atmosphere")

library(dplyr)

wet_sulfate_deposition_clean <- zone_atmosphere %>%
  filter(variable_name == "wetdepso4_kgperha") %>%
  select(zoneid, year, value, datacoveragepct) %>%
  rename(
    bailey_zoneid = zoneid,
    sample_year = year,
    wetdepso4_kgperha = value
  )
range(wet_sulfate_deposition_clean$sample_year, na.rm = TRUE)

wet_sulfate_deposition_clean %>%
  count(sample_year) %>%
  arrange(desc(sample_year)) %>%
  head(10)

# the data is only available for sulfate wet depositions until 2017, so my model A will be 2001-2017


sort(unique(sulfate_nlcd_A_model$sample_year))

View(sulfate_nlcd_A_model)


# removing data points with sulafte blanks 

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  filter(!is.na(wetdepso4_kgperha))
names(sulfate_nlcd_A_model)


View(sulfate_nlcd_A_model)

sort(unique(sulfate_nlcd_A_model$sample_year))

names(sulfate_nlcd_A_model)

saveRDS(sulfate_nlcd_A_model, "sulfate_nlcd_A_model.rds")
View(sulfate_nlcd_A_model)

########################################
##########Precipitation###############
########################################


library(readr)
zone_climate_monthly <- read_csv("excel tables/zone_climate_monthly.csv")
View(zone_climate_monthly)

names(zone_climate_monthly)




library(dplyr)

climate_ppt <- zone_climate_monthly %>%
  select(
    zoneid,
    climate_year,
    climate_month,
    spatial_division,
    climate_ppt_mm
  )




climate_ppt %>%
  count(zoneid, spatial_division, climate_year, climate_month) %>%
  filter(n > 1)





library(dplyr)

climate_ppt_hu12 <- climate_ppt %>%
  filter(spatial_division == "hu12") %>%
  select(
    zoneid,
    climate_year,
    climate_month,
    climate_ppt_mm
  ) %>%
  rename(hu12_zoneid = zoneid)

View(climate_ppt_hu12)

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  left_join(
    climate_ppt_hu12,
    by = c(
      "hu12_zoneid",
      "sample_year" = "climate_year",
      "sample_month" = "climate_month"
    )
  )






