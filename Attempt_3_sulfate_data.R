
####################### Cleaning Sulfate data in R#####################


###############Uploading Necessary Libraries###################


install.packages("data.table")
library(data.table)

chemistry_limno<- fread(file.choose())

View(chemistry_limno)

chemistry_limno$parameter_name

#########filter for sulfate only#########

sulfate_data<- chemistry_limno[parameter_id==34]
View(sulfate_data)


install.packages("tidydr")
library(tidyr)

Sulfate_wide <- pivot_wider(sulfate_data,
                            names_from = parameter_name,
                            values_from = parameter_value)
View(Sulfate_wide)

install.packages(dpylr)
library(dplyr)

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

library(readxl)

Sulfate_topten <- read_excel(file.choose())
View(Sulfate_topten)


#joining sulfate top ten percent of depths with watershed data
install.packages(dpylr)
library(dpylr)

sulfate_watershed<- Sulfate_topten%>%
  left_join(lake_watersheds_clean,
            by="lagoslakeid")
View(sulfate_watershed)



