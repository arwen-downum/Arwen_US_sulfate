#######################                           #####################
####################### Cleaning Sulfate data in R#####################
#######################                           #####################

###############Uploading Necessary Libraries###################
###############                              ###################

install.packages("data.table")
library(data.table)

chemistry_limno<- fread(file.choose())

View(chemistry_limno)

chemistry_limno$parameter_name

#########filter for sulfate only#########

sulfate_data<- Chemistry_limo[parameter_id==34]
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

