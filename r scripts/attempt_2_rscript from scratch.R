#######################                           #####################
####################### Cleaning Sulfate data in R#####################
#######################                           #####################

###############Uploading Necessary Libraries###################
###############                              ###################

install.packages("data.table")
library(data.table)

install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

###############Uploading DATA###################
###############             ###################

Chemistry_limo<- fread(file.choose())

View(Chemistry_limo)

###############Filtering data###################
###############             ###################

sulfate_data <-Chemistry_limo[parameter_id == 34]
View(sulfate_data)


Sulfate_wide <- pivot_wider(sulfate_data,
              names_from = parameter_name,
              values_from = parameter_value)
View(Sulfate_wide)

##### filter by depth####

str(Sulfate_wide$sample_depth_m)

Sulfate_1.5m <- Sulfate_wide%>%
  filter(sample_depth_m==1.5)
view(Sulfate_1.5m)

surface_sulfate<- Sulfate_wide %>%
  filter(sample_depth_m <=2, 
         sample_depth_m >= 1) #just example of other way to filter
  #filter(between(sample_depth_m,1.0,2.0))

View(surface_sulfate)

#####Filter by date####
str(surface_sulfate$sample_date)

#Surface_sulfate.as.date<- as.Date(surface_sulfate$sample_date)

surface_sulfate$sample_date <- as.Date(surface_sulfate$sample_date)


library(dplyr)
library(lubridate)

Sulfate_2017 <- surface_sulfate %>%
  filter(year(sample_date) == 2017)
View(Sulfate_2017)

####################Uploading more data################

library(data.table)

Cluster_info_limno<-fread(file.choose())

View(Cluster_info_limno)

### decided how to join the data

install.packages("janitor")
library(janitor)

clean_names(Sulfate_2017)
clean_names(Cluster_info_limno)

write.csv(Sulfate_2017,"avg_sulfate_2017.csv",row.names = FALSE)




sulfate_data_left_joined<- left_join(Sulfate_2017,Cluster_info_limno,
                                     )


