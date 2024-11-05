#exploring data

pacman::p_load(tidyverse, lubridate, akima, reshape2, 
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes)


chemistry_limno <- chemistry_limno

sulfate <- chemistry_limno |>
  mutate(Date = as.Date(sample_date)) |>
  mutate(Year = year(Date)) |>
  mutate(DOY = yday(Date)) |>
  filter(parameter_name == "so4_mgl") |>
  relocate(Date, Year, DOY, .before = sample_date)


write.csv(sulfate, "./sulfate_w_dates.csv", row.names = FALSE)


#lakes 
lakes_information<- cluster_information_limno|>
  select(lagoslakeid, lake_waterarea_ha, lake_states, lake_namelagos)|>
  mutate(State = lake_states, Lake = lake_namelagos)|>
  select(-lake_namelagos, -lake_states)
  
#join lake information to sulfate

sulfate_w_lakeinfo<- sulfate|>
  left_join(lakes_information, by = "lagoslakeid")

write.csv(sulfate_w_lakeinfo, "./sulfate_w_lakeinfo.csv")

#see how many records for each year we have

yearly_counts <- sulfate |>
  mutate(Year = year(sample_date)) |>
  group_by(Year) |>
  summarise(Count = n())

#data from 4551 lakes