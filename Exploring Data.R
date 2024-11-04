#exploring data

pacman::p_load(tidyverse, lubridate, akima, reshape2, 
               gridExtra, grid, colorRamps, RColorBrewer, rLakeAnalyzer,
               reader, cowplot, dplyr, tidyr, ggplot2, zoo, purrr, beepr, forecast, ggthemes)


chemistry_limno <- chemistry_limno

sulfate <- chemistry_limno|>
  filter(parameter_name == "so4_mgl")

#see how many unique lakes are present

lakes<- sulfate|>
  distinct(lagoslakeid)

#data from 4551 lakes