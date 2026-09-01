# load in libraries
install.packages("data.table")
library(data.table)

library(dplyr)
library(lubridate)
library(ggplot2)

## download data 

library(readxl)
sulfate_topten <- read_excel("sulfate_topten.xlsx")
View(sulfate_topten)

# filter out under 2m lakes 

lakes_greater_2m <- sulfate_topten%>%
  filter(lake_maxdepth_m >= 2)
View(lakes_greater_2m)

#group by yearly average

str(lakes_greater_2m)

lakes_greater_2m$year_only<- as.numeric(substr(lakes_greater_2m$sample_date, 1, 4))

head(lakes_greater_2m$year_only, 10)

yearly_avg<- lakes_greater_2m%>%
  group_by(year_only)%>%
  summarize(so4_mgl= mean(so4_mgl, na.rm = TRUE))
View(yearly_avg)


# plot yearly avg

ggplot(yearly_avg, aes(x=year_only, y= so4_mgl)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = 1990, linetype = "dashed", color = "red") +
  labs(
    title = "Average Lake Sulfate Levels Over Time",
    subtitle = "Dashed line marks 1990 Clean Air Act Amendments",
    x = "Year",
    y = "Average Sulfate (mg/L)"
  ) +
  theme_minimal()








