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



library(readxl)
sulfate_topten <- read_excel("sulfate_topten.xlsx")
View(sulfate_topten)

names(sulfate_topten)

summary(sulfate_topten$so4_mgl)
head(sulfate_topten[, c("source_id", "source_sample_siteid", "so4_mgl")])

sulfate_topten %>%
  count(source_id, sort = TRUE)
sulfate_topten %>%
  group_by(source_id) %>%
  summarise(
    n = sum(!is.na(so4_mgl)),
    median_so4 = median(so4_mgl, na.rm = TRUE),
    min_so4 = min(so4_mgl, na.rm = TRUE),
    max_so4 = max(so4_mgl, na.rm = TRUE)
  ) %>%
  arrange(desc(n))

install.packages("data.table")  # only needed once
library(data.table)


chemistry_limno <- fread(file.choose())


View(chemistry_limno)

names(chemistry_limno)
ls()

names(sulfate)
