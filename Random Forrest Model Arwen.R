###################################################################
##################################################################
############ MY OWN RANDOM FORREST MODEL######################
################################################################
################################################################



sulfate_nlcd_A_model <- readRDS("sulfate_nlcd_A_model.rds")
View(sulfate_nlcd_A_model)


########################################
##########Precipitation###############
########################################

install.packages("data.table")
library(data.table)
zone_climate_monthly<- fread(file.choose())

names(climate)

library(readr)
zone_climate_monthly <- read_csv("excel tables/zone_climate_monthly.csv")
#Viene_climate_monthly)

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

View(sulfate_nlcd_A_model)



################# breaking up data based on lake depth ##############

# Get the range of lake depths
range(sulfate_nlcd_A_model$lake_maxdepth_m, na.rm = TRUE)


summary(sulfate_nlcd_A_model$lake_maxdepth_m)

range(sulfate_nlcd_A_model$lake_maxdepth_m, na.rm = TRUE)

quantile(
  sulfate_nlcd_A_model$lake_maxdepth_m,
  probs = c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
  na.rm = TRUE
)


library(ggplot2)

ggplot(sulfate_nlcd_A_model,
       aes(x = lake_maxdepth_m)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Maximum Depth",
    x = "Lake Depth in m",
    y = "Number of Lakes"
  ) +
  theme_minimal()

################ADDING IN LAKE SURFACE AREA############
library(readr)
cluster_information_limno <- read_csv("cluster_information_limno.csv")
View(cluster_information_limno)
names(cluster_information_limno)


library(dplyr)

lake_area <- cluster_information_limno %>%
  select(
    lagoslakeid,
    lake_waterarea_ha
  ) %>%
  distinct(lagoslakeid, .keep_all = TRUE)

View(lake_area)

summary(lake_area$lake_waterarea_ha)

range(lake_area$lake_waterarea_ha, na.rm = TRUE)

quantile(
  lake_area$lake_waterarea_ha,
  probs = c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
  na.rm = TRUE
)

#### join my data set 
sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  left_join(
    lake_area,
    by = "lagoslakeid"
  )

# Check the columns
names(sulfate_nlcd_A_model)

# Check how many area values matched
sum(!is.na(sulfate_nlcd_A_model$lake_waterarea_ha))

# Check missing values
sum(is.na(sulfate_nlcd_A_model$lake_waterarea_ha))

# Check area range
range(sulfate_nlcd_A_model$lake_waterarea_ha, na.rm = TRUE)

cluster_information_limno %>%
  group_by(lagoslakeid) %>%
  summarise(
    n_area_values = n_distinct(lake_waterarea_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_area_values > 1)

########################################################
###### setting precip data to one month ahead############
#######################################################
library(dplyr)

precip_monthly <- zone_climate_monthly %>%
  filter(spatial_division == "hu12") %>%
  select(
    hu12_zoneid = zoneid,
    climate_year,
    climate_month,
    climate_ppt_mm
  )

names(precip_monthly)

unique(zone_climate_monthly$spatial_division)

names(sulfate_nlcd_A_model)

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  mutate(
    precip_year = if_else(sample_month == 1,
                          sample_year - 1,
                          sample_year),
    precip_month = if_else(sample_month == 1,
                           12L,
                           sample_month - 1L)
  )


sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  left_join(
    precip_monthly,
    by = c(
      "hu12_zoneid",
      "precip_year" = "climate_year",
      "precip_month" = "climate_month"
    )
  ) %>%
  rename(prev_month_ppt_mm = climate_ppt_mm)

summary(sulfate_nlcd_A_model$prev_month_ppt_mm)

sum(is.na(sulfate_nlcd_A_model$prev_month_ppt_mm))

names(precip_monthly)


sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  select(-climate_ppt_mm)

sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  left_join(
    precip_monthly,
    by = c(
      "hu12_zoneid",
      "precip_year" = "climate_year",
      "precip_month" = "climate_month"
    )
  ) %>%
  rename(prev_month_ppt_mm = climate_ppt_mm)

summary(sulfate_nlcd_A_model$prev_month_ppt_mm)

sum(is.na(sulfate_nlcd_A_model$prev_month_ppt_mm))
nrow(sulfate_nlcd_A_model)

saveRDS(
  sulfate_nlcd_A_model,
  "sulfate_nlcd_A_model.rds"
)

View(sulfate_nlcd_A_model)

Sulfate_nlcd_precipdata_A_model<- sulfate_nlcd_A_model

dir.create("data/processed",
           recursive = TRUE,
           showWarnings = FALSE)

saveRDS(
  Sulfate_nlcd_precipdata_A_model,
  "data/processed/Sulfate_nlcd_precipdata_A_model.rds"
)



sulfate_nlcd_A_model <- readRDS("sulfate_nlcd_A_model.rds")
na
View(sulfate_nlcd_A_model)
# Create cyclical month variables
sulfate_nlcd_A_model <- sulfate_nlcd_A_model %>%
  mutate(
    month_sin = sin(2 * pi * (sample_month - 1) / 12),
    month_cos = cos(2 * pi * (sample_month - 1) / 12)
  )

nrow(sulfate_nlcd_A_model)
names(sulfate_nlcd_A_model)

########################################################
###### adding in dates data############
#######################################################

# Load into a separate object so nothing gets overwritten
model_check <- readRDS(
  "data/processed/Sulfate_nlcd_precipdata_A_model.rds"
)

dim(model_check)

names(model_check)

original_check <- readRDS(
  "data/processed/sulfate_nlcd_A_model.rds"
)

dim(original_check)

names(original_check)



sapply(
  model_check[, c(
    "ws_zoneid",
    "nws_zoneid",
    "hu4_zoneid",
    "hu8_zoneid",
    "hu12_zoneid",
    "buff100_zoneid",
    "buff500_zoneid",
    "epanutr_zoneid",
    "omernik3_zoneid",
    "bailey_zoneid"
  )],
  function(x) length(unique(na.omit(x)))
)


names(model_check)






intersect(
  c("lagoslakeid", "eventid_epi", "sample_id"),
  names(model_check)
)
intersect(
  c("lagoslakeid", "eventid_epi", "sample_id"),
  names(dates_data)
)
library(dplyr)

date_lookup <- dates_data %>%
  select(
    lagoslakeid,
    eventid_epi,
    sample_id,
    sample_date
  ) %>%
  distinct()

model_with_dates <- model_check %>%
  select(-any_of(c("sample_date", "sample_date.x", "sample_date.y"))) %>%
  left_join(
    date_lookup,
    by = c("lagoslakeid", "eventid_epi", "sample_id"),
    relationship = "many-to-one"
  )

names(model_with_dates)

View(model_with_dates)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  model_with_dates,
  "data/processed/model_with_dates.rds"
)


library(readxl)

dates_data <- read_excel(file.choose())

names(dates_data)


library(dplyr)

dates_data %>%
  count(eventid_epi, lagoslakeid, sample_id) %>%
  filter(n > 1)


date_lookup <- dates_data %>%
  select(
    eventid_epi,
    lagoslakeid,
    sample_id,
    sample_date
  ) %>%
  distinct()

class(date_lookup$sample_date)
head(date_lookup$sample_date)

ls()

#####################################
###########Chal Data
library(data.table)

library(data.table)

chemistry_limno <- fread(
  "C:/Users/Arwen/Downloads/chemistry_limno.csv"
)
names(chemistry_limno)

library(dplyr)

chemistry_limno %>%
  filter(grepl("chlorophyll|chl-a|chla",
               parameter_name,
               ignore.case = TRUE)) %>%
  count(parameter_id, parameter_name, sort = TRUE)
######## extract Chla A 

names(chemistry_limno)
chla_data <- chemistry_limno %>%
  filter(parameter_id == 9) %>%
  select(
    lagoslakeid,
    sample_date,
    sample_id,
    parameter_value,
    parameter_detectionlimit_value,
    censorcode,
    sample_depth_m
  ) %>%
  rename(
    chla_ugl = parameter_value,
    chla_detection_limit = parameter_detectionlimit_value,
    chla_censorcode = censorcode,
    chla_sample_depth_m = sample_depth_m
  )
summary(chla_data$chla_sample_depth_m)
names(chla_data)

hist(
  chla_data$chla_sample_depth_m,
  breaks = 20,
  main = "Distribution of Chl-a Sample Depths",
  xlab = "Sample Depth (m)",
  ylab = "Number of Samples"
)


chla_data %>%
  mutate(
    depth_category = case_when(
      chla_sample_depth_m == 0 ~ "Surface (0 m)",
      chla_sample_depth_m > 0 &
        chla_sample_depth_m <= 2 ~ "Shallow (0–2 m)",
      chla_sample_depth_m > 2 &
        chla_sample_depth_m <= 5 ~ "Intermediate (2–5 m)",
      chla_sample_depth_m > 5 ~ "Deep (>5 m)",
      TRUE ~ "Missing"
    )
  ) %>%
  count(depth_category) %>%
  mutate(percent = round(n / sum(n) * 100, 2))


summary(model_check$sample_depth_m)

library(ggplot2)

ggplot() +
  geom_histogram(
    data = chla_data,
    aes(x = chla_sample_depth_m),
    bins = 25,
    fill = "blue",
    alpha = 0.5
  ) +
  labs(
    title = "Chl-a Sampling Depth Distribution",
    x = "Sample Depth (m)",
    y = "Number of Samples"
  ) +
  theme_minimal()

sulfate_data <- chemistry_limno %>%
  filter(parameter_id == 34) %>%
  select(
    lagoslakeid,
    sample_date,
    sample_id,
    sample_depth_m,
    parameter_value
  )




dim(chla_data)

summary(chla_data$chla_ugl)

n_distinct(chla_data$lagoslakeid)

sum(is.na(chla_data$chla_ugl))

chla_data %>%
  count(lagoslakeid, sample_date) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# check date formats 
class(chla_data$sample_date)

class(sulfate_nlcd_A_model$sample_date)

sum(
  paste(chla_data$lagoslakeid, chla_data$sample_date) %in%
    paste(sulfate_nlcd_A_model$lagoslakeid,
          sulfate_nlcd_A_model$sample_date)
)

names(model_with_dates)

View(model_with_dates)

names(model_check)


names(model_with_dates)

class(model_with_dates$sample_date)

class(chla_data$sample_date)

model_with_dates <- model_with_dates %>%
  mutate(sample_date = as.Date(sample_date))

chla_data <- chla_data %>%
  mutate(sample_date = as.Date(sample_date))

sum(
  paste(model_with_dates$lagoslakeid,
        model_with_dates$sample_date) %in%
    paste(chla_data$lagoslakeid,
          chla_data$sample_date)
)

sulfate_nlcd_A_chla_model <- model_with_dates %>%
  left_join(
    chla_data %>%
      select(lagoslakeid, sample_date, chla_ugl),
    by = c("lagoslakeid", "sample_date")
  )

sulfate_nlcd_A_chla_model <- model_with_dates %>%
  left_join(
    chla_data %>%
      select(lagoslakeid, sample_date, chla_ugl),
    by = c("lagoslakeid", "sample_date")
  )


library(dplyr)

# Check for duplicate lake/date combinations in chlorophyll data
chla_data %>%
  count(lagoslakeid, sample_date) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Check for duplicate lake/date combinations in sulfate data
model_with_dates %>%
  count(lagoslakeid, sample_date) %>%
  filter(n > 1) %>%
  arrange(desc(n))

model_with_dates %>%
  filter(lagoslakeid == 96686) %>%
  arrange(sample_date) %>%
  select(
    lagoslakeid,
    sample_date,
    so4_mgl,
    sample_depth_m,
    lake_maxdepth_m,
    everything()
  ) %>%
  View()

model_with_dates %>%
  filter(lagoslakeid == 96686) %>%
  group_by(sample_date) %>%
  summarise(
    observations = n(),
    sulfate_values = paste(so4_mgl, collapse = ", "),
    depths = paste(sample_depth_m, collapse = ", "),
    sample_ids = paste(sample_id, collapse = ", "),
    .groups = "drop"
  )

chla_data %>%
  count(lagoslakeid, sample_date) %>%
  filter(n > 1) %>%
  arrange(desc(n))



# Find columns containing depth in their names
names(model_with_dates)[
  grepl("depth", names(model_with_dates),
        ignore.case = TRUE)
]

names(chla_data)[
  grepl("depth", names(chla_data),
        ignore.case = TRUE)
]


Backup_model_with_dates <- model_with_dates

saveRDS(
  Backup_model_with_dates,
  "Backup_model_with_dates.rds"
)

file.exists("Backup_model_with_dates.rds")
########## to load latter
Backup_model_with_dates <- readRDS("Backup_model_with_dates.rds")

############# Chal join 

sulfate_nlcd_A_chla_model <- model_with_dates %>%
  left_join(
    chla_data %>%
      select(lagoslakeid, sample_date, chla_ugl),
    by = c("lagoslakeid", "sample_date")
  )


names(sulfate_nlcd_A_chla_model)
  View(sulfate_nlcd_A_chla_model)

  
  # Create backup
  Backup_sulfate_nlcd_A_chla_model <- sulfate_nlcd_A_chla_model
  
  # Save backup as RDS
  saveRDS(
    Backup_sulfate_nlcd_A_chla_model,
    "Backup_sulfate_nlcd_A_chla_model.rds"
  )
  
  Backup_sulfate_nlcd_A_chla_model <- readRDS(
    "Backup_sulfate_nlcd_A_chla_model.rds"
  ) 
  
 
names(sulfate_nlcd_A_chla_model)
  


####################
########working with sulfate data#


#### checking why my shap results for sulf turned data lower 

names(sulfate_nlcd_A_chla_model)

class(sulfate_nlcd_A_chla_model$wetdepso4_kgperha)

class(sulfate_nlcd_A_chla_model$so4_mgl)


sulfate_check <- sulfate_nlcd_A_chla_model %>%
  mutate(
    wetdepso4_kgperha = as.numeric(wetdepso4_kgperha)
  )

class(sulfate_check$wetdepso4_kgperha)

summary(sulfate_check$wetdepso4_kgperha)
sum(is.na(sulfate_check$wetdepso4_kgperha))

sum(is.na(sulfate_nlcd_A_chla_model$wetdepso4_kgperha))


cor(
  sulfate_check$wetdepso4_kgperha,
  sulfate_check$so4_mgl,
  use = "complete.obs",
  method = "spearman"
)

sum(complete.cases(
  sulfate_check$wetdepso4_kgperha,
  sulfate_check$so4_mgl
))


library(dplyr)

yearly_sulfate <- sulfate_check %>%
  group_by(sample_year) %>%
  summarise(
    n = n(),
    mean_deposition = mean(wetdepso4_kgperha, na.rm = TRUE),
    mean_sulfate = mean(so4_mgl, na.rm = TRUE),
    median_sulfate = median(so4_mgl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sample_year)

print(yearly_sulfate, n = Inf)

install.packages("ggplot2") # Only needed if not already installed

library(ggplot2)

ggplot(yearly_sulfate, aes(x = sample_year, y = mean_deposition)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Wet Sulfate Deposition Over Time",
    x = "Sampling Year",
    y = "Mean Wet Sulfate Deposition (kg/ha)"
  ) +
  theme_minimal()

ggplot(yearly_sulfate, aes(x = sample_year, y = median_sulfate)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Median Lake Sulfate Over Time",
    x = "Sampling Year",
    y = "Median Sulfate (mg/L)"
  ) +
  theme_minimal()


yearly_correlations <- sulfate_check %>%
  group_by(sample_year) %>%
  summarise(
    n = sum(complete.cases(
      wetdepso4_kgperha,
      so4_mgl
    )),
    correlation = cor(
      wetdepso4_kgperha,
      so4_mgl,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  )



print(yearly_correlations, n = Inf)


regional_correlations <- sulfate_check %>%
  group_by(hu4_zoneid) %>%
  summarise(
    n = sum(complete.cases(
      wetdepso4_kgperha,
      so4_mgl
    )),
    correlation = cor(
      wetdepso4_kgperha,
      so4_mgl,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(correlation)

print(regional_correlations, n = Inf)

library(ggplot2)

ggplot(
  regional_correlations,
  aes(x = correlation)
) +
  geom_histogram(
    bins = 15,
    color = "black"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Regional Variation in Deposition-Sulfate Relationships",
    x = "Spearman Correlation",
    y = "Number of HUC4 Regions"
  ) +
  theme_minimal()


selected_regions <- c(
  "hu4_116",
  "hu4_149",
  "hu4_12",
  "hu4_5"
)

regional_plot_data <- sulfate_check %>%
  filter(hu4_zoneid %in% selected_regions)

ggplot(
  regional_plot_data,
  aes(
    x = wetdepso4_kgperha,
    y = so4_mgl
  )
) +
  geom_point(alpha = 0.35) +
  geom_smooth(
    method = "loess",
    color = "red"
  ) +
  facet_wrap(~hu4_zoneid, scales = "free") +
  labs(
    title = "Regional Deposition vs. Lake Sulfate",
    x = "Wet Sulfate Deposition (kg/ha)",
    y = "Lake Sulfate (mg/L)"
  ) +
  theme_minimal()


regional_correlations <- sulfate_check %>%
  group_by(hu4_zoneid) %>%
  summarise(
    n = sum(complete.cases(
      wetdepso4_kgperha,
      so4_mgl
    )),
    correlation = cor(
      wetdepso4_kgperha,
      so4_mgl,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(correlation)

print(regional_correlations, n = Inf)

ggplot(
  sulfate_check,
  aes(
    x = pct_forest,
    y = so4_mgl
  )
) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue") +
  labs(
    title = "Forest Cover vs. Lake Sulfate",
    x = "Forest Cover (%)",
    y = "Lake Sulfate (mg/L)"
  ) +
  theme_minimal()


ggplot(
  sulfate_check,
  aes(
    x = pct_forest,
    y = so4_mgl
  )
) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue") +
  labs(
    title = "Forest Cover vs. Lake Sulfate",
    x = "Forest Cover (%)",
    y = "Lake Sulfate (mg/L)"
  ) +
  theme_minimal()
ggplot(
  sulfate_check,
  aes(
    x = cluster_lon_dd,
    y = cluster_lat_dd,
    color = wetdepso4_kgperha
  )
) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Geographic Distribution of Wet Sulfate Deposition",
    x = "Longitude",
    y = "Latitude",
    color = "Wet Deposition"
  ) +
  theme_minimal()

ggplot(
  sulfate_check,
  aes(
    x = cluster_lon_dd,
    y = cluster_lat_dd,
    color = so4_mgl
  )
) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(
    trans = "log10"
  ) +
  labs(
    title = "Geographic Distribution of Lake Sulfate",
    x = "Longitude",
    y = "Latitude",
    color = "Sulfate (mg/L)"
  ) +
  theme_minimal()

ggplot(
  sulfate_check,
  aes(
    x = cluster_lon_dd,
    y = cluster_lat_dd,
    color = so4_mgl
  )
) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(
    trans = "log10"
  ) +
  labs(
    title = "Geographic Distribution of Lake Sulfate",
    x = "Longitude",
    y = "Latitude",
    color = "Sulfate (mg/L)"
  ) +
  theme_minimal()

regional_comparison <- sulfate_check %>%
  filter(
    hu4_zoneid %in% c(
      "hu4_116",
      "hu4_149",
      "hu4_12",
      "hu4_5"
    )
  ) %>%
  group_by(hu4_zoneid) %>%
  summarise(
    observations = n(),
    unique_lakes = n_distinct(lagoslakeid),
    
    mean_sulfate = mean(so4_mgl, na.rm = TRUE),
    median_sulfate = median(so4_mgl, na.rm = TRUE),
    
    mean_deposition = mean(
      wetdepso4_kgperha, na.rm = TRUE
    ),
    
    mean_forest = mean(pct_forest, na.rm = TRUE),
    mean_agriculture = mean(pct_agriculture, na.rm = TRUE),
    mean_developed = mean(pct_developed, na.rm = TRUE),
    
    mean_depth = mean(lake_maxdepth_m, na.rm = TRUE),
    
    .groups = "drop"
  )

print(regional_comparison)

sulfate_check %>%
  filter(
    hu4_zoneid %in% c(
      "hu4_116",
      "hu4_149",
      "hu4_12",
      "hu4_5"
    )
  ) %>%
  count(hu4_zoneid, lagoslakeid, name = "measurements") %>%
  arrange(hu4_zoneid, desc(measurements))

# ---- 0nstall and load packages -------------------------------------------
# You only need to install packages once per computer. The line below installs
# anything that is missing, then library() loads them for this session.

pkgs <- c("randomForest", # builds the random forest model
          "fastshap",     # calculates SHAP values for any model
          "ggplot2",      # makes the plots
          "ggbeeswarm")   # makes the "beeswarm" SHAP summary plot
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

library(randomForest)
library(fastshap)
library(ggplot2)
library(ggbeeswarm)

# Random forests and SHAP both use random numbers. Setting a "seed" makes the
# random numbers the same every time, so you get the same answer as your
# classmates (and as yourself next week).
set.seed(123)


# ---- 1. Load and look at the data -------------------------------------------

rf_data <- sulfate_nlcd_A_model %>%
  select(
    so4_mgl,
    pct_forest,
    pct_developed,
    pct_agriculture,
    wetdepso4_kgperha,
    prev_month_ppt_mm,
    lake_waterarea_ha,
    lake_maxdepth_m,
    sample_year,
    month_sin,
    month_cos
  )
View(rf_data)
head(rf_data)


#Convert wet sulfate deposition to numeric
rf_data <- rf_data %>%
  mutate(
    wetdepso4_kgperha = as.numeric(wetdepso4_kgperha)
  )
str(rf_data)
summary(rf_data)
colSums(is.na(rf_data))
nrow(rf_data)

# Number of observations before removing missing values
nrow(rf_data)

# Remove rows with missing values
rf_data <- na.omit(rf_data)

# Number of observations remaining
nrow(rf_data)
table(sulfate_nlcd_A_model$censorcode, useNA = "ifany")
sulfate_nlcd_A_model %>%
  group_by(censorcode) %>%
  summarise(
    n = n(),
    min_so4 = min(so4_mgl, na.rm = TRUE),
    median_so4 = median(so4_mgl, na.rm = TRUE),
    max_so4 = max(so4_mgl, na.rm = TRUE),
    mean_so4 = mean(so4_mgl, na.rm = TRUE),
    min_detection_limit = min(parameter_detectionlimit_value, na.rm = TRUE),
    max_detection_limit = max(parameter_detectionlimit_value, na.rm = TRUE)
  )
sulfate_nlcd_A_model %>%
  group_by(censorcode) %>%
  summarise(
    total = n(),
    detection_limits_available =
      sum(!is.na(parameter_detectionlimit_value)),
    detection_limits_missing =
      sum(is.na(parameter_detectionlimit_value))
  )
####################
###########build my RF###########
#####################################

set.seed(123)

rf <- randomForest(
  so4_mgl ~ .,
  data = rf_data,
  ntree = 500,
  importance = TRUE
)

rf
######################### Evaluate Model Performance 
# Out-of-bag R-squared
oob_r2 <- rf$rsq[length(rf$rsq)]

cat("OOB R-squared:", round(oob_r2, 3), "\n")

# Out-of-bag mean squared error
oob_mse <- rf$mse[length(rf$mse)]

cat("OOB MSE:", round(oob_mse, 3), "\n")

# Out-of-bag RMSE
oob_rmse <- sqrt(oob_mse)

cat("OOB RMSE:", round(oob_rmse, 3), "mg/L\n")

quantile(
  rf_data$so4_mgl,
  probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
  na.rm = TRUE
)
summary(rf_data$so4_mgl)

plot(
  rf,
  main = "Random Forest: OOB Error vs. Number of Trees"
)

############# Variable importance 
# Calculate variable importance
imp <- importance(rf, type = 1)

# Sort from most important to least important
imp <- imp[order(imp[, 1], decreasing = TRUE), ]

imp

varImpPlot(
  rf,
  type = 1,
  main = "Sulfate Random Forest Variable Importance"
)

###################################
########Calculate SHAP values######

# Select predictors only
X <- rf_data %>%
  select(-so4_mgl)

# Take a random sample for SHAP
set.seed(123)

X_shap <- X %>%
  slice_sample(n = min(1000, nrow(X)))

# Prediction function required by fastshap
pred_fun <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Calculate SHAP values
install.packages(
  "fastshap",
  repos = c(
    "https://bgreenwell.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
library(fastshap)
shap <- fastshap::explain(
  object = rf,
  X = X,
  newdata = X_shap,
  pred_wrapper = pred_fun,
  nsim = 100,
  adjust = TRUE
)

shap <- as.data.frame(shap)

head(shap)

# rank variables sing SHAP
shap_rank <- data.frame(
  variable = names(shap),
  mean_abs_shap = sapply(shap, function(x) mean(abs(x)))
)

shap_rank <- shap_rank %>%
  arrange(desc(mean_abs_shap))

shap_rank

# create a SHAP beeswarm plot
shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = unlist(shap, use.names = FALSE)
)

# Rank variables
shap_rank <- shap_rank %>%
  arrange(desc(mean_abs_shap))

shap_long$variable <- factor(
  shap_long$variable,
  levels = rev(shap_rank$variable)
)

# Standardize predictor values for plotting
exists("shap")
exists("X_shap")
shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = unlist(shap, use.names = FALSE)
)
shap_long$value_z <- ave(
  shap_long$value,
  shap_long$variable,
  FUN = function(v) as.numeric(scale(v))
)

#Create a SHAP beeswarm plot
shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = unlist(shap, use.names = FALSE)
)

# Rank variables
shap_rank <- shap_rank %>%
  arrange(desc(mean_abs_shap))

shap_long$variable <- factor(
  shap_long$variable,
  levels = rev(shap_rank$variable)
)

# Standardize predictor values for plotting
shap_long$value_z <- ave(
  shap_long$value,
  shap_long$variable,
  FUN = function(v) as.numeric(scale(v))
)

# make plot 
install.packages("ggbeeswarm")
library(ggbeeswarm)
p_bee <- ggplot(
  shap_long,
  aes(x = shap, y = variable, color = value_z)
) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_quasirandom(
    orientation = "y",
    size = 1.5,
    width = 0.3
  ) +
  scale_color_gradient2(
    low = "#2166AC",
    mid = "grey85",
    high = "#B2182B",
    name = "Predictor value"
  ) +
  labs(
    title = "SHAP Summary: Predictors of Lake Sulfate",
    x = "SHAP value (mg/L)",
    y = NULL
  ) +
  theme_bw()

print(p_bee)

##############SHAP dependence plots

p_dep <- ggplot(
  shap_long,
  aes(x = value, y = shap)
) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_x"
  ) +
  labs(
    title = "SHAP Dependence Plots",
    x = "Predictor value",
    y = "SHAP value (mg/L)"
  ) +
  theme_bw()

print(p_dep)

###########################Save your model and results  
# Save the random forest model
saveRDS(rf, "sulfate_random_forest.rds")

# Save variable importance
write.csv(
  imp,
  "sulfate_variable_importance.csv"
)

# Save SHAP values
write.csv(
  shap,
  "sulfate_SHAP_values.csv",
  row.names = FALSE
)

# Save plots
ggsave("sulfate_SHAP_beeswarm.png", p_bee,
       width = 8, height = 6, dpi = 300)

ggsave("sulfate_SHAP_dependence.png", p_dep,
       width = 10, height = 7, dpi = 300)
  

names(sulfate_data)

names(chemistry_limno)

names(lagos_dates)
head(lagos_dates)

head(sulfate_data)

head(chemistry_limno)


saveRDS(rf_data, "rf_data.rds")


write.csv(
  sulfate_rf_enriched,
  "data/processed/sulfate_rf_enriched.csv",
  row.names = FALSE
)


nrow(model_check)


nrow(sulfate_nlcd_A_chla_model)

