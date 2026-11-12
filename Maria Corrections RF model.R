###################################################################
#################################################################
####### MY OWN RANDOM FORREST MODEL with Maria Corrections######
################################################################
################################################################

############## Fixing my data frame ################

# See all available dataframes
ls()








# Check column names
names(sulfate_nlcd_A_full)
names(sulfate_rf_enriched)

library(dplyr)

# Check dimensions
dim(sulfate_nlcd_A_full)

# Check unique lakes and observations
sulfate_nlcd_A_full %>%
  summarise(
    rows = n(),
    unique_lakes = n_distinct(lagoslakeid),
    unique_samples = n_distinct(sample_id),
    min_year = min(sample_year, na.rm = TRUE),
    max_year = max(sample_year, na.rm = TRUE),
    missing_sulfate = sum(is.na(so4_mgl)),
    missing_nlcd = sum(is.na(pct_forest))
  )

# Check duplicate sample records
sulfate_nlcd_A_full %>%
  count(sample_id) %>%
  filter(n > 1)


# Inspect deposition dataset
dim(wet_sulfate_deposition_clean)

names(wet_sulfate_deposition_clean)

head(wet_sulfate_deposition_clean)

# Check existing Model A
dim(sulfate_nlcd_A_model)
names(sulfate_nlcd_A_model)

# Check existing RF dataset
dim(sulfate_rf_enriched)
names(sulfate_rf_enriched)


# 1. Full sulfate + NLCD
dim(sulfate_nlcd_A_full)

# 2. Deposition
dim(wet_sulfate_deposition_clean)
names(wet_sulfate_deposition_clean)

head(wet_sulfate_deposition_clean)

# 3. Lake information
names(lake_info_model)

# 4. Check lake area
"lake_waterarea_ha" %in% names(lake_info_model)

# 5. Check the full sulfate-NLCD dataset
sulfate_nlcd_A_full %>%
  summarise(
    rows = n(),
    lakes = n_distinct(lagoslakeid),
    years_min = min(sample_year, na.rm = TRUE),
    years_max = max(sample_year, na.rm = TRUE),
    missing_forest = sum(is.na(pct_forest)),
    missing_agriculture = sum(is.na(pct_agriculture))
  )

###################################################
###########ADD in my data #########################
####################################################

sulfate_nlcd_A_model <- readRDS("sulfate_nlcd_A_model.rds")

rm(list = ls())

write.csv(
  sulfate_nlcd_A_model,
  "data/processed/sulfate_nlcd_A_model.csv",
  row.names = FALSE
)

dir.create("data/processed",
           recursive = TRUE,
           showWarnings = FALSE)


write.csv(
  sulfate_nlcd_A_model,
  "data/processed/sulfate_nlcd_A_model.csv",
  row.names = FALSE
)

file.exists("data/processed/sulfate_nlcd_A_model.csv")
normalizePath("data/processed/sulfate_nlcd_A_model.csv")
