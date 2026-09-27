

sulfate2 <- read.csv(file.choose())

View(sulfate2)


###### view all names of all colums, so I know what variables I am wokring with 
colnames(sulfate2)


# ==========================================================
# Filter for EPA DATA
# ==========================================================
# remove non dissolved and total sulfate
library(dplyr)
library(stringr)
epa_fraction<- sulfate2%>%
  mutate(fraction_flag = str_to_lower(str_trim(ResultSampleFractionText))) %>%
  filter(fraction_flag %in% c("dissolved", "total"))

nrow(epa_fraction)
table(epa_fraction$ResultSampleFractionText, useNA = "ifany")
View(epa_fraction)


# cretaing the same units
table(epa_fraction$ResultMeasure.MeasureUnitCode, useNA = "ifany")

unique(str_trim(epa_fraction$ResultMeasure.MeasureUnitCode))



library(dplyr)
library(stringr)

epa_mgl <- epa_fraction %>%
  mutate(unit_clean = str_trim(ResultMeasure.MeasureUnitCode)) %>%
  filter(!unit_clean %in% c("mg/g", "mg/kg")) %>%
  mutate(
    value_num = as.numeric(ResultMeasureValue),
    conversion_factor = case_when(
      unit_clean %in% c("mg/l", "mg/L")   ~ 1,
      unit_clean %in% c("ug/l", "ug/L")   ~ 0.001,
      unit_clean == "umol/L"              ~ 0.09606,
      unit_clean == "ueq/L"               ~ 0.04803,
      unit_clean == "mg/l CaCO3*^"        ~ 0.9606,
      unit_clean == "ppm"                 ~ 1,
      TRUE ~ NA_real_   # blank "" and anything else stay NA
    ),
    so4_mgl = value_num * conversion_factor
  )

# Confirm mg/g and mg/kg are gone
table(epa_mgl$unit_clean, useNA = "ifany")

# Check what's still excluded (should just be blanks now)
epa_mgl %>%
  filter(is.na(conversion_factor)) %>%
  count(unit_clean, sort = TRUE)

blank_unit_rows <- epa_mgl %>%
  filter(unit_clean == "")

# Do they have actual result values?
sum(!is.na(blank_unit_rows$ResultMeasureValue) & blank_unit_rows$ResultMeasureValue != "")
sum(is.na(blank_unit_rows$ResultMeasureValue) | blank_unit_rows$ResultMeasureValue == "")

# Peek at a few rows to see the pattern
blank_unit_rows %>%
  select(ResultMeasureValue, ResultDetectionConditionText, CharacteristicName, ResultSampleFractionText) %>%
  head(20)


epa_mgl_final <- epa_mgl %>%
  filter(!is.na(so4_mgl))

nrow(epa_mgl_final)

# check speciation method

table(epa_mgl_final$MethodSpeciationName, useNA = "ifany")


library(dplyr)
library(stringr)

epa_speciated <- epa_mgl_final %>%
  mutate(
    speciation_clean = str_trim(MethodSpeciationName),
    speciation_clean = if_else(is.na(speciation_clean) | speciation_clean == "",
                               "none", speciation_clean)
  ) %>%
  filter(speciation_clean %in% c("none", "None", "as SO4", "as S", "as CaCO3")) %>%
  mutate(
    so4_mgl_final = case_when(
      speciation_clean == "as S"     ~ so4_mgl * 2.996,
      speciation_clean == "as CaCO3" ~ so4_mgl * 0.9606,
      TRUE ~ so4_mgl   # none, None, as SO4 need no change
    ),
    converted_from_S = speciation_clean == "as S",
    converted_from_CaCO3 = speciation_clean == "as CaCO3"
  )

# Check it worked
nrow(epa_speciated)
table(epa_speciated$speciation_clean)
summary(epa_speciated$so4_mgl_final)

# cleaning up data concerns 

# How many negative values, and what do they look like?
epa_speciated %>%
  filter(so4_mgl_final < 0) %>%
  select(so4_mgl_final, ResultDetectionConditionText, unit_clean, speciation_clean) %>%
  head(10)

sum(epa_speciated$so4_mgl_final < 0)

# Look at the extreme high end
epa_speciated %>%
  arrange(desc(so4_mgl_final)) %>%
  select(so4_mgl_final, MonitoringLocationIdentifier, unit_clean, speciation_clean, CharacteristicName) %>%
  head(15)

library(dplyr)

epa_clean <- epa_speciated %>%
  filter(so4_mgl_final >= 0) %>%   # drop the 11 negative values
  mutate(
    outlier_flag = so4_mgl_final > 5000   # flag anything above 5000 mg/L as an outlier to review
  )

# Check the result
nrow(epa_clean)
sum(epa_clean$outlier_flag)
summary(epa_clean$so4_mgl_final)

# See where the flagged outliers are coming from
epa_clean %>%
  filter(outlier_flag) %>%
  count(MonitoringLocationIdentifier, sort = TRUE) %>%
  head(10)



