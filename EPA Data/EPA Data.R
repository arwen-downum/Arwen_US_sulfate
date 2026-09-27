###### Uploading data from EPA site #########


sulfate<- read.csv(file.choose())

View(sulfate)

sum(!is.na(sulfate$ActivityEndDate))


table(sulfate$ResultSampleFractionText, useNA = "ifany")

table(sulfate$CharacteristicName)


table(sulfate$ResultMeasure.MeasureUnitCode, useNA = "ifany")

install.packages("dplyr")
library(dplyr)


sulfate_mgL <- sulfate %>%
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", "mg/L"))
nrow(sulfate_mgL)


table(
  sulfate_mgL$ResultSampleFractionText,
  useNA = "ifany"
)

colnames(sulfate)
table(sulfate$ResultMeasure.MeasureUnitCode, useNA = "ifany")
# ==========================================================
# Filter EPA (Water Quality Portal) sulfate data to match
# LAGOS so4_mgl (dissolved sulfate, mg SO4/L)
# ==========================================================
###### find only mg/L and mg/l data

epa_sulfate_mgL <- sulfate %>%
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/L", "mg/l"))
View(epa_sulfate_mgL)

table(epa_sulfate_mgL$CharacteristicName)

table(epa_sulfate_mgL$ResultSampleFractionText, useNA = "ifany")

### now filtering for dissolved and total only 
library(stringr)
EPA<- epa_sulfate_mgL%>%
  mutate(fraction_flag = str_to_lower(str_trim(ResultSampleFractionText))) %>%
  filter(fraction_flag %in% c("dissolved", "total"))


View(EPA)
table(EPA$ResultSampleFractionText)

table(EPA$MethodSpeciationName, useNA = "ifany")

epa <- EPA %>%
  mutate(
    speciation = str_to_lower(str_trim(MethodSpeciationName)),
    # treat blank, NA, and "None" as "unspecified"
    speciation = if_else(is.na(speciation) | speciation %in% c("", "none"),
                         "unspecified", speciation)
  ) %>%
  # keep only the rows we can use
  filter(speciation %in% c("unspecified", "as so4", "as s")) %>%
  mutate(
    value_num = as.numeric(ResultMeasureValue),
    # convert "as S" rows to SO4, leave the rest alone
    so4_mgl = if_else(speciation == "as s", value_num * 2.996, value_num),
    converted_from_S = (speciation == "as s")
  )

# Check it worked
table(epa$speciation)
nrow(epa)


# Rows where the original value exists but didn't convert to a number
bad_values <- epa %>%
  filter(is.na(value_num) & !is.na(ResultMeasureValue) & ResultMeasureValue != "")

nrow(bad_values)
sort(table(bad_values$ResultMeasureValue), decreasing = TRUE)[1:10]

# How many are simply blank?
sum(is.na(epa$ResultMeasureValue) | epa$ResultMeasureValue == "")


epa <- epa %>%
  mutate(
    nondetect_flag = !is.na(ResultDetectionConditionText) &
      str_trim(ResultDetectionConditionText) != ""
  ) %>%
  filter(!is.na(so4_mgl), so4_mgl >= 0)

# Check
nrow(epa)
sum(epa$nondetect_flag)
summary(epa$so4_mgl)

epa %>% arrange(desc(so4_mgl)) %>% select(so4_mgl, MonitoringLocationIdentifier) %>% head(10)


View(epa)


write.csv(epa, file = "epa_sulfate_cleaned.csv", row.names = FALSE)
getwd()





#################Further clenaing in R 































