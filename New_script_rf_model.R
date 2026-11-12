library(readr)
sulfate_nlcd_A_model <- read_csv("data/processed/sulfate_nlcd_A_model.csv")
View(sulfate_nlcd_A_model)


install.packages("data.table")
library(data.table)
zone_climate_monthly<- fread(file.choose())

names(zone_climate_monthly)


sulfate_nlcd_A_chla_model <- readRDS(
  "Backup_sulfate_nlcd_A_chla_model.rds"
)
############################## Log Transform data 
names(sulfate_nlcd_A_chla_model)

sulfate_nlcd_A_model %>%
  arrange(desc(sO4_mgl)) %>%
  slice_head(n = 10)


library(dplyr)

sulfate_nlcd_A_chla_model <- sulfate_nlcd_A_chla_model %>%
  mutate(
    log_so4_mgl = log10(so4_mgl)
  )
summary(sulfate_nlcd_A_chla_model$so4_mgl)

summary(sulfate_nlcd_A_chla_model$log_so4_mgl)

par(mfrow = c(1, 2))

hist(sulfate_nlcd_A_chla_model$so4_mgl,
     breaks = 50,
     main = "Original Sulfate",
     xlab = "Sulfate (mg/L)")

hist(sulfate_nlcd_A_chla_model$log_so4_mgl,
     breaks = 50,
     main = "Log10-Transformed Sulfate",
     xlab = "Log10 Sulfate")

par(mfrow = c(1, 1))


library(dplyr)

sulfate_nlcd_A_chla_model <- sulfate_nlcd_A_chla_model %>%
  mutate(
    sample_date = as.Date(sample_date),
    Day_of_Year = as.numeric(format(sample_date, "%j"))
  )
names(sulfate_nlcd_A_chla_model)

sulfate_nlcd_A_chla_model <- sulfate_nlcd_A_chla_model %>%
  mutate(
    Seasonal_Timing = sin(2 * pi * Day_of_Year / 365.25)
  )

library(dplyr)

model_saved_sulfate_chla <- sulfate_nlcd_A_chla_model
names(model_saved_sulfate_chla)

library(dplyr)

nrow(model_saved_sulfate_chla)

map_data <- model_saved_sulfate_chla %>%
  select(
    lagoslakeid,
    cluster_lat_dd,
    cluster_lon_dd,
    so4_mgl,
    log_so4_mgl
  ) %>%
  distinct(lagoslakeid, .keep_all = TRUE) %>%
  filter(
    !is.na(cluster_lat_dd),
    !is.na(cluster_lon_dd)
  )

nrow(map_data)


write.csv(
  map_data,
  "sulfate_lake_map.csv",
  row.names = FALSE
)

getwd()


nrow(map_data)

n_distinct(map_data$lagoslakeid)

################################ counting smaples per a lake


samples_per_lake <- sulfate_nlcd_A_model %>%
  count(lagoslakeid, name = "sample_count")

clean_samples_per_lake<- na.omit(samples_per_lake)

clean_samples_per_lake %>%
  summarise(
    total_lakes = n(),
    single_sample = sum(sample_count == 1),
    two_to_five = sum(sample_count >= 2 & sample_count <= 5),
    six_to_ten = sum(sample_count >= 6 & sample_count <= 10),
    more_than_ten = sum(sample_count > 10),
    median_samples = median(sample_count),
    mean_samples = mean(sample_count)
  )










# ---- 0. Install and load packages --------------------------------

pkgs <- c(
  "dplyr",
  "randomForest",
  "fastshap",
  "ggplot2",
  "ggbeeswarm"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

library(dplyr)
library(randomForest)
library(fastshap)
library(ggplot2)
library(ggbeeswarm)

set.seed(123)


# ---- 1. Prepare the sulfate dataset -------------------------------
nrow(sulfate_nlcd_A_chla_model)
rf_data <- sulfate_nlcd_A_chla_model %>%
  transmute(
    Log10_Sulfate = log10(so4_mgl),
    Forest_Cover_pct = pct_forest,
    Developed_Land_pct = pct_developed,
    Agricultural_Land_pct = pct_agriculture,
    Wet_Sulfate_Deposition_kg_ha = wetdepso4_kgperha,
    Chlorophyll_a_ug_L = chla_ugl,
    Previous_Month_Precipitation_mm = prev_month_ppt_mm,
    Lake_Surface_Area_ha = lake_waterarea_ha,
    Lake_Maximum_Depth_m = lake_maxdepth_m,
    Day_of_Year = as.numeric(format(as.Date(sample_date), "%j"))
  )

rf_data <- na.omit(rf_data)
nrow(rf_data)

set.seed(123)

rf <- randomForest(
  Log10_Sulfate ~ .,
  data = rf_data,
  ntree = 500,
  importance = TRUE
)

# ---- 2. Check missing values --------------------------------------

colSums(is.na(rf_data))

# Number of observations before removing missing values
nrow(rf_data)

# Remove observations with missing predictors or response
rf_data <- na.omit(rf_data)

# Check final sample size
nrow(rf_data)

# Check sulfate distribution on log scale
summary(rf_data$Log10_Sulfate)


# ---- 3. Build random forest model ---------------------------------

set.seed(123)

rf <- randomForest(
  Log10_Sulfate ~ .,
  data = rf_data,
  ntree = 500,
  importance = TRUE
)

print(rf)


# ---- 4. Evaluate model performance --------------------------------
ls()
print(rf)
names(rf)

rfrsq <- tail(rf$rsq, 1)

rfrsq

# OOB R-squared
rfrsq <- tail(rf$rsq, 1)

# OOB MSE
rfmse <- tail(rf$mse, 1)

# OOB RMSE
rfrmse <- sqrt(rfmse)

# Print results
rfrsq
rfmse
rfrmse

oob_r2 <- rfrsq[length(rfrsq)]

oob_mse <- rfmse[length(rfmse)]

oob_rmse <- sqrt(oob_mse)

cat("OOB R-squared:", round(oob_r2, 3), "\n")

cat("OOB MSE (log scale):", round(oob_mse, 4), "\n")

cat("OOB RMSE (log scale):", round(oob_rmse, 4), "\n")


# ---- 5. Variable importance ----------------------------------------

imp <- importance(rf, type = 1)

imp <- imp[order(imp[, 1], decreasing = TRUE), ]

print(imp)

varImpPlot(
  rf,
  type = 1,
  main = "Random Forest Variable Importance: Lake Sulfate"
)

imp <- importance(rf, type = 1)

rownames(imp) <- gsub("_", " ", rownames(imp))

imp <- imp[order(imp[, 1], decreasing = TRUE), ]

print(imp)

varImpPlot(
  rf,
  type = 1,
  main = "Random Forest Variable Importance: Lake Sulfate"
)
#####################updated cleaner names Section 5 ###################################################


# ---- 5. Variable importance ----------------------------------------

# Extract permutation importance (%IncMSE)
imp <- importance(rf, type = 1)

# Convert to data frame
imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp[, 1]
)

# Clean variable labels
imp_df$Variable <- gsub("_", " ", imp_df$Variable)

# Make labels more presentation-friendly
imp_df$Variable <- gsub("Forest Cover pct", "Forest Cover (%)", imp_df$Variable)
imp_df$Variable <- gsub("Developed Land pct", "Developed Land (%)", imp_df$Variable)
imp_df$Variable <- gsub("Agricultural Land pct", "Agricultural Land (%)", imp_df$Variable)
imp_df$Variable <- gsub("Wet Sulfate Deposition kg ha", "Wet Sulfate Deposition (kg/ha)", imp_df$Variable)
imp_df$Variable <- gsub("Chlorophyll a ug L", "Chlorophyll-a (µg/L)", imp_df$Variable)
imp_df$Variable <- gsub("Previous Month Precipitation mm", "Previous Month Precipitation (mm)", imp_df$Variable)
imp_df$Variable <- gsub("Lake Surface Area ha", "Lake Surface Area (ha)", imp_df$Variable)
imp_df$Variable <- gsub("Lake Maximum Depth m", "Lake Maximum Depth (m)", imp_df$Variable)
imp_df$Variable <- gsub("Day of Year", "Day of Year", imp_df$Variable)

# Order variables by importance
imp_df <- imp_df[order(imp_df$Importance), ]

# Plot
library(ggplot2)

ggplot(imp_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Random Forest Variable Importance",
    subtitle = "Permutation importance (%IncMSE)",
    x = "% Increase in Mean Squared Error",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank()
  )














# ---- 6. Plot random forest error -----------------------------------

plot(
  rf,
  main = "Random Forest: OOB Error vs. Number of Trees"
)


# ---- 7. Prepare data for SHAP --------------------------------------

# Select predictors only
X <- rf_data %>%
  select(-Log10_Sulfate)

# Select sample for SHAP calculations
set.seed(123)

X_shap <- X %>%
  slice_sample(n = min(1000, nrow(X)))

# Prediction function
pred_fun <- function(object, newdata) {
  predict(object, newdata = newdata)
}


# ---- 8. Calculate SHAP values --------------------------------------

set.seed(123)

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


# ---- 9. Rank variables using SHAP ----------------------------------

shap_rank <- data.frame(
  variable = names(shap),
  mean_abs_shap = sapply(
    shap,
    function(x) mean(abs(x))
  )
) %>%
  arrange(desc(mean_abs_shap))

print(shap_rank)


# ---- 10. Create SHAP plotting dataset ------------------------------

shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = unlist(shap, use.names = FALSE)
)

# Standardize predictor values for color scale

sapply(X_shap, class)

X_shap$Wet_Sulfate_Deposition_kg_ha <- as.numeric(
  X_shap$Wet_Sulfate_Deposition_kg_ha
)
sapply(X_shap, class)
shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = as.vector(shap)
)



shap_long$value_z <- ave(
  shap_long$value,
  shap_long$variable,
  FUN = function(v) {
    as.numeric(scale(v))
  }
)

# Order variables by SHAP importance
shap_long$variable <- factor(
  shap_long$variable,
  levels = rev(shap_rank$variable)
)


# ---- 11. SHAP beeswarm plot ----------------------------------------
p_bee <- ggplot(
  shap_long,
  aes(
    x = shap,
    y = variable
  )
) +
  geom_vline(
    xintercept = 0,
    color = "grey60"
  ) +
  geom_quasirandom(
    aes(color = value_z),
    orientation = "y",
    size = 1.5,
    width = 0.3
  ) +
  scale_color_gradient2(
    low = "#2166AC",
    mid = "grey85",
    high = "#B2182B",
    name = "Standardized\npredictor value"
  ) +
  labs(
    title = "SHAP Summary: Predictors of Lake Sulfate",
    x = "SHAP value (log10 sulfate scale)",
    y = NULL
  ) +
  theme_bw()

print(p_bee)


dim(shap)

dim(X_shap)

dim(shap_long)

length(shap_long$shap)

length(shap_long$value_z)

length(shap_long$variable)

class(shap)
str(shap)
head(shap)

shap_long <- data.frame(
  variable = rep(names(X_shap), each = nrow(X_shap)),
  value = unlist(X_shap, use.names = FALSE),
  shap = as.vector(as.matrix(shap))
)

shap_long$value_z <- ave(
  shap_long$value,
  shap_long$variable,
  FUN = function(v) as.numeric(scale(v))
)

dim(shap_long)

summary(shap_long$shap)

length(shap_long$shap)

p_bee <- ggplot(
  shap_long,
  aes(x = shap, y = variable)
) +
  geom_vline(
    xintercept = 0,
    color = "grey60"
  ) +
  geom_quasirandom(
    aes(color = value_z),
    orientation = "y",
    size = 1.5,
    width = 0.3
  ) +
  scale_color_gradient2(
    low = "#2166AC",
    mid = "grey85",
    high = "#B2182B",
    name = "Standardized\npredictor value"
  ) +
  labs(
    title = "SHAP Summary: Predictors of Lake Sulfate",
    x = "SHAP value (log10 sulfate scale)",
    y = NULL
  ) +
  theme_bw()

print(p_bee)





# ---- 12. SHAP dependence plots ------------------------------------

p_dep <- ggplot(
  shap_long,
  aes(
    x = value,
    y = shap
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey60"
  ) +
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
    y = "SHAP value (log10 sulfate scale)"
  ) +
  theme_bw()






print(p_dep)

names(rf_data)


p_dep <- ggplot(
  shap_long,
  aes(
    x = value,
    y = shap
  )
) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  geom_point(
    alpha = 0.35,
    size = 1
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    color = "blue",
    linewidth = 1.1,
    se = FALSE
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_x"
  ) +
  labs(
    title = "SHAP Dependence Plots",
    subtitle = "Environmental predictors of lake sulfate concentrations",
    x = "Predictor value",
    y = "SHAP value (log10 sulfate scale)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    strip.text = element_text(
      size = 10,
      face = "bold"
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )

p_dep

#################################### updated labels plots
# Presentation-friendly variable labels
variable_labels <- c(
  "Log10_Sulfate" = "Log10 Sulfate Concentration",
  "Lake_MaxDepth_m" = "Maximum Lake Depth (m)",
  "Previous_Month_PPT_mm" = "Previous Month Total Precipitation (mm)",
  "wetdepso4_kgperha" = "Wet Sulfate Deposition (kg/ha)",
  "lake_waterarea_ha" = "Lake Surface Area (ha)",
  "chla_ugl" = "Chlorophyll-a Concentration (µg/L)",
  "pct_agriculture" = "Agricultural Land Cover (%)",
  "pct_developed" = "Developed Land Cover (%)",
  "pct_forest" = "Forested Land Cover (%)"
)

shap_long$variable <- recode(
  shap_long$variable,
  !!!variable_labels
)

shap_long$variable <- gsub("_", " ", shap_long$variable)

p_bee <- ggplot(
  shap_long,
  aes(x = shap, y = variable)
) +
  geom_vline(
    xintercept = 0,
    color = "grey60"
  ) +
  geom_quasirandom(
    aes(color = value_z),
    orientation = "y",
    size = 1.5,
    width = 0.3
  ) +
  scale_color_gradient2(
    low = "#2166AC",
    mid = "grey85",
    high = "#B2182B",
    name = "Standardized\nPredictor Value"
  ) +
  labs(
    title = "SHAP Summary: Predictors of Lake Sulfate",
    x = "SHAP Value (Log10 Sulfate Scale)",
    y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

print(p_bee)






library(ggplot2)

# Get the names of your predictors
variables <- unique(shap_long$variable)

# Create one separate plot for each predictor
shap_plots <- list()

for (v in variables) {
  
  shap_plots[[v]] <- ggplot(
    subset(shap_long, variable == v),
    aes(x = value, y = shap)
  ) +
    geom_hline(
      yintercept = 0,
      color = "grey60",
      linetype = "dashed"
    ) +
    geom_point(
      alpha = 0.5,
      size = 1.5
    ) +
    labs(
      title = paste("SHAP Dependence Plot:", v),
      x = v,
      y = "SHAP value (log10 sulfate scale)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13
      )
    )
}

# Display each plot individually
shap_plots[[1]]


shap_plots[[1]]  # Chlorophyll A (if first)
shap_plots[[2]]  # Lake surface area
shap_plots[[3]]  # Day of year
shap_plots[[4]]  # Maximum depth
shap_plots[[5]]
shap_plots[[6]]
shap_plots[[7]]

names(rf_data)
dim(rf_data)


# Get the names of your predictors
variables <- unique(shap_long$variable)

# Create one separate plot for each predictor
shap_plots <- list()

for (v in variables) {
  
  # Replace underscores with spaces for display
  pretty_name <- gsub("_", " ", v)
  
  shap_plots[[v]] <- ggplot(
    subset(shap_long, variable == v),
    aes(x = value, y = shap)
  ) +
    geom_hline(
      yintercept = 0,
      color = "grey60",
      linetype = "dashed"
    ) +
    geom_point(
      alpha = 0.5,
      size = 1.5
    ) +
    labs(
      title = paste("SHAP Dependence Plot:", pretty_name),
      x = pretty_name,
      y = "SHAP value (log10 sulfate scale)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13
      )
    )
}

# Display each plot individually
shap_plots[[1]]
shap_plots[[2]]
shap_plots[[3]]
shap_plots[[4]]
shap_plots[[5]]
shap_plots[[6]]
shap_plots[[9]]










names(sulfate_nlcd_A_chla_model)


n_distinct(rf_data$lagoslakeid)
n_distinct(sulfate_nlcd_A_chla_model$lagoslakeid)

nrow(rf_data)






##################
################ raw data shap output


library(dplyr)
library(randomForest)

# Select response and predictors
rf_data_raw <- sulfate_nlcd_A_chla_model %>%
  select(
    so4_mgl,
    chla_ugl,
    lake_waterarea_ha,
    Day_of_Year,
    lake_maxdepth_m,
    wetdepso4_kgperha,
    prev_month_ppt_mm,
    pct_developed,
    pct_agriculture,
    pct_forest
  ) %>%
  mutate(
    wetdepso4_kgperha = as.numeric(wetdepso4_kgperha)
  ) %>%
  na.omit()

dim(rf_data_raw)



set.seed(123)

rf_model_raw <- randomForest(
  so4_mgl ~ .,
  data = rf_data_raw,
  ntree = 500,
  importance = TRUE
)

print(rf_model_raw)

# R-squared
rf_raw_rsq <- tail(rf_model_raw$rsq, 1)

# MSE
rf_raw_mse <- tail(rf_model_raw$mse, 1)

# RMSE
rf_raw_rmse <- sqrt(rf_raw_mse)

rf_raw_rsq
rf_raw_mse
rf_raw_rmse

# New predictor dataset
X_shap_raw <- rf_data_raw %>%
  select(-so4_mgl)

# Check dimensions
dim(X_shap_raw)
names(X_shap_raw)


x = "SHAP value (mg/L sulfate)"


y = "SHAP value (mg/L sulfate)"


# View model results
print(rf_model_raw)

library(fastshap)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)

# Select predictors (exclude original sulfate response)
X_raw <- rf_data_raw %>%
  select(-so4_mgl)

# Select up to 1,000 observations for SHAP
set.seed(123)

X_shap_raw <- X_raw %>%
  slice_sample(n = min(1000, nrow(X_raw)))

# Prediction function
pred_fun_raw <- function(object, newdata) {
  predict(object, newdata = newdata)
}

# Calculate SHAP values
shap_raw <- fastshap::explain(
  object = rf_model_raw,
  X = X_raw,
  newdata = X_shap_raw,
  pred_wrapper = pred_fun_raw,
  nsim = 100,
  adjust = TRUE
)

shap_raw <- as.data.frame(shap_raw)



shap_long_raw <- data.frame(
  variable = rep(names(X_shap_raw), each = nrow(X_shap_raw)),
  value = unlist(X_shap_raw, use.names = FALSE),
  shap = as.vector(as.matrix(shap_raw))
)

# Standardize predictor values for coloring
shap_long_raw$value_z <- ave(
  shap_long_raw$value,
  shap_long_raw$variable,
  FUN = function(v) as.numeric(scale(v))
)

# Check dimensions
dim(shap_long_raw)
summary(shap_long_raw$shap)


p_bee_raw <- ggplot(
  shap_long_raw,
  aes(x = shap, y = variable)
) +
  geom_vline(
    xintercept = 0,
    color = "grey60"
  ) +
  geom_quasirandom(
    aes(color = value_z),
    orientation = "y",
    size = 1.5,
    width = 0.3
  ) +
  scale_y_discrete(
    labels = function(x) gsub("_", " ", x)
  ) +
  scale_color_gradient2(
    low = "#2166AC",
    mid = "grey85",
    high = "#B2182B",
    name = "Standardized\nPredictor Value"
  ) +
  labs(
    title = "SHAP Summary: Predictors of Lake Sulfate",
    x = "SHAP Value (mg/L Sulfate)",
    y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

print(p_bee_raw)

library(ggplot2)

# Filter for wet sulfate deposition
wetdep_data <- subset(
  shap_long_raw,
  variable == "wetdepso4_kgperha"
)

# Create dependence plot
p_wetdep <- ggplot(
  wetdep_data,
  aes(x = value, y = shap)
) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linetype = "dashed"
  ) +
  geom_point(
    color = "#2166AC",
    alpha = 0.6,
    size = 2
  ) +
  labs(
    title = "Effect of Wet Sulfate Deposition on Lake Sulfate",
    x = "Wet Sulfate Deposition (kg/ha)",
    y = "SHAP Value (mg/L Sulfate)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

print(p_wetdep)





