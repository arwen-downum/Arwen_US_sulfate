# Load the readr package (useful for reading CSVs later if needed)
library(readr)

# Install data.table package (run once, needed for fread/fwrite)
install.packages("data.table")

library(data.table)


# Load the data.table package (provides fread, fwrite, and fast data handling)
library(data.table)


# Open a file chooser window to select your CSV file manually

chemistry_limno <- fread(file.choose())


# Check the dimensions of the dataset (rows x columns)


dim(chemistry_limno)
head(chemistry_limno)


# Filter the dataset to keep only rows where parameter_id equals 34 (sulfate)

sulfate_data <- chemistry_limno[parameter_id == 34]


# Save the filtered sulfate dataset to a new CSV file
fwrite(sulfate_data, "sulfate_only.csv")

View(chemistry_limno)

View(sulfate_data)

######## Seperating out data by State
nj_sulfate <- chemistry_limno[grepl("^NJ", sample_id) & parameter_id == 34]
View(nj_sulfate)
