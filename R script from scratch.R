###################################                                            #############
###################################                                            ##############
##################################  Cleaning Sulfate data to Upload in Arc GIS ##############
#################################                                              #############
#################################                                              #############



###############                          ################
################   Uplaoding data into R ################
################                          #################

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
#######################                      #################################
######################### Filtering dataset  #############################
#library(dplyr)



######## Seperating out data by State
nj_sulfate <- chemistry_limno[grepl("^NJ", sample_id) & parameter_id == 34]
View(nj_sulfate)



#data_2017 <- chemistry_limno[grep("^2017", sample_date) & parameter_id == 34]
View(data_2017)

rlang::last_trace()

str(chemistry_limno$sample_date)

chemistry_limno$sample_date <- as.Date(chemistry_limno$sample_date)

library(dplyr)

data_2017 <- chemistry_limno %>%
  filter(format(sample_date, "%Y") == "2017" & parameter_id == 34)

View(data_2017)

### Filter for sulfate only in 2017

sulfate_data$sample_date<- as.Date(sulfate_data$sample_date)

sulfate_data_2017<-sulfate_data%>%
  filter(format(sample_date, "%Y") == "2017" & parameter_id == 34)
View(sulfate_data_2017)

#######################                                            #######################################
########################################## Making Table joins#############################################
######################                                              ######################################
#######################################                       ############################################


############ First step is to add in cluster information data##############
##### I will add in the data the same way I added in the lakes data


# Open a file chooser window to select your CSV file manually

library(data.table)

cluster_information_limno <- fread(file.choose())

View(cluster_information_limno)

####### Joining data 
library(dplyr)



###### Decide what Colums to left join
colnames(sulfate_data)
colnames(cluster_information_limno)


### do left join
combined_data <- left_join(
  sulfate_data,
  cluster_information_limno,
  by = c("lagoslakeid", "cluster_id")
)

View(combined_data)

####################### My data table was too large so creating left joined data for only 2017

combined_data_2017 <- left_join(
  sulfate_data_2017,
  cluster_information_limno,
  by = c("lagoslakeid", "cluster_id")
)

View(combined_data_2017)


################################## Filter data  For 1.5 meters depth ##########################
###########################                                          ##################

depth_1.5m <- combined_data%>%
     filter(sample_depth_m == 1.5)
View(depth_1.5m)

depth_1.5m_2017 <- combined_data_2017%>%
  filter(sample_depth_m==1.5)
View(depth_1.5m_2017)

#######################                         #####################################
#######################Group by date, and site #######################################

#avg_sulfate <- combined_data %>%
#group_by(sample_date, source_sample_siteid) %>%   # group by date and site
  #summarise(mean_sulfate = mean(parameter_value, na.rm = TRUE))  # take average

avg_sufate <- depth_1.5m%>%
  group_by(sample_date,sample_id)%>%
  summarise(mean_sulfate = mean(parameter_value, na.rm = TRUE))
head(avg_sufate)

avg_sulfate_2017<- depth_1.5m_2017%>%
  group_by(sample_date,sample_id)%>%
  summarise(mean_sulfate = mean(parameter_value, na.rm = TRUE))

head(avg_sulfate_2017)

#### my output was weird , so I am exanming date data 

library(dplyr)

combined_data %>%
  summarise(
    earliest_date = min(sample_date, na.rm = TRUE),
    latest_date   = max(sample_date, na.rm = TRUE)
  )

system("git --version")



