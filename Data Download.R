#download data for exploring sulfate distribution

#from EDI: LAGOS metadate https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1439.5

library(readr)

#chemistry_limno
library(readr)
library(httr)

chemistry_limno <- read.csv(
  "https://portal.edirepository.org/nis/data/edi.1439.5/f4b1df4f4544ba0d015690c923bf9868?csv=true"
)


url <- "https://portal.edirepository.org/nis/data/edi/1439/5/f4b1df4f4544ba0d015690c923bf9868.csv"
#tmp <- tempfile(fileext = ".csv")
#GET(url,
   # write_disk(tmp, overwrite = TRUE),
    #user_agent("Mozilla/5.0"))
#chemistry_limno <- read_csv(tmp)

#tmp <- tempfile(fileext = ".csv")

#(url, tmp, mode = "wb")

#chemistry_limno <- read_csv(tmp)
library(readr)

url <- "https://portal.edirepository.org/nis/data/edi/1439/5/f4b1df4f4544ba0d015690c923bf9868.csv"

tmp <- tempfile(fileext = ".csv")

download.file(url, tmp, mode = "wb")

chemistry_limno <- read_csv(tmp)

tmp <- tempfile(fileext = ".csv")

download.file(url, tmp, mode = "wb")

chemistry_limno <- read_csv(tmp)

#parameter_id == 34 for sulfate


#sourcedata_limno
#sourcedata_limno <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=fdc599cf0501e13423e19860a503fff3")
#url2 <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=fdc599cf0501e13423e19860a503fff3&csv=true"
url2 <- "https://portal.edirepository.org/nis/data/edi.1439.5/fdc599cf0501e13423e19860a503fff3"
tmp2 <- tempfile(fileext = ".csv")

GET(url2,
    write_disk(tmp2, overwrite = TRUE),
    user_agent("Mozilla/5.0"))

sourcedata_limno <- read_csv(tmp2)

#cluster_information_limno
#A data table of coordinates and characteristics of each LIMNO site cluster
#and for all site clusters in the chemistry_limno.csv and the alldepths_allsites_limno.csv tables;
#also included are helpful variables from LAGOS-US LOCUS on lake area, lake name,
#and the states which the intersect the lake polygon

cluster_information_limno <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=824b259acddce3ebf5cb63aa5484127e")

#The User Guide for the LAGOS-US LIMNO module includes descriptions of
#the contents of all data and metadata tables and the methods and data sources used to
#create them. Specifically, the LAGOS US LIMNO user guide provides an overview of the
#LAGOS US data platform and its constituent modules followed by detailed descriptions
#of the LIMNO module structure, source material, data and metadata table structure,
#variable naming conventions and definitions, and methods used to create variables.
#https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=9e1078a9bfab8e3f8d323b796619e30c

#Data dictionary
#https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=0df262e8b73f9443b1bb626c012c3342

#parameter_description <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=e6b757d6411a54a88896a2fa9ad63ae5")
#url3 <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=e6b757d6411a54a88896a2fa9ad63ae5&csv=true"
url3 <- "https://portal.edirepository.org/nis/data/edi.1439.5/e6b757d6411a54a88896a2fa9ad63ae5"
tmp3 <- tempfile(fileext = ".csv")

GET(url3,
    write_disk(tmp3, overwrite = TRUE),
    user_agent("Mozilla/5.0"))

parameter_description <- read_csv(tmp3)


# my r has correctly download my files now I wnat to see them 
View(chemistry_limno)

#Check if It is empty 
dim(chemistry_limno)
