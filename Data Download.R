#download data for exploring sulfate distribution

#from EDI: LAGOS metadate https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1439.5


#chemistry_limno
chemistry_limno<- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=f4b1df4f4544ba0d015690c923bf9868")
#parameter_id == 34 for sulfate


#sourcedata_limno
sourcedata_limno <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=fdc599cf0501e13423e19860a503fff3")

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

parameter_description <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1439.5&entityid=e6b757d6411a54a88896a2fa9ad63ae5")
