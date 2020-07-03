#Created by Curry Cunnungham

# NOTES =======================================================
# Difference between ASL_Mean_noAge.rds and ASL_Mean_noAge_NEW.rds:
#  1) Limits to only observations from escapement and commercial catch
#  2) Appends C or E to end of LocationID


#Get all summary data, not age structured
require(dplyr)
require(tidyverse)
require(reshape2)

#read this data in from KNB, Jeanette Clark, Rich Brenner, and Bert Lewis. 2018. 
#Compiled age, sex, and length data for Alaskan salmon, 1922-2017.
#Knowledge Network for Biocomplexity. doi:10.5063/F1707ZTM
#rename as needed

# master <- read.csv('/home/sjclark/ASL/ASL_master.csv', header=TRUE, stringsAsFactors=FALSE)
master <- data.table::fread('/home/sjclark/ASL/ASL_master.csv', header=T, stringsAsFactors=F, na.strings=c("","NA"), colClasses=c(Flag="character"))
head(master)


#Define working directory
wd <- paste0(getwd(),"/Manuscript_Code")
output.dir <- wd

#Remove Flagged Data and non MEHP
master <- master[is.na(master$Flag) &
                   master$LocationID!='unknown',]
master <- subset(master, Length.Measurement.Type == 'mid-eye to fork of tail' |Length.Measurement.Type == 'unknown'| is.na(Length.Measurement.Type) == TRUE)


#data summaries from master, I want averages of each trait, plus sample sizes for THAT trait
# master$year <- as.factor(master$sampleYear)

#==================================================================================
#Project Type Specific Data
# length_avg <- master %>% group_by(SASAP.Region, LocationID, sampleYear, Species, ASLProjectType) %>% 
#   summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n()) 

length_avg <- master %>% group_by(SASAP.Region, LocationID, sampleYear, Species, ASLProjectType, Lat, Lon) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n(),
             mean_fwAge=mean(Fresh.Water.Age, na.rm=TRUE), mean_oAge=mean(Salt.Water.Age, na.rm=TRUE)) %>% 
  filter(n>=10)

length_avg <- droplevels(subset(length_avg, length_avg$sampleYear!="NA"))
length_avg <- droplevels(subset(length_avg, !is.na(length_avg$sampleYear)))
# length_avg <- droplevels(subset(length_avg, length_avg$year!="NA", length_avg$year!="<NA>", !is.na(length_avg$year)))

# alt.length_avg <- length_avg %>% filter(sampleYear!="NA",!is.na(sampleYear)) #Same

length_avg <- data.frame(length_avg)
#Make year a numeric
# length_avg$year <- as.numeric(length_avg$year)

saveRDS(length_avg, file=paste0(output.dir,'/Data/ASL_Mean_noAge.rds'))

#==================================================================================
#Average Across Projects
length_avg_noType <- master %>% group_by(SASAP.Region, LocationID, sampleYear, Species) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n(),
             mean_fwAge=mean(Fresh.Water.Age, na.rm=TRUE), mean_oAge=mean(Salt.Water.Age, na.rm=TRUE)) %>% 
  filter(n>=10)

length_avg_noType <- droplevels(subset(length_avg_noType, length_avg_noType$sampleYear!="NA"))
length_avg_noType <- droplevels(subset(length_avg_noType, !is.na(length_avg_noType$sampleYear)))

length_avg_noType <- data.frame(length_avg_noType)
#Make year a numeric
# length_avg$year <- as.numeric(length_avg$year)

saveRDS(length_avg_noType, file=paste0(output.dir,'/Data/ASL_Mean_noAge_noType.rds'))

#==================================================================================
#Average Across Projects
length_avg_noage2 <- master %>% group_by(SASAP.Region, LocationID, ASLProjectType, sampleYear, Species) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n()) %>% 
  filter(n>=10)

length_avg_noage2 <- droplevels(subset(length_avg_noage2, length_avg_noage2$sampleYear!="NA"))
length_avg_noage2 <- droplevels(subset(length_avg_noage2, !is.na(length_avg_noage2$sampleYear)))

length_avg_noage2 <- data.frame(length_avg_noage2)

saveRDS(length_avg_noage2, file=paste0(output.dir,'/Data/ASL_Mean_noAge2.rds'))

#==================================================================================
#Average Across Projects
length_avg_noType_noLocationID <- master %>% group_by(SASAP.Region, sampleYear, Species) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n(),
             mean_fwAge=mean(Fresh.Water.Age, na.rm=TRUE), mean_oAge=mean(Salt.Water.Age, na.rm=TRUE)) %>% 
  filter(n>=10)
length_avg_noType_noLocationID <- droplevels(subset(length_avg_noType_noLocationID, length_avg_noType_noLocationID$sampleYear!="NA"))
length_avg_noType_noLocationID <- droplevels(subset(length_avg_noType_noLocationID, !is.na(length_avg_noType_noLocationID$sampleYear)))

length_avg_noType_noLocationID <- data.frame(length_avg_noType_noLocationID)
#Make year a numeric
# length_avg$year <- as.numeric(length_avg$year)

saveRDS(length_avg_noType_noLocationID, file=paste0(output.dir,'/Data/ASL_Mean_noAge_noType_noLocationID.rds'))

#==================================================================================
#Average Across locations within regions MAINTAINING PROJECT TYPE
length_avg_noLocationID <- master %>% group_by(SASAP.Region, sampleYear, Species, ASLProjectType) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n(),
             mean_fwAge=mean(Fresh.Water.Age, na.rm=TRUE), mean_oAge=mean(Salt.Water.Age, na.rm=TRUE)) %>% 
  filter(n>=10)
#length_avg_noLocationID <- droplevels(subset(length_avg_noLocationID, length_avg$sampleYear!="NA"))
#length_avg_noLocationID <- droplevels(subset(length_avg_noLocationID, !is.na(length_avg$sampleYear)))

length_avg_noLocationID <- data.frame(length_avg_noLocationID)
#Make year a numeric
# length_avg$year <- as.numeric(length_avg$year)

saveRDS(length_avg_noLocationID, file=paste0(output.dir,'/Data/ASL_Mean_noAge_noLocationID.rds'))


################
# Create Input for Drivers Analysis =======================================================
# Difference from ASL_Mean_noAge.rds:
#  1) Limits to only observations from escapement and commercial catch
#  2) Appends C or E to end of LocationID

#Contains average length by location, year, project type
length_avg_ce <- master %>% group_by(SASAP.Region, LocationID, sampleYear, Species, ASLProjectType, Lat, Lon) %>% 
  filter(ASLProjectType %in% c('escapement','commercial catch')) %>% 
  summarize( mean_length=mean(Length, na.rm=TRUE), sd_length=sd(Length, na.rm=TRUE), n=n(),
             mean_fwAge=mean(Fresh.Water.Age, na.rm=TRUE), mean_oAge=mean(Salt.Water.Age, na.rm=TRUE)) %>% 
  filter(n>=10)

dim(length_avg_ce)

length_avg_ce <- droplevels(subset(length_avg_ce, length_avg_ce$sampleYear!="NA"))
length_avg_ce <- droplevels(subset(length_avg_ce, !is.na(length_avg_ce$sampleYear)))

# length_avg_ce <- length_avg %>% filter(sampleYear!="NA",!is.na(sampleYear)) #Same

#Rename - Append a C or E to project
length_avg_ce$LocationID <- paste0(length_avg_ce$LocationID, "_", ifelse(length_avg_ce$ASLProjectType=='escapement','E','C'))


length_avg_ce <- data.frame(length_avg_ce)
#Make year a numeric
# length_avg$year <- as.numeric(length_avg$year)

saveRDS(length_avg_ce, file=paste0(output.dir,'/Data/ASL_Mean_noAge_NEW.rds'))

