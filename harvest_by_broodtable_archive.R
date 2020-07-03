#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Harvest rate calculated from brood tables
#created by Krista Oke
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require(car)
require(heplots)
require(tidyverse)

#Import brood tables

brood_sockeye <- read.csv('https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Ac747eee7-89fd-44ba-bce1-a14d0670792d',
                          header=TRUE, stringsAsFactors=FALSE)

brood_chinook <- read.csv('https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A2dd0cf28-faf1-4b16-ab31-28c792e75d92',
                          header=TRUE, stringsAsFactors=FALSE)

brood_coho <- read.csv('https://knb.ecoinformatics.org/knb/d1/mn/v2/object/knb.92354.1',
                       header=TRUE, stringsAsFactors=FALSE)


#===============================================================


#Only use data that has not been flagged

brood_sockeye <- droplevels(subset(brood_sockeye, brood_sockeye$UseFlag!="0"))
brood_chinook <- droplevels(subset(brood_chinook, brood_chinook$UseFlag!="0"))
brood_coho <- droplevels(subset(brood_coho, brood_coho$UseFlag!="0"))


#remove Washington and BC data

brood_sockeye <- droplevels(subset(brood_sockeye, brood_sockeye$Region!="WA"))
brood_sockeye <- droplevels(subset(brood_sockeye, brood_sockeye$Region!="Fraser River"))
brood_sockeye <- droplevels(subset(brood_sockeye, brood_sockeye$Region!="BC North"))
brood_chinook <- droplevels(subset(brood_chinook, brood_chinook$Region!="WA"))
brood_chinook <- droplevels(subset(brood_chinook, brood_chinook$Region!="Fraser River"))
brood_chinook <- droplevels(subset(brood_chinook, brood_chinook$Region!="BC North"))
brood_coho <- droplevels(subset(brood_coho, brood_coho$Region!="WA"))
brood_coho <- droplevels(subset(brood_coho, brood_coho$Region!="Fraser River"))
brood_coho <- droplevels(subset(brood_coho, brood_coho$Region!="BC North"))


#subset just necessarly columns
brood_sockeye <- brood_sockeye[,-c(1,2,4,6,7,8,9)]
brood_chinook <- brood_chinook[,-c(1,2)]
brood_coho <- brood_coho[,-c(1,2)]


#Turn brood tables


Gbrood_sockeye <- brood_sockeye %>% gather(key="ageclass", value="N_in_ageclass", -"Stock", -"Region", -"BroodYear", 
                                           -"TotalEscapement", -"TotalRecruits", -"UseFlag")

Gbrood_chinook <- brood_chinook %>% gather(key="ageclass", value="N_in_ageclass", -"Stock", -"Region", -"BroodYear", 
                                           -"TotalEscapement", -"TotalRecruits", -"UseFlag")

Gbrood_coho <- brood_coho %>% gather(key="ageclass", value="N_in_ageclass", -"Stock", -"Region", -"BroodYear", 
                                     -"TotalEscapement", -"TotalRecruits", -"UseFlag")




#create column for total age

Gbrood_sockeye$yrs <- NA
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R0.1")] <- 2
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R0.2")] <- 3
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R0.3")] <- 4
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R0.4")] <- 5
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R0.5")] <- 6
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R1.1")] <- 3
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R1.2")] <- 4
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R1.3")] <- 5
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R1.4")] <- 6
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R1.5")] <- 7
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R2.1")] <- 4
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R2.2")] <- 5
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R2.3")] <- 6
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R2.4")] <- 7
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R2.5")] <- 8
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R3.1")] <- 5
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R3.2")] <- 6
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R3.3")] <- 7
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R3.4")] <- 8
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R3.5")] <- 9
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R4.1")] <- 6
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R4.2")] <- 7
Gbrood_sockeye$yrs[which(Gbrood_sockeye$ageclass=="R4.3")] <- 8


Gbrood_chinook$yrs <- NA
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.1")] <- 2
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.2")] <- 3
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.3")] <- 4
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.4")] <- 5
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.5")] <- 6
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R0.6")] <- 7
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.1")] <- 3
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.2")] <- 4
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.3")] <- 5
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.4")] <- 6
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.5")] <- 7
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R1.6")] <- 8
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R2.1")] <- 4
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R2.2")] <- 5
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R2.3")] <- 6
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R2.4")] <- 7
Gbrood_chinook$yrs[which(Gbrood_chinook$ageclass=="R2.5")] <- 8


Gbrood_coho$yrs <- NA
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R0.1")] <- 2
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R1.1")] <- 3
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R1.2")] <- 4
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R2.0")] <- 3
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R2.1")] <- 4
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R2.2")] <- 5
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R3.0")] <- 4
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R3.1")] <- 5
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R3.2")] <- 6
Gbrood_coho$yrs[which(Gbrood_coho$ageclass=="R4.1")] <- 6


Gbrood_sockeye$ReturnYear <- Gbrood_sockeye$BroodYear + Gbrood_sockeye$yrs
Gbrood_chinook$ReturnYear <- Gbrood_chinook$BroodYear + Gbrood_chinook$yrs
Gbrood_coho$ReturnYear <- Gbrood_coho$BroodYear + Gbrood_coho$yrs

returns_sockeye <- Gbrood_sockeye %>% group_by(Stock, ReturnYear) %>% 
  summarize(summed_returns = sum(N_in_ageclass, na.rm=TRUE))

returns_chinook <- Gbrood_chinook %>% group_by(Stock, ReturnYear) %>% 
  summarize(summed_returns = sum(N_in_ageclass, na.rm=TRUE))

returns_coho <- Gbrood_coho %>% group_by(Stock, ReturnYear) %>% 
  summarize(summed_returns = sum(N_in_ageclass, na.rm=TRUE))

sub_yr_sockeye <- Gbrood_sockeye[,c("Stock", "BroodYear", "TotalEscapement")]
sub_yr_chinook <- Gbrood_chinook[,c("Stock",  "BroodYear", "TotalEscapement")]
sub_yr_coho <- Gbrood_coho[,c("Stock", "BroodYear", "TotalEscapement")]

names(sub_yr_sockeye) <- c("Stock", "Year", "TotalEscapement")
names(sub_yr_chinook) <- c("Stock", "Year", "TotalEscapement")
names(sub_yr_coho) <- c("Stock", "Year", "TotalEscapement")

names(returns_sockeye) <- c("Stock",  "Year" , "summed_returns")
names(returns_chinook) <- c("Stock", "Year" , "summed_returns")
names(returns_coho) <- c("Stock",  "Year" , "summed_returns")

returns_esc_sockeye <- left_join(sub_yr_sockeye, returns_sockeye)
returns_esc_sockeye$harvest <- returns_esc_sockeye$summed_returns - returns_esc_sockeye$TotalEscapement
returns_esc_sockeye$harvest_rate <- returns_esc_sockeye$harvest / returns_esc_sockeye$summed_returns



returns_esc_chinook <- left_join(sub_yr_chinook, returns_chinook)
returns_esc_chinook$harvest <- returns_esc_chinook$summed_returns - returns_esc_chinook$TotalEscapement
returns_esc_chinook$harvest_rate <- returns_esc_chinook$harvest / returns_esc_chinook$summed_returns


returns_esc_coho <- left_join(sub_yr_coho, returns_coho)
returns_esc_coho$harvest <- returns_esc_coho$summed_returns - returns_esc_coho$TotalEscapement
returns_esc_coho$harvest_rate <- returns_esc_coho$harvest / returns_esc_coho$summed_returns


#average recent years to make sure no incomplete early years are included
#and cut off at 2010 because that's the last complete year

returns_esc_sockeye_90 <- returns_esc_sockeye[which(returns_esc_sockeye$Year>1989 &
                                                      returns_esc_sockeye$Year<2011),]

avg_hr_sock <- returns_esc_sockeye_90 %>% group_by(Stock) %>% 
  summarize(avg_HR = mean(harvest_rate, na.rm=TRUE), SD_HR=sd(harvest_rate, na.rm=TRUE))

#cut off at 2008 because that's the last complete year
returns_esc_chinook_90 <- returns_esc_chinook[which(returns_esc_chinook$Year>1989 &
                                                      returns_esc_chinook$Year<2008),]
returns_esc_chinook_90 <- returns_esc_chinook_90[which(is.finite(returns_esc_chinook_90$harvest_rate)),]
avg_hr_chin <- returns_esc_chinook_90 %>% group_by(Stock) %>% 
  summarize(avg_HR = mean(harvest_rate, na.rm=TRUE), SD_HR=sd(harvest_rate, na.rm=TRUE))

#this method unreliable for Andrew Creek, Chickamin, Chilkat, King Salmon River, Stikine


#Other harvest rate estimates========================================================

otherHR <- read.csv(paste0(wd,"/Data/2018-08-13_chinook_exploitation_rates.csv"),
                    header=TRUE, stringsAsFactors=FALSE)
otherHR90_10 <- otherHR[which(otherHR$Year>1989 &
                                otherHR$Year<2011),]

avg_hr_other <- otherHR90_10 %>% group_by(LocationID, Species) %>% 
  summarize(avg_HR = mean((Exploitationrate_percent/100), na.rm=TRUE), SD_HR=sd((Exploitationrate_percent/100), na.rm=TRUE))



# #====================================================================================
# 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Slopes from lms====

#*#
#==================================================================================================
#Load and Clean Data

#wd <- paste0(getwd(),"/Manuscript_Code")
source(paste0(getwd(),"/R/subset-minLength-yrs.R"))
source(paste0(getwd(),"/R/rename-LocationID-multiples.R"))

start.year <- 1960

all.dat <- readRDS(paste0(wd,"/Data/ASL_Mean_noAge.rds"))
#all.dat <- na.omit(all.dat) #lat and long have a lot of NAs so this was pulling out too much data
all.dat <- all.dat[all.dat$LocationID!='unknown' &
                     all.dat$sampleYear>=start.year &
                     all.dat$mean_length!="NA" &
                     all.dat$mean_length!="NaN",]


#===============================================================================
#Rename Location ID's that appear in more than one Region
species.dat <- rename_LocationID_multiples(all.dat)



#===============================================================================
#Subset data to only include locations with >25 years of data
n.min.years <- 25
n.min.years.chin <- 10 
#species.dat <- subset_minLength_yrs(species.dat, n.min.yrs=n.min.years)

#===============================================================================
#Convert to factors
species.dat$SASAP.Region <- as.factor(species.dat$SASAP.Region)
species.dat$LocationID <- as.factor(species.dat$LocationID)
species.dat$Species <- as.factor(species.dat$Species)
species.dat$ASLProjectType <- as.factor(species.dat$ASLProjectType)

#===============================================================================
#Limit to ONLY Catch and Escapement



species.dat <- species.dat[species.dat$ASLProjectType=='escapement' | species.dat$ASLProjectType=='commercial catch',]
sockeye.dat <- species.dat[species.dat$Species=='sockeye',]
sockeye.dat <- subset_minLength_yrs(sockeye.dat , n.min.yrs=n.min.years)
chinook.dat <- species.dat[species.dat$Species=='chinook',]
#chinook.dat <- subset_minLength_yrs(chinook.dat , n.min.yrs=n.min.years)
chinook.dat <- subset_minLength_yrs(chinook.dat , n.min.yrs=n.min.years.chin)
chum.dat <- species.dat[species.dat$Species=='chum',]
chum.dat <- subset_minLength_yrs(chum.dat , n.min.yrs=n.min.years)
coho.dat <- species.dat[species.dat$Species=='coho',]
coho.dat <- subset_minLength_yrs(coho.dat , n.min.yrs=n.min.years)
#*#

#lm fits for change======


locs <- unique(sockeye.dat$LocationID)
slope_df <- data.frame(matrix(NA, nrow = length(locs), ncol = 3))

i <- 1
counter <- 1
for (i in 1:length(locs)) {
  tempdat <- sockeye.dat[which(sockeye.dat$LocationID==locs[i]),]
  temploc <- locs[i]
  
  mod <- lm(mean_length ~ sampleYear, data=tempdat[which(tempdat$sampleYear > 1989 & tempdat$sampleYear < 2011),])
  mod_age <- lm(mean_oAge ~ sampleYear, data=tempdat[which(tempdat$sampleYear > 1989 & tempdat$sampleYear < 2011),])
  
  slope_df[counter,] <- c(as.character(temploc), mod$coefficients[2], mod_age$coefficients[2])
  
  counter <- counter + 1
}


names(slope_df) <- c("LocationID", "slope", "age_slope")
slope_df$slope <- as.numeric(slope_df$slope)
slope_df$age_slope <- as.numeric(slope_df$age_slope)
slope_df$LocationID <- as.factor(slope_df$LocationID)

slope_df_subset <- slope_df[which(slope_df$LocationID=="Ayakulik River"|
                                    slope_df$LocationID=="Black Lake"|
                                    slope_df$LocationID=="Bear River"|
                                    slope_df$LocationID=="Chilkoot River"|
                                    slope_df$LocationID=="Coghill River"|
                                    slope_df$LocationID=="Coghill District"|
                                    slope_df$LocationID=="Egegik River"|
                                    slope_df$LocationID=="Egegik District"|
                                    slope_df$LocationID=="Eshamy River"|
                                    slope_df$LocationID=="Eshamy Bay"|
                                    slope_df$LocationID=="Eshamy District"|
                                    slope_df$LocationID=="Frazer Lake"|
                                    slope_df$LocationID=="Goodnews River (Middle Fork)"|
                                    slope_df$LocationID=="W5 (Goodnews Bay Subdistrict)"|
                                    slope_df$LocationID=="Igushik River"|
                                    slope_df$LocationID=="Kasilof River"|
                                    slope_df$LocationID=="Kenai River"|
                                    slope_df$LocationID=="Kvichak River"|
                                    slope_df$LocationID=="Naknek-Kvichak District"|
                                    slope_df$LocationID=="Naknek River"|                 
                                    slope_df$LocationID=="Nelson River"|
                                    slope_df$LocationID=="Nushagak River"|
                                    slope_df$LocationID=="Nushagak District"|
                                    slope_df$LocationID=="Redoubt Lake"|
                                    slope_df$LocationID=="Togiak River"|
                                    slope_df$LocationID=="Togiak District"|
                                    slope_df$LocationID=="Ugashik River"|
                                    slope_df$LocationID=="Ugashik District"|
                                    slope_df$LocationID=="Wood River"),]

slope_df_subset$Stock <- NA
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Ayakulik River")] <- "Ayakulik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Black Lake")] <- "Black Lake"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Bear River")] <- "Bear"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Chilkoot River")] <- "Chilkoot"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Coghill River")] <- "Coghill"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Coghill District")] <- "Coghill"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Egegik River")] <- "Egegik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Egegik District")] <- "Egegik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Eshamy River")] <- "Eshamy"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Eshamy Bay")] <- "Eshamy"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Eshamy District")] <- "Eshamy"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Frazer Lake")] <- "Frazer"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Goodnews River (Middle Fork)")] <- "Goodnews"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="W5 (Goodnews Bay Subdistrict)")] <- "Goodnews"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Igushik River")] <- "Igushik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Kasilof River")] <- "Kasilof"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Kenai River")] <- "Kenai"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Kvichak River")] <- "Kvichak"
#slope_df_subset$Stock[which(slope_df_subset$LocationID=="Naknek-Kvichak District")] <- ""
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Naknek River" )] <- "Naknek"              
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Nelson River")] <- "Nelson"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Nushagak River")] <- "Nushagak"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Nushagak District")] <- "Nushagak"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Redoubt Lake")] <- "Redoubt"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Togiak River")] <- "Togiak"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Togiak District")] <- "Togiak"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Ugashik River")] <- "Ugashik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Ugashik District")] <- "Ugashik"
slope_df_subset$Stock[which(slope_df_subset$LocationID=="Wood River")] <- "Wood"


slope_df_subset <- droplevels(subset(slope_df_subset, slope_df_subset$Stock!="Eshamy"))
slope_df_subset <- droplevels(subset(slope_df_subset, slope_df_subset$Stock!="Early Karluk"))
slope_df_subset <- droplevels(subset(slope_df_subset, slope_df_subset$Stock!="Late Karluk"))
slope_df_subset <- droplevels(subset(slope_df_subset, slope_df_subset$Stock!="Early Upper Station"))
slope_df_subset <- droplevels(subset(slope_df_subset, slope_df_subset$Stock!="Late Upper Station"))



# avg_hr <- returns_esc_sockeye[which(returns_esc_sockeye$Year>1989),] %>% group_by(Stock) %>% 
#   summarize(avg_HR = mean(harvest_rate, na.rm=TRUE))


lm_slopes_harvest  <- left_join(avg_hr_sock, slope_df_subset)

mod1 <- lm(slope ~ avg_HR , data=lm_slopes_harvest)
summary(mod1)
Anova(mod1)



#Repeat for other species====
#for Chinook


#lm fits for change======



chinook_loopdat <- chinook.dat[,c("sampleYear", "LocationID", "Species", "SASAP.Region", "mean_length", "mean_fwAge", "mean_oAge" )]
#

locs <- unique(chinook_loopdat$LocationID[which(chinook_loopdat$sampleYear > 1989)])
chinook_slope_df <- data.frame(matrix(NA, nrow = length(locs) + 1, ncol = 3)) 

i <- 1
counter <- 1
for (i in 1:length(locs)) {
  tempdat <- chinook_loopdat[which(chinook_loopdat$LocationID==locs[i]),]
  temploc <- locs[i]
  
  mod <- lm(mean_length ~ sampleYear, data=tempdat[which(tempdat$sampleYear > 1989 & tempdat$sampleYear < 2009),], na.action = na.omit)
  mod_age <- lm(mean_oAge ~ sampleYear, data=tempdat[which(tempdat$sampleYear > 1989 & tempdat$sampleYear < 2009),], na.action = na.omit)
  
  chinook_slope_df[counter,] <- c(as.character(temploc), mod$coefficients[2], mod_age$coefficients[2])
  
  counter <- counter + 1
}


names(chinook_slope_df) <- c("LocationID", "slope", "age_slope")
chinook_slope_df$slope <- as.numeric(chinook_slope_df$slope)
chinook_slope_df$age_slope <- as.numeric(chinook_slope_df$age_slope)
chinook_slope_df$LocationID <- as.factor(chinook_slope_df$LocationID)



#brood table method unreliable for Andrew Creek, Chickamin, Chilkat, King Salmon River, Stikine
#need ASL for Stikine, Chilkat, King Salmon River, Unuk

chinook_df_subset <- chinook_slope_df[which(chinook_slope_df$LocationID=="Nushagak River"|
                                              chinook_slope_df$LocationID=="Nushagak District"|
                                              chinook_slope_df$LocationID=="Bethel (Village/City)"|
                                              chinook_slope_df$LocationID=="Alsek River"|
                                              chinook_slope_df$LocationID=="Situk River"|
                                              chinook_slope_df$LocationID=="Unuk"|
                                              chinook_slope_df$LocationID=="Chilkat River"|
                                              chinook_slope_df$LocationID=="Stikine River"| 
                                              chinook_slope_df$LocationID=="Taku River"),]




#CHANGE LOCATION NAMES FOR CHINOOK
chinook_df_subset$Stock <- NA
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Nushagak River")] <- "Nushagak"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Nushagak District")] <- "Nushagak"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Bethel (Village/City)")] <- "Kuskokwim"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Alsek River")] <- "Alsek River"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Situk River")] <- "Situk River"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Taku River")] <- "Taku River"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Unuk")] <- "Unuk River"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Chilkat River")] <- "Chilkat River"
chinook_df_subset$Stock[which(chinook_df_subset$LocationID=="Stikine River")] <- "Stikine River"


unique( avg_hr_chin$Stock)
unique( avg_hr_other$LocationID)

chin_harvest1  <- left_join(avg_hr_chin,  chinook_df_subset)
chin_harvest2  <- left_join(avg_hr_other[which(avg_hr_other$Species=="chinook"),],  chinook_df_subset)

chin_harvest1 <- as.data.frame(na.omit(chin_harvest1[,c("Stock", "LocationID", "avg_HR", "slope", "age_slope", "SD_HR")]))
chin_harvest2 <- as.data.frame(na.omit(chin_harvest2[,c("Stock", "LocationID", "avg_HR", "slope", "age_slope", "SD_HR")]))

chin_harvest <- rbind(chin_harvest1, chin_harvest2)

#chin_harvest <- left_join( na.omit(chin_harvest1), na.omit(chin_harvest2), by="Stock")
#re-arrange and rbind





mod_c <- lm(slope ~ avg_HR, data=chin_harvest)
summary(mod_c)
Anova(mod_c)
etasq(mod_c)

mod_ca <- lm(age_slope ~ avg_HR, data=chin_harvest)
summary(mod_ca)
Anova(mod_ca)

c1 <- ggplot(chin_harvest, aes(avg_HR, slope))
c1 + geom_point(aes(colour=Stock)) + geom_smooth(method="lm")

c2 <- ggplot(chin_harvest, aes(avg_HR, age_slope))
c2 + geom_point(aes(colour=Stock)) + geom_smooth(method="lm")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#stats on lm slopes-----


mod_1 <- lm(slope ~ avg_HR, data=lm_slopes_harvest)
summary(mod_1)
Anova(mod_1)
etasq(mod_1)

gg2 <- ggplot(lm_slopes_harvest, aes(avg_HR, slope))
gg2 + geom_point(aes(colour=Stock)) + geom_smooth(method="lm")



mod_2 <- lm(age_slope ~ avg_HR, data=lm_slopes_harvest)
summary(mod_2)
Anova(mod_2)
etasq(mod_2)

gg2 <- ggplot(lm_slopes_harvest, aes(avg_HR, age_slope))
gg2 + geom_point(aes(colour=Stock)) + geom_smooth(method="lm")


table(lm_slopes_harvest$Stock)



pl1 <- ggplot(sockeye.dat, aes(sampleYear, mean_length))
pl1 + geom_point() + geom_smooth(method="lm") + facet_wrap(~LocationID)



#Both together====
library(visreg)

chin_harvest$Species <- "Chinook"
lm_slopes_harvest$Species <- "sockeye"  

chin_harvest_3 <- chin_harvest[,c("Stock", "avg_HR", "LocationID", "slope", "age_slope", "Species", "SD_HR")]
#chin_harvest_3$avg_HR <- chin_harvest_3$avg_HR/100

both_dat <- rbind(chin_harvest_3, lm_slopes_harvest)  


modb <- lm(slope ~ avg_HR + Species, data=both_dat)
summary(modb)
Anova(modb)
etasq(modb)

modb_a <- lm(age_slope ~ avg_HR + Species, data=both_dat)
summary(modb_a)
Anova(modb_a)
etasq(modb_a)
visreg(modb_a)

ggb <- ggplot(both_dat, aes(avg_HR, slope, fill=Species))
ggb + geom_point(aes(colour=Stock)) + geom_smooth(method="lm")

ggb <- ggplot(both_dat, aes(avg_HR, slope))
ggb + geom_point() + geom_smooth(method="lm") + facet_wrap(~Species) +
  xlab("Average harvest rate") + ylab("Change in length") + theme_bw()+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))

ggb <- ggplot(both_dat, aes(avg_HR, age_slope))
ggb + geom_point() + geom_smooth(method="lm") + facet_wrap(~Species) +
  xlab("Average harvest rate") + ylab("Change in SW age") + theme_bw()

#plot both chinook and sockeye using model fit
visreg(modb, "avg_HR", partial=FALSE, gg=TRUE, rug=FALSE) + theme_bw()+ 
  geom_errorbarh(data=both_dat,aes(xmin=avg_HR-SD_HR, xmax=avg_HR+SD_HR, x=avg_HR, y=slope)) +
  geom_point(data=both_dat, aes(avg_HR, slope, colour=Species)) + ylab("Change in length (mm per year)") +
  xlab("Average harvest rate") + theme(panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(),  legend.position=c(0.85, 0.15)) +
  scale_color_manual(labels = c("Chinook", 
                                "Sockeye"), 
                     values = c("Chinook"="grey29", 
                                "sockeye"="#009E73"))  + coord_cartesian(xlim = c(0, 0.9))


