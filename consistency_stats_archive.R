#==================================================================================================
#Project Name: Consistency
#Creator: Krista Oke
#
#
#
#
#==================================================================================================
#NOTES:
#
#
#==================================================================================================
require(tidyverse)
require(lme4)
require(gamm4)
#require(reshape2)
require(itsadug)
require(visreg)
require(pscl)
require(mgcv)
require(viridis)
library(ggplot2)
library(data.table)




wd <- paste0(getwd(),"/Manuscript_Code") #

source(paste0(wd,"/subset-minLength-yrs.R"))
source(paste0(wd,"/rename-LocationID-multiples.R"))

output.dir <- paste0(wd,"/Output")
figs.dir <- paste0(wd,"/Figs")
#===============================================================================
##### CONTROL SECTION #####

#Minimum Number of Years
n.min.years <- 0
n.min.years <- 20 #ran most at 10 and 1970

#Specify Start Year
start.year <- 1970


#===============================================================================
#Load and Clean Data

all.dat <- readRDS(paste0(wd,"/Data/ASL_Mean_noAge.rds"))
#all.dat <- na.omit(all.dat) #lat and long have a lot of NAs so this was pulling out too much data
all.dat <- all.dat[all.dat$LocationID!='unknown' &
                     all.dat$sampleYear>=start.year &
                     all.dat$mean_length!="NA" &
                     all.dat$mean_length!="NaN",]

#===============================================================================
#Rename Location ID's that appear in more than one Region
species.dat <- rename_LocationID_multiples(all.dat)
#species.dat <- dtbl

#===============================================================================
#Subset data to only include locations with >25 years of data
species.dat <- subset_minLength_yrs(species.dat, n.min.yrs=n.min.years)

#===============================================================================
#Convert to factors
species.dat$SASAP.Region <- as.factor(species.dat$SASAP.Region)
species.dat$LocationID <- as.factor(species.dat$LocationID)
species.dat$Species <- as.factor(species.dat$Species)
species.dat$ASLProjectType <- as.factor(species.dat$ASLProjectType)

#===============================================================================
#Limit to ONLY Catch and Escapement

species.dat <- species.dat[species.dat$ASLProjectType=='escapement' | species.dat$ASLProjectType=='commercial catch',]

species.dat <- species.dat[species.dat$Species!="pink",]
species.dat <- species.dat[is.finite(species.dat$Species)==TRUE,]

esc <- droplevels(subset(species.dat, species.dat$ASLProjectType=='escapement'))
cat <- droplevels(subset(species.dat, species.dat$ASLProjectType=='commercial catch'))

esc <- droplevels(subset(esc, esc$Species!='pink'))
cat <- droplevels(subset(cat, cat$Species!='pink'))

#=====================

dat.sock <- species.dat[species.dat$Species=='sockeye',]
dat.sock <- droplevels(subset_minLength_yrs(n.min.yrs=20, dat.sock))

dat.ck <- species.dat[species.dat$Species=='chinook',]
dat.ck <- droplevels(subset_minLength_yrs(n.min.yrs=20, dat.ck))

dat.chum <- species.dat[species.dat$Species=='chum',]
dat.chum <- droplevels(subset_minLength_yrs(n.min.yrs=20, dat.chum))

dat.coho <- species.dat[species.dat$Species=='coho',]
dat.coho <- droplevels(subset_minLength_yrs(n.min.yrs=20, dat.coho))

#=====================
#TABLE######

#chinook
chinook_yr <- gam(mean_length ~  LocationID + SASAP.Region + 
                    s(sampleYear) ,
                  data=dat.ck)
summary(chinook_yr)
anova(chinook_yr)
anova(chinook_yr)$s.table

chinook_loc <- gam(mean_length ~  LocationID + SASAP.Region + 
                     
                     s(sampleYear, LocationID, bs='fs') ,
                   data=dat.ck)
summary(chinook_loc)
anova(chinook_loc)
anova(chinook_loc)$s.table

chinook_reg <- gam(mean_length ~  LocationID + SASAP.Region + 
                     
                     s(sampleYear, SASAP.Region, bs='fs'),
                   data=dat.ck)
summary(chinook_reg)
anova(chinook_reg)
anova(chinook_reg)$s.table

AIC(chinook_yr, chinook_loc, chinook_reg)


#sockeye
sockeye_yr <- gam(mean_length ~  LocationID + SASAP.Region + 
                    s(sampleYear) ,
                  data=dat.sock)
summary(sockeye_yr)
anova(sockeye_yr)
anova(sockeye_yr)$s.table

sockeye_loc <- gam(mean_length ~  LocationID + SASAP.Region + 
                     
                     s(sampleYear, LocationID, bs='fs') ,
                   data=dat.sock)
summary(sockeye_loc)
anova(sockeye_loc)
anova(sockeye_loc)$s.table

sockeye_reg <- gam(mean_length ~  LocationID + SASAP.Region + 
                     
                     s(sampleYear, SASAP.Region, bs='fs'),
                   data=dat.sock)
summary(sockeye_reg)
anova(sockeye_reg)
anova(sockeye_reg)$s.table

AIC(sockeye_yr, sockeye_loc, sockeye_reg)


#chum
chum_yr <- gam(mean_length ~  LocationID + SASAP.Region + 
                 s(sampleYear) ,
               data=dat.chum)
summary(chum_yr)
anova(chum_yr)
anova(chum_yr)$s.table

chum_loc <- gam(mean_length ~  LocationID + SASAP.Region + 
                  
                  s(sampleYear, LocationID, bs='fs') ,
                data=dat.chum)
summary(chum_loc)
anova(chum_loc)
anova(chum_loc)$s.table

chum_reg <- gam(mean_length ~  LocationID + SASAP.Region + 
                  
                  s(sampleYear, SASAP.Region, bs='fs'),
                data=dat.chum)
summary(chum_reg)
anova(chum_reg)
anova(chum_reg)$s.table

AIC(chum_yr, chum_loc, chum_reg)


#coho
coho_yr <- gam(mean_length ~  LocationID + SASAP.Region + 
                 s(sampleYear) ,
               data=dat.coho)
summary(coho_yr)
anova(coho_yr)
anova(coho_yr)$s.table

coho_loc <- gam(mean_length ~  LocationID + SASAP.Region + 
                  
                  s(sampleYear, LocationID, bs='fs') ,
                data=dat.coho)
summary(coho_loc)
anova(coho_loc)
anova(coho_loc)$s.table

coho_reg <- gam(mean_length ~  LocationID + SASAP.Region + 
                  
                  s(sampleYear, SASAP.Region, bs='fs'),
                data=dat.coho)
summary(coho_reg)
anova(coho_reg)
anova(coho_reg)$s.table

AIC(coho_yr, coho_loc, coho_reg)





#=== Year only smoothers ==========================================================================

#==================================================================================================
library(visreg)
library(ggplot2)
library(mgcv)


#Load and Clean Data
all.dat2 <- readRDS(paste0(wd,"/Data/ASL_Mean_noAge2.rds"))
start.year <- 1975
#all.dat <- na.omit(all.dat) #lat and long have a lot of NAs so this was pulling out too much data
all.dat2 <- all.dat2[all.dat2$LocationID!='unknown' &
                       all.dat2$sampleYear>=start.year &
                       all.dat2$mean_length!="NA" &
                       all.dat2$mean_length!="NaN",]


#===============================================================================
#Rename Location ID's that appear in more than one Region
species.dat <- rename_LocationID_multiples(all.dat2)

#

#===============================================================================
#Subset data to only include locations with >25 years of data
n.min.years <- 5 #update as needed
species.dat <- subset_minLength_yrs(species.dat, n.min.yrs=n.min.years)

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
chinook.dat <- subset_minLength_yrs(chinook.dat , n.min.yrs=n.min.years)
chum.dat <- species.dat[species.dat$Species=='chum',]
chum.dat <- subset_minLength_yrs(chum.dat , n.min.yrs=n.min.years)
coho.dat <- species.dat[species.dat$Species=='coho',]
coho.dat <- subset_minLength_yrs(coho.dat , n.min.yrs=n.min.years)


allsockyr <- gam(mean_length ~  LocationID + SASAP.Region + 
                   s(sampleYear),
                 data=sockeye.dat[which(sockeye.dat$sampleYear>1974),])
allchinyr <- gam(mean_length ~  LocationID + SASAP.Region + 
                   s(sampleYear),
                 data=chinook.dat[which(chinook.dat$sampleYear>1974),])
allchumyr <- gam(mean_length ~  LocationID + SASAP.Region + 
                   s(sampleYear),
                 data=chum.dat[which(chum.dat$sampleYear>1974),])
allcohoyr <- gam(mean_length ~  LocationID + SASAP.Region + 
                   s(sampleYear),
                 data=coho.dat[which(coho.dat$sampleYear>1974),])










