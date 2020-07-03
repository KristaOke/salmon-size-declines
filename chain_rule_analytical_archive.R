#chain rule analytical
#Created by Krista Oke with extensive help from Steve Munch and Jared Kibele

library(dtplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(devtools)

wd <- getwd()

source(paste0(getwd(),"/Manuscript_Code/subset-minLength-yrs.R")) #


#START HERE
fw_asl <- read.csv(paste0(wd,'/Manuscript_Code/Data/2019_12_16_ASL_summary_by_FWSWage.csv'), header=TRUE, stringsAsFactors=FALSE)  #
fw_asl <- fw_asl[,-1]

escapement <- droplevels(subset(fw_asl, fw_asl$ASLProjectType=="escapement"))
#escapement <- escapement[,-12]
catch <- droplevels(subset(fw_asl, fw_asl$ASLProjectType=="commercial catch"))

both <- fw_asl[which(fw_asl$ASLProjectType=="escapement" |
                       fw_asl$ASLProjectType=="commercial catch"),]




#BY LOCATION



#remove old results
if (exists("locchange_outdf")) rm(locchange_outdf)

#choose data to input
#input_dat <- escapement[,-c(8,9,10,11,13,14,15)]
input_dat <- both[,-c(8,9,10,11,13,14,15)]

input_dat <- droplevels(subset_minLength_yrs(n.min.yrs=5, input_dat))


#ACTUAL ANALYSES##########

sps <- na.omit(unique(input_dat$Species))
sps <- sps[which(sps!="pink")]

for (v in 1:length(sps)){
  temp_sps <- sps[v]
  print(temp_sps)
  
  #control section comment out as needed---
  
  species_dat <- droplevels(subset(input_dat, input_dat$Species==temp_sps))
  
  species_dat <- droplevels(subset_minLength_yrs(n.min.yrs=5, species_dat))
  
  #
  dat_years <- na.omit(species_dat) %>% group_by(SASAP.Region, Species, ASLProjectType, LocationID) %>%
    summarize(lengthyrs=length(unique(sampleYear)), minyr=min(sampleYear), maxyr=max(sampleYear))
  dat_years$max_min <- dat_years$maxyr - dat_years$minyr
  
  # nyrs <- 2016-1970
  # dat_years$difference <- dat_years$max_min - dat_years$lengthyrs
  # dat_years$difference_70 <- nyrs - dat_years$lengthyrs
  
  comp_subset <- droplevels(subset(dat_years, dat_years$lengthyrs>5))
  completecases <- as.vector(unique(comp_subset$LocationID))
  
  complete_dat <- species_dat[species_dat$LocationID %in% completecases,]
  species_dat <- complete_dat
  
  #---
  
  #filter out small n here
  #then bootstrap
  
  locsw_fw1 <- species_dat %>% group_by(LocationID, sampleYear, Species) %>% 
    summarize(total_n=sum(n, na.rm=T)) 
  locsw_fw2 <- species_dat %>% group_by(LocationID, sampleYear, Species, Salt.Water.Age, Fresh.Water.Age) %>% 
    summarize(ageclass_n=sum(n, na.rm=T), mean_length=mean(mean, na.rm=T))
  
  locsw_fw <- merge(locsw_fw1, locsw_fw2, by=c("LocationID", "sampleYear", "Species"))
  locsw_fw$sw_fw_age_proportion <- locsw_fw$ageclass_n/locsw_fw$total_n
  
  #remove small sample sizes
  # temp_avgs <- droplevels(subset(locsw_fw, locsw_fw$total_n>10))
  temp_avgs <- locsw_fw
  
  # #loop through species
  # for (v in 1:length(sps)){
  #   temp_sps <- sps[v]
  #   print(temp_sps)
  #   
  #   temp_avgs <- droplevels(subset(loc_age_len, loc_age_len$Species==temp_sps))
  
  #create avgs dataframe for that species
  yrly_avgs <- temp_avgs %>% group_by(LocationID, sampleYear) %>% 
    summarize( loc_mean_length=mean(mean_length, na.rm=T) )
  temp_avgs <- temp_avgs %>% unite(fw_sw, Fresh.Water.Age, Salt.Water.Age, sep="_", remove=FALSE)
  
  temp_avgs <- na.omit(temp_avgs)
  
  
  if (exists("age_outdf")) rm(age_outdf)
  locs <- unique(temp_avgs$LocationID)
  locs <- locs[is.na(locs)==FALSE]
  for (i in 1:length(unique(temp_avgs$LocationID))) { #
    loc <- locs[i]
    temp <- subset(temp_avgs, temp_avgs$LocationID==loc) 
    
    
    temp$fw_sw <- as.factor(paste(temp$Fresh.Water.Age,"_",temp$Salt.Water.Age, sep=""))
    
    ages <- unique(temp$fw_sw)
    
    for(g in 1:length(ages)) {
      temp_age <- ages[g]
      
      temp_age_dat <- temp[which(temp$fw_sw==temp_age),]
      
      print(temp_age)
      
      temp.yrs <- unique(temp_age_dat$sampleYear)    #
      print(loc)
      d_saa <- vector(length=length(temp.yrs))
      av_saa <- vector(length=length(temp.yrs))
      d_age_prop <- vector(length=length(temp.yrs))
      av_age_prop <- vector(length=length(temp.yrs))
      yr_vec <- vector(mode="numeric", length=length(temp.yrs))
      age_vec <- vector(mode="numeric", length=length(temp.yrs))
      n_vec <- vector(mode="numeric", length=length(temp.yrs))
      prev_mean <- vector(mode="numeric", length=length(temp.yrs))
      new_mean <- vector(mode="numeric", length=length(temp.yrs))
      
      
      for (h in 1:length(temp.yrs)) {                      
        yr <- temp.yrs[h]
        n <- temp_age_dat$total_n[h]
        #print(yr)
        if ((yr-1) %in% temp.yrs) {     #add here 2 yr avgs
          d_saa[h] <- temp_age_dat$mean_length[h] - temp_age_dat$mean_length[h-1]
          av_saa[h] <- (temp_age_dat$mean_length[h] + temp_age_dat$mean_length[h-1])/2
          d_age_prop[h] <- temp_age_dat$sw_fw_age_proportion[h] - temp_age_dat$sw_fw_age_proportion[h-1]
          av_age_prop[h] <- (temp_age_dat$sw_fw_age_proportion[h] + temp_age_dat$sw_fw_age_proportion[h-1])/2
          yr_vec[h] <- yr
          age_vec[h] <- as.character(temp_age)
          n_vec[h] <- n       #n will repeat b/c its the n that went into age class proportion calc!
          prev_mean[h] <- temp_age_dat$mean_length[h-1]
          new_mean[h] <- temp_age_dat$mean_length[h]
          #  }
        } else {
          d_saa[h] <- NA
          d_age_prop[h] <- NA
          av_saa[h] <- NA
          av_age_prop[h] <- NA
          yr_vec[h] <- yr
          age_vec[h] <- as.character(temp_age)
          n_vec[h] <- n
          prev_mean[h] <- NA
          new_mean[h] <- temp_age_dat$mean_length[h]
        }
      }
      if (exists("age_outdf")) {
        age_outdf <- data.frame(yr_vec, d_saa, av_saa, d_age_prop, av_age_prop, loc, age_vec, n_vec, prev_mean, new_mean) %>% 
          bind_rows(age_outdf)
      } else {
        age_outdf <- data.frame(yr_vec, d_saa, av_saa, d_age_prop, av_age_prop, loc, age_vec, n_vec, prev_mean, new_mean) #%>% cleancols()
      }
    }
  }
  age_outdf <- age_outdf[which(age_outdf$yr_vec!=0),]
  
  changes <- age_outdf
  changes <- na.omit(changes)
  
  changes$avgp_x_dx <- abs(changes$av_age_prop) * abs(changes$d_saa)   
  changes$dp_x_avgx <- abs(changes$d_age_prop) * abs(changes$av_saa)
  changes$avgp_x_avgx <- abs(changes$av_age_prop) * abs(changes$av_saa)
  
  #use abs() for changes in both, following steve's notes 
  
  #create output vectors
  age_change_vec <- vector(mode="numeric", length=length(changes$yr_vec))
  saa_change_vec <- vector(mode="numeric", length=length(changes$yr_vec))
  size_change_calc <- vector(mode="numeric", length=length(changes$yr_vec))
  size_calc <- vector(mode="numeric", length=length(changes$yr_vec))
  yr_change_vec <- vector(mode="numeric", length=length(changes$yr_vec))
  loc_change_vec <- vector(mode="character",length=length(changes$yr_vec))
  
  #loop through calculations to get change due to age vs saa
  
  #if (exists("locchange_outdf")) rm(locchange_outdf)
  r<-1
  locs2 <- unique(changes$loc)
  for (r in 1:length(locs2)) {
    loc <- locs2[r]
    temp <- changes[which(changes$loc==loc),]
    print(loc)
    
    yrs <- unique(temp$yr_vec)
    yrs <- sort(yrs, decreasing=FALSE)
    
    age_change_vec <- vector(mode="numeric", length=length(yrs))
    saa_change_vec <- vector(mode="numeric", length=length(yrs))
    size_change_calc <- vector(mode="numeric", length=length(yrs))
    size_calc <- vector(mode="numeric", length=length(yrs))
    yr_change_vec <- vector(mode="numeric", length=length(yrs))
    loc_change_vec <- vector(mode="character",length=length(yrs))
    mean_dsize <- vector(mode="numeric",length=length(yrs))
    species <- vector(length=length(yrs))
    
    g<-1
    for(g in 1:length(yrs)) {
      yr <- yrs[g]
      # print(yr)
      yr_dat <- subset(temp, temp$yr_vec==yr)
      
      change_by_saa <- (sum(yr_dat$avgp_x_dx))/(sum(yr_dat$avgp_x_dx + yr_dat$dp_x_avgx))   
      change_by_age <- (sum(yr_dat$dp_x_avgx))/(sum(yr_dat$dp_x_avgx + yr_dat$avgp_x_dx))
      change_in_size_computed <- sum(yr_dat$avgp_x_dx + yr_dat$dp_x_avgx)
      change_in_size_computed2 <- sum(yr_dat$av_age_prop*yr_dat$av_saa)
      mean_dsize[g] <- mean(yr_dat$d_saa)
      
      # print(change_by_age)
      
      age_change_vec[g] <- change_by_age
      saa_change_vec[g] <- change_by_saa
      yr_change_vec[g] <- yr
      # loc_change_vec[g] <- as.character(loc)
      size_change_calc[g] <- change_in_size_computed2
      size_calc[g] <- sum(yr_dat$avgp_x_avgx)
      species[g] <- temp_sps
    } 
    if (exists("locchange_outdf")) {
      locchange_outdf <- data.frame(loc, yr_change_vec, mean_dsize, age_change_vec, saa_change_vec, size_change_calc, size_calc, species) %>% 
        bind_rows(locchange_outdf)
    } else {
      locchange_outdf <- data.frame(loc, mean_dsize, yr_change_vec, age_change_vec, saa_change_vec, size_change_calc, size_calc, species) #%>% cleancols()
    }
  }
  
}



loc_reg_l<- input_dat[,c(2,5)]
loc_reg <- distinct(loc_reg_l, SASAP.Region, LocationID)

locdat <- merge(locchange_outdf, loc_reg, by.x="loc", by.y="LocationID")

#write.csv(locdat, file=paste0(wd,"/Manuscript_Code/Output/chainrule_output_location.csv")) #
