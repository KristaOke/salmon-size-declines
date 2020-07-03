#==================================================================================================
#Project Name: SUBMISSION: Recent declines in salmon body size impact ecosystems and people
#
#Creator: Curry Cunningham
#
#Purpose: Plot and summarize results from Bayesian hierarchical models quantifying relationship between change in
#           salmon size and covariates.
#==================================================================================================
require(tidyverse)
require(visreg)
require(sjPlot)
require(gridExtra)
require(dplyr)
require(brms)
require(bayesplot)
require(brmstools)
require(jtools)
require(reshape2)
require(ggthemes)
require(viridis)
require(ggridges)
require(tidybayes)
require(RColorBrewer)

# CONTROL SECTION ===========================================================
#Name of model output
#Plot Length or Age
name.model.out <- "std_length"

# Define workflow paths =====================================================
# *Assumes you are working from the sasap-size-declines R project
wd <- getwd()
output.dir <- file.path(wd,"Output")
figs.dir <- file.path(wd,"Figs")
data.dir <- file.path(wd,"Data")

# Subdirectory for figures
figs.dir <- file.path(figs.dir, "Drivers_Analysis")

# Load Data ==================================================================
new.dat <- readRDS(file.path(data.dir,"new.dat.4.rds")) 
new.dat$SASAP.Region <- factor(new.dat$SASAP.Region)
new.dat$LocationID <- factor(new.dat$LocationID)

#Standardize candidate response variables:
#   Length: Z-score
#   Ocean age: Deviations from time series mean
new.dat <- new.dat %>% group_by(SASAP.Region, LocationID, Species) %>% 
  mutate(standard_length=(mean_length-mean(mean_length,na.rm=TRUE))/sd(mean_length,na.rm=TRUE),
         standard_oAge=(mean_oAge-mean(mean_oAge,na.rm=TRUE)))#/sd(mean_oAge,na.rm=TRUE))

# Calculate number of year observations for each time-series
new.dat <- new.dat %>% group_by(SASAP.Region, LocationID, Species) %>% mutate(nn=n())

#Add year as factor
new.dat$sampleYear.factor <- factor(new.dat$sampleYear)

#Remove NA Values- ADD YEAR HERE
new.dat <- new.dat %>% dplyr::select(c(mean_length, standard_length, standard_oAge,
                                       Species, SASAP.Region, LocationID,
                                       sampleYear, sampleYear.factor, n, nn,
                                       PDO_Jeff, NPGO,  MEI,  MEIw,
                                       orcaCombined, sealionWAK,
                                       sockTotal,
                                       pinkTotal, pinkTotalAK,
                                       chumTotal, chumTotalAK,
                                       airTempSummer, SSTsummer, regime, icecov)) %>% na.omit()

#Ensure Regime is a factor
new.dat$regime <- as.factor(new.dat$regime)

#Define Species
species <- unique(new.dat$Species)
species <- species[-which(species=='pink')]
n.species <- length(species)

# Load lookup table with covariate identifiers, full names, and categories.
covar.lookup <- read.csv(file.path(data.dir, "Covar-Lookup.csv"), header=TRUE, stringsAsFactors=FALSE)

# Load Model Estimates =======================================================
hbm.out <- vector("list", length=n.species)
s <- 1
for(s in 1:n.species) {
  print(paste("Loaded:", species[s]))
  temp.name <- paste0(species[s], "_", name.model.out, ".rds")
  hbm.out[[s]] <- readRDS(file.path(figs.dir, temp.name))
}

# Extract Posterior Samples for Stock-specific Effects =======================
s <- 1
for(s in 1:n.species) {
  print(paste("Compiled Effects List:", species[s]))
  
  #Load Temporary data for reference
  temp.dat <- new.dat %>% subset(Species==species[s] & sampleYear>=1975 & nn>=min.years)
  
  #Extract Bayesian Model Random Effects (project-level) estimates
  ran <- brmstools::tidycoef(hbm.out[[s]], grouping="LocationID", summary=FALSE) 
  
  list.ran <- melt(ran)
  names(list.ran) <- c('iter','LocationID','Type','Covar','var','value')
  #Add Species
  list.ran$Species <- species[s]

  #Setup list of areas
  coefs <- coef(hbm.out[[s]])[[1]]
  locs <- dimnames(coefs)[[1]]
  regions <- vector(length=length(locs))
  for(l in 1:length(locs)) {
    
    regions[l] <- unique(as.character(temp.dat$SASAP.Region[temp.dat$LocationID==locs[l]]))
    
    if(length(unique(temp.dat$SASAP.Region[temp.dat$LocationID==locs[l]]))!=1) {stop()}
    if(is.na(unique(temp.dat$SASAP.Region[temp.dat$LocationID==locs[l]]))) {stop()}
  }
  #Combine into temporary list
  lookup <- data.frame(locs, regions, stringsAsFactors=FALSE)
  names(lookup) <- c("LocationID", "SASAP.Region")
  print(lookup)
  #Join
  list.ran.2 <- list.ran %>% full_join(lookup, by=c("LocationID"="LocationID"))
  

  if(s==1) {
    list.cov.eff <- list.ran.2
  }else {
    list.cov.eff <- rbind(list.cov.eff, list.ran.2)
  }
  
}
#Returns: list.cov.eff

# Add Updated Covariate Names =================================================
list.cov.eff <- list.cov.eff %>% left_join(covar.lookup)


# RUN TO HERE PRIOR TO PLOTTING ===============================================

# PLOT: Posteriors for Stock-specific Effects =================================

temp.species <- 'chinook'

g <- ggplot(filter(list.cov.eff, Species==temp.species, Covar=='PDO_Jeff', !is.na(LocationID)), 
            aes(value, color=SASAP.Region)) +
       theme_linedraw() +
       scale_color_stata() +
       geom_line(stat="density", alpha=0.5) +
       ggtitle(paste(species[s],"salmon"),
               subtitle="Location-specific Covariate Effects") +
       geom_vline(xintercept = 0, lty=1)
g


#PLOT: Single Species, Multiple Parameters ===========================

temp.species <- 'sockeye'

pdf(file.path(figs.dir,"1-Single Species.pdf"), height=10, width=15)
for(s in 1:n.species) {
g <- ggplot(filter(list.cov.eff, Species==species[s], !is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, color=SASAP.Region, group=LocationID)) +
  theme_gray() +
  # scale_color_stata() +
  geom_line(stat="density", alpha=0.5) +
  theme(legend.position='bottom') +
  ggtitle(paste(species[s],"salmon"),
          subtitle="Location-specific Covariate Effects") +
  geom_vline(xintercept = 0, lty=1) +
  facet_wrap(~Covar_title, scales='free_y')
plot(g)
}#next s
dev.off()

#Scaled Axis
pdf(file.path(figs.dir,"2-Single Species-scaledAxis.pdf"), height=10, width=15)
for(s in 1:n.species) {
  g <- ggplot(filter(list.cov.eff, Species==species[s], !is.na(LocationID),
                     Covar!='ssampleYear_1', Covar!='Intercept'), 
              aes(value, color=SASAP.Region, group=LocationID)) +
    theme_gray() +
    geom_line(stat="density", alpha=0.5) +
    theme(legend.position='bottom') +
    ggtitle(paste(species[s],"salmon"),
            subtitle="Location-specific Covariate Effects") +
    geom_vline(xintercept = 0, lty=1) +
    facet_wrap(~Covar_title, scales='free_y') +
    coord_cartesian(xlim=c(-0.75,0.75))
    
  plot(g)
}#next s
dev.off()


#Compare across regions
pdf(file.path(figs.dir,"3-Species-Region.pdf"), height=15, width=15)
for(s in 1:n.species) {
g <- ggplot(filter(list.cov.eff, Species==species[s], !is.na(LocationID), 
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, color=LocationID, group=LocationID)) +
  theme_gray() +
  geom_line(stat="density", alpha=0.5) +
  theme(legend.position='none') +
  ggtitle(paste(species[s],"salmon"),
            subtitle="Location-specific Covariate Effects") +
  geom_vline(xintercept = 0, lty=1) +
  facet_grid(SASAP.Region~Covar_title, scales='free') +
  coord_cartesian(xlim=c(-0.75,0.75))
plot(g)
}#next s
dev.off()


#PLOT: Multiple Species, Multiple Parameters ===========================

g <- ggplot(filter(list.cov.eff, !is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, color=SASAP.Region, group=LocationID)) +
  theme_linedraw() +
  geom_line(stat="density", alpha=0.25) +
  theme(legend.position='none') +
  ggtitle('Location-specific Covariate Effects',
          subtitle='Posterior Distributions') +
  geom_vline(xintercept = 0, lty=1) +
  facet_grid(Covar_title~Species, scales='free') +
  coord_cartesian(xlim=c(-0.75,0.75))
g
ggsave(file.path(figs.dir,"4-All Species.png"), plot=g, height=15, width=12, units='in', dpi=500)

g <- ggplot(filter(list.cov.eff, !is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, color=SASAP.Region, group=LocationID)) +
  theme_linedraw() +
  geom_line(stat="density", alpha=0.25) +
  theme(legend.position='none') +
  ggtitle('Location-specific Covariate Effects',
          subtitle='Posterior Distributions') +
  geom_vline(xintercept = 0, lty=1) +
  facet_grid(Species~Covar_title, scales='free') +
  coord_cartesian(xlim=c(-0.75,0.75))
g
ggsave(file.path(figs.dir,"4b-All Species-Reversed Pannels.png"), plot=g, height=10, width=15, units='in', dpi=500)


# PLOT: Group-level mean effects across species ============================================

g <- ggplot(filter(list.cov.eff, is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, fill=Species, group=Species)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_density(alpha=0.5, lwd=FALSE) +
  theme(legend.position='top', 
        axis.text.y = element_blank()) +
  geom_vline(xintercept = 0, lty=1, color='red') +
  facet_wrap(~Covar_title, scales='free') +
  xlab("Coefficient Value") +
  ylab("Probability Density")
g

ggsave(file.path(figs.dir,"5-Group Mean Effects.png"), plot=g, height=7, width=8, units='in', dpi=500)
ggsave(file.path(figs.dir,"5-Group Mean Effects.pdf"), plot=g, height=7, width=8, units='in', dpi=500)

# PLOT: All Locations Colored By Species ============================================
list.cov.eff$spec.loc <- paste0(list.cov.eff$Species,"-",list.cov.eff$LocationID)

g <- ggplot(filter(list.cov.eff, !is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(value, color=Species, group=spec.loc)) +
  theme_linedraw() +
  scale_color_colorblind() +
  geom_line(stat="density", alpha=0.2) +
  theme(legend.position='bottom',
        axis.text.y=element_blank()) +
  geom_vline(xintercept = 0, lty=1) +
  facet_wrap(~Covar_title, scales='free') +
  coord_cartesian(xlim=c(-1,1)) +
  xlab("Coefficient Value") +
  ylab("Probability Density")
g
ggsave(file.path(figs.dir,"6-All Species Together.png"), plot=g, height=7, width=8, units='in', dpi=500)
ggsave(file.path(figs.dir,"6-All Species Together.pdf"), plot=g, height=7, width=8, units='in', dpi=500)


# PLOT: Smoothed Residual Trend =================================

# Extract Residual Trend and 50% and 95% CI
s <- 1
for(s in 1:n.species) {
  temp.smooths <- brms::marginal_smooths(hbm.out[[s]], probs=c(0.025, 0.975))[[1]]
  temp.smooths$Species <- species[s]
  if(s==1) {
    list.smooths <- temp.smooths
  }else {
    list.smooths <- rbind(list.smooths, temp.smooths)
  }
}

g.smth <- list.smooths %>% ggplot(aes(x=sampleYear, y=estimate__, 
                                      color=Species, fill=Species)) +
            theme_linedraw() +
            scale_color_colorblind() +
            scale_fill_colorblind() +
            geom_hline(yintercept=0, col='black', alpha=0.5) +
            geom_ribbon(aes(ymin=lower__, ymax=upper__), alpha=0.25, lwd=1e-6) +
            geom_line() +
            xlab("Sample Year") +
            ylab("Estimate") 


g.smth
ggsave(file.path(figs.dir,"7-Smooth Terms.png"), plot=g.smth, height=5, width=5, units='in', dpi=500)

g.smth.2 <- list.smooths %>% ggplot(aes(x=sampleYear, y=estimate__, 
                                        color=Species, fill=Species)) +
  theme_linedraw() +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  geom_hline(yintercept=0, col='black', alpha=0.5) +
  geom_ribbon(aes(ymin=lower__, ymax=upper__), alpha=0.25, lwd=1e-6) +
  geom_line() +
  xlab("Sample Year") +
  ylab("Estimate") +
  facet_wrap(~Species) +
  theme(legend.position = 'none')
g.smth.2
ggsave(file.path(figs.dir,"7-Smooth Terms_together.png"), plot=g.smth.2, height=5, width=5, units='in', dpi=500)
ggsave(file.path(figs.dir,"7-Smooth Terms_together.pdf"), plot=g.smth.2, height=5, width=5, units='in', dpi=500)

# PLOT: Caterplot of group-level mean effects across species ============================================

g <- ggplot(filter(list.cov.eff, is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(x=Covar_title, y=value, fill=Species)) +
  theme_bw() +
  geom_hline(yintercept = 0, lty=1, color='red') +
  geom_eye(scale='width', .width = c(0.5, 0.95)) +
  theme(legend.position='top') +
  facet_wrap(~Species, scales='free_x') +
  ylab("Mean Covariate Effect Across Stocks") +
  xlab("") +
  coord_flip()
g

ggsave(file.path(figs.dir,"8-Group Mean Eye.png"), plot=g, height=7, width=8, units='in', dpi=500)
ggsave(file.path(figs.dir,"8-Group Mean Eye.pdf"), plot=g, height=7, width=8, units='in', dpi=500)

# PLOT: GGridges of group-level mean effects across species ============================================

g <- ggplot(filter(list.cov.eff, is.na(LocationID),
                   Covar!='ssampleYear_1', Covar!='Intercept'), 
            aes(y=Covar_title, x=value, fill=Species)) +
  theme_bw() +
  scale_fill_colorblind() +
  geom_vline(xintercept = 0, lty=1, color='red') +
  geom_density_ridges(alpha=0.75) +
  facet_wrap(~Species, scales='free_x') +
  xlab("Mean Covariate Effect Across Stocks") +
  ylab("")
  # coord_flip()
g

ggsave(file.path(figs.dir,"9-Group Mean Ridges.png"), plot=g, height=7, width=8, units='in', dpi=500)
ggsave(file.path(figs.dir,"9-Group Mean Ridges.pdf"), plot=g, height=7, width=8, units='in', dpi=500)


# PLOT: Colored caterplot of group-level mean effects across species ============================================

# Add Grouping Factors =======================
head(list.cov.eff)

list.cov.eff.2 <- list.cov.eff
list.cov.eff.2$Covar_title <- factor(list.cov.eff.2$Covar_title, levels=rev(covar.lookup$Covar_title), ordered=TRUE)
list.cov.eff.2$category_fine <- factor(list.cov.eff.2$category_fine, levels=unique(covar.lookup$category_fine), ordered=TRUE)



g.eye <- list.cov.eff.2 %>% filter(is.na(LocationID), Covar!='ssampleYear_1', Covar!='Intercept') %>%  
         ggplot(aes(x=Covar_title, y=value, fill=category_fine)) +

  
  theme_linedraw() +
  scale_fill_manual(values=c("#edf8fb", "#b3cde3", "#8c96c6", "#88419d"), name=element_blank()) +
  geom_hline(yintercept = 0, lty=1, color='red') +
  geom_eye(scale='width', color='black',
            .width = c(0.50, 0.95)) +
  stat_summary(fun.y="median", colour="gray", geom="point", show.legend=FALSE) +
  theme(legend.position='top') +
  facet_wrap(~Species, scales='free_x') +
  ylab("Mean Covariate Effect Across Stocks") +
  xlab("") +
  coord_flip()
g.eye

ggsave(file.path(figs.dir,"10-Group Mean Eye_v1.pdf"), plot=g.eye, 
         height=7, width=8, units='in', dpi=500)

# New Color
g.eye.2 <- g.eye + scale_fill_brewer(palette="Set1", name=element_blank())

ggsave(file.path(figs.dir,"10-Group Mean Eye_v2.pdf"), 
       plot=g.eye.2, 
       height=7, width=8, units='in', dpi=500)

# New Color  - Colorblind friendly http://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=4
g.eye.3 <- g.eye + scale_fill_manual(values=c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6"), name=element_blank())

ggsave(file.path(figs.dir,"10-Group Mean Eye_v3.pdf"), 
       plot=g.eye.3, 
       height=7, width=8, units='in', dpi=500)


# With Boxes ===========

temp.cols <- c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")

g.eye.4 <- g.eye.3 + 
             annotate("rect", xmin=7.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[1]) +
             annotate("rect", xmin=5.5, xmax=7.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[2]) +
             annotate("rect", xmin=2.5, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[3]) +
             annotate("rect", xmin=-Inf, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.2, fill=temp.cols[4])
  

# g.eye.4
ggsave(file.path(figs.dir,"10-Group Mean Eye_v4.pdf"), 
       plot=g.eye.4, 
       height=7, width=8, units='in', dpi=500)

# Gray Violins ==========
g.eye.5 <- g.eye.4 + scale_fill_manual(values=rep("dark gray",4), name=element_blank())
  
ggsave(file.path(figs.dir,"10-Group Mean Eye_v5.pdf"), 
       plot=g.eye.5, 
       height=7, width=8, units='in', dpi=500)
