#==================================================================================================
#Project Name: SUBMISSION: Recent declines in salmon body size impact ecosystems and people
#
#Creator: Curry Cunningham
#
#Purpose: Construction Bayesian hierarchical models quantifying relationship between change in
#           salmon size and covariates.
#==================================================================================================
require(tidyverse)
require(dplyr)
require(brms)
require(bayesplot)
require(brmstools)
require(jtools)

# CONTROL SECTION ===========================================================
min.years <- 20  #Select minimum time series length threshold. Minimum number of year for observation time series to be included

fit <- TRUE  # Whether to fit model, or read model output file.

#MCMC Stuff
n.iter <- 2e4  # Number of MCMC iterations
n.thins <- 10  # Thinning rate 1/10
n.chains <- 3  # Number of MCMC chains to run in parallel

# Define workflow paths =====================================================
# *Assumes you have set your working directory to: /Manuscript_Code
wd <- getwd()
output.dir <- file.path(wd,"Output")
figs.dir <- file.path(wd,"Figs")
data.dir <- file.path(wd,"Data")

# Create subdirectory for Drivers Analysis figures
figs.dir <- file.path(figs.dir, "Drivers_Analysis")
dir.create(figs.dir)

start.time <- date()

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

# Create Output Object ==================================================
R2_length <- array(dim=c(n.species, ncol=5), dimnames=list(species,c('MinYrs','Estimate','Est.Error','Q2.5','Q97.5')))
R2_age <- array(dim=c(n.species, ncol=5), dimnames=list(species,c('MinYrs','Estimate','Est.Error','Q2.5','Q97.5')))

# Standard Length Analysis ==============================================

s <- 1
for(s in 1:n.species) {
  
  temp.dat <- new.dat %>% subset(Species==species[s] & sampleYear>=1975 & nn>=min.years)
  
  if(fit==TRUE) {
    brm <- brm(standard_length ~  -1 + PDO_Jeff + (0 + PDO_Jeff|LocationID) +
                 NPGO + (0 + NPGO|LocationID) +
                 MEI + (0 + MEI|LocationID) +
                 MEIw + (0 + MEIw|LocationID) +
                 sockTotal + (0 + sockTotal|LocationID) +
                 pinkTotal + (0 + pinkTotal|LocationID) +
                 pinkTotalAK + (0 + pinkTotalAK|LocationID) +
                 chumTotal + (0 + chumTotal|LocationID) +
                 chumTotalAK + (0 + chumTotalAK|LocationID) +
                 airTempSummer + (0 + airTempSummer|LocationID) +
                 SSTsummer + (0 + SSTsummer|LocationID) +
                 icecov + (0 + icecov|LocationID) +
                 s(sampleYear),
               data=temp.dat,
               iter=n.iter, thin=n.thins,
               chains=n.chains,
               family=gaussian(),
               prior=c(prior(normal(0,1), class=b),
                       prior(normal(0,10), class=sd),
                       prior(normal(0,10), class=sds),
                       prior(normal(0,10), class=sigma)),
               sample_prior=TRUE,
               control = list(adapt_delta = 0.99))
    
    #Save output
    saveRDS(brm, file=file.path(figs.dir,paste0(species[s],"_std_length.rds")))
  }else {
    brm <- readRDS(file=file.path(figs.dir,paste0(species[s],"_std_length.rds")))
  }
  # names(brm)
  
  #Record R2
  R2 <- bayes_R2(brm)
  R2_length[s,1] <- min.years
  R2_length[s,2:5] <- R2
  
  #Extract Coefficients
  coefs <- coef(brm)[[1]]
  locs <- dimnames(coefs)[[1]]
  pars <- dimnames(coefs)[[3]]
  
  # Convert to array of mcmc draws
  mcmc <- as.array(brm)
  
  pdf(file=file.path(figs.dir,paste0(species[s],"_std_length.pdf")), height=6, width=6)
  #Plot Group Means and Uncertainty, plus population-specific means
  p <- brmstools::coefplot(brm, level=0.95, interval_col='black',
                      point_col='black',
                      r_col='blue', r_intervals=FALSE, r_alpha=0.75) +
    geom_vline(aes(xintercept=0), alpha=0.5, color='red') +
    theme_gray() +
    ggtitle(paste('Bayes R2:', round(bayes_R2(brm)[1],3)))
  plot(p)
  #Plot again without smooth
  p <- brmstools::coefplot(brm, level=0.95, interval_col='black',
                      point_col='black',
                      r_col='blue', r_intervals=FALSE, r_alpha=0.75,
                      pars=pars[-which(pars=='ssampleYear_1')]) +
    geom_vline(aes(xintercept=0), alpha=0.5, color='red') +
    theme_gray() +
    ggtitle(paste('Bayes R2:', round(bayes_R2(brm)[1],3)))
  plot(p)
  #Plot Year Smooth
  g <- marginal_smooths(brm)
  plot(g)
  g <- marginal_smooths(brm, spaghetti=TRUE)
  plot(g)[[1]] + theme_gray()
  g <- marginal_smooths(brm, spaghetti=TRUE, nsamples=100)# + theme_black()
  plot(g)[[1]] + theme_gray()
  
  # extract posterior samples of population-level effects 
  samples1 <- posterior_samples(brm, "^b")
  head(samples1)
  g <- mcmc_areas(samples1, prob = 0.5, prob_outer = 0.99, point_est = "mean")
  plot(g)
  g.b <- mcmc_areas_ridges(samples1)
  plot(g.b)
  #Plot without year attribute
  g <- mcmc_areas(samples1[-which(names(samples1)=='bs_ssampleYear_1')], prob = 0.5, prob_outer = 0.99, point_est = "mean")
  plot(g)
  g.b <- mcmc_areas_ridges(samples1[-which(names(samples1)=='bs_ssampleYear_1')])
  plot(g.b)
  
  # extract posterior samples of group-level standard deviations
  samples2 <- posterior_samples(brm, "^sd_")
  head(samples2)
  g2 <- mcmc_areas(samples2, prob = 0.5, prob_outer = 0.99, point_est = "mean")
  plot(g2)
  g2 <- mcmc_areas_ridges(samples2)
  plot(g2)
  # extract group level random effects
  
  #Plot separate forest plots ====================
  
  for(p in 1:(length(pars)-1)) {
    p <- brmstools::forest(brm, pars=pars[p], text=FALSE, 
                           av_name='Group Mean Effect', sort=FALSE) +
      geom_vline(aes(xintercept=0), alpha=0.5, color='red')
    plot(p)
  }#next p
  
  dev.off()
}#next s


end.time <- date()

print(paste("START:",start.time))
print(paste("END:",end.time))