#==================================================================================================
#Project Name: Ecological Consequences
#created by Krista Oke and Katie Kobayashi
#
#Purpose: Ecological impacts
#           We will explore consequences of size and age declines for:
#             decreased nutrient transport
#             and population productivity.
#            
#==================================================================================================

require(tidyverse)

wd <- paste0(getwd(),"/Manuscript_Code")

output.dir <- paste0(wd,"/Output")
figs.dir <- paste0(wd,"/Figs")
data.dir <- paste0(wd,"/Data")

#==========================================================================================

# Read in metadata
linear_model_list <- read.csv(paste0(data.dir,"/consequences Linear Model List.csv"),
                              header = TRUE
) %>%  
  select(-X)


# subset for projects/regions/spp. of interest
#we are interested in all escapement projects that produced a non-NA linear model output
ecology_linear_models <- subset(linear_model_list, ASLProjectType == "escapement") %>% 
  subset(!is.na(X..change.90minus.2010plus)) %>% 
  subset(Species != "pink")

#===========================================================================================

# Use species, locations, and "change.90minus.2010plus" (-> Dbodysize)
# from linear models list as base for ecology consequences table
ecology <- select(ecology_linear_models,
                  SASAP.Region, 
                  LocationID, 
                  Species, 
                  X90minus.mean,
                  X2010plus.mean,
                  D.90.2010,
                  X..change.90minus.2010plus)

colnames(ecology) <- c("SASAP.Region", 
                       "LocationID", 
                       "Species", 
                       "X90minus.mean",
                       "X2010plus.mean",
                       "D.90.2010",
                       "Percent.90.2010")

#============================================================================================

#Model estimated weight (pounds) from mean fish sizes using standard length-weight regression
##  W = a × L^b

## a and b parameters based on values fitted in 'len-weight_relationship.R'
chinook.a <- 0.000000001773218
chinook.b <- 3.295027
coho.a <- 0.00000000645953
coho.b <- 3.141252
# pink.a <- 0.0155
# pink.b <- 3.06
sockeye.a <- 0.00000001007088
sockeye.b <- 3.079836
chum.a <- 0.000000006302654
chum.b <- 3.137442

#estimate mean weight (kg) for fish 1990 and earlier, based on mean length (mm/10)
ecology <- mutate(ecology, '90.meanweight.g' =
                    as.numeric(ifelse(Species == 'chinook',
                                      chinook.a*((X90minus.mean)^chinook.b)*1000, 
                                      ifelse(Species == 'coho', 
                                             coho.a*((X90minus.mean)^coho.b)*1000, 
                                             ifelse(Species == 'pink',
                                                    pink.a*((X90minus.mean)^pink.b)*1000, 
                                                    ifelse(Species == 'sockeye',
                                                           sockeye.a*((X90minus.mean)^sockeye.b)*1000, 
                                                           ifelse(Species == 'chum',
                                                                  chum.a*((X90minus.mean)^chum.b)*1000,
                                                                  "NA"
                                                           )))))))

#estimate mean weight (kg) for fish 2010 and later, based on mean length (mm/10)
ecology <- mutate(ecology, '2010.meanweight.g' =
                    as.numeric(ifelse(Species == 'chinook',
                                      chinook.a*((X2010plus.mean)^chinook.b)*1000, 
                                      ifelse(Species == 'coho', 
                                             coho.a*((X2010plus.mean)^coho.b)*1000, 
                                             ifelse(Species == 'pink',
                                                    pink.a*((X2010plus.mean)^pink.b)*1000, 
                                                    ifelse(Species == 'sockeye',
                                                           sockeye.a*((X2010plus.mean)^sockeye.b)*1000, 
                                                           ifelse(Species == 'chum',
                                                                  chum.a*((X2010plus.mean)^chum.b)*1000,
                                                                  "NA"
                                                           )))))))

#calculate raw (delta, D) and percent (%) change in estimated mean weight pre-1990 vs. post-2010
ecology <- mutate(ecology, 'D.weight.g' =
                    ecology$`2010.meanweight` - ecology$`90.meanweight`
)

#convert weight from g to lbs
#ecology <- ecology %>%  
#  mutate('90.meanweight.lb' = `90.meanweight.g`*0.00220462) %>% 
#  mutate('2010.meanweight.lb' = `2010.meanweight.g`*0.00220462) %>% 
#  mutate('D.weight.lb' = D.weight.g*0.00220462)

#===========================================================================================

#parameters taken from Table 1 in Moore et al 2011 and Twining 2017
parameters <- tibble(
  species = c("steelhead", "sockeye", "pink", "coho", "chum", "chinook"),
  post_spawn_mort = c(0.38, 1, 1, 1, 1, 1),
  gP_gadult = c(0.0038, 0.0038, 0.0038, 0.0038, 0.0038, 0.0038),
  gP_gamete = c(2.07, NA, NA, NA, NA, NA),
  gP_excretion = c(0.0316, NA, NA, NA, NA, NA),
  weeks_adult_res = c(2, NA, NA, NA, NA, NA)
)
View(parameters)


ecology <- mutate(ecology, 'gphosperfish_pre1990'=
                    (ifelse(Species == 'pink',
                            (`90.meanweight.g`*parameters[[3,"gP_gadult"]]), 
                            ifelse(Species == 'sockeye',
                                   (`90.meanweight.g`*parameters[[2,"gP_gadult"]]), 
                                   ifelse(Species == 'chum',
                                          (`90.meanweight.g`*parameters[[5,"gP_gadult"]]),
                                          ifelse(Species == 'coho',
                                                 (`90.meanweight.g`*parameters[[4,"gP_gadult"]]),
                                                 ifelse(Species == 'chinook',
                                                        (`90.meanweight.g`*parameters[[6,"gP_gadult"]]),
                                                        "NA")))))))

ecology <- mutate(ecology, 'gphosperfish_post2010'=
                    (ifelse(Species == 'pink',
                            (`2010.meanweight.g`*parameters[[3,"gP_gadult"]]), 
                            ifelse(Species == 'sockeye',
                                   (`2010.meanweight.g`*parameters[[2,"gP_gadult"]]), 
                                   ifelse(Species == 'chum',
                                          (`2010.meanweight.g`*parameters[[5,"gP_gadult"]]),
                                          ifelse(Species == 'coho',
                                                 (`2010.meanweight.g`*parameters[[4,"gP_gadult"]]),
                                                 ifelse(Species == 'chinook',
                                                        (`2010.meanweight.g`*parameters[[6,"gP_gadult"]]),
                                                        "NA")))))))
#grams phosphorus gained/lost per fish
ecology <- mutate(ecology, 'D.PhosphorusPerFish'=
                    as.numeric(`gphosperfish_post2010`) - as.numeric(`gphosperfish_pre1990`)
)

#============================================================================================

#ESTABLISH LINEAR RELATIONSHIPS FOR FECUNDITY


sockeye_ecology <- ecology %>% subset(Species=="sockeye")


#using published relationships 
sockeye_ecology$eggs_90 <- (11.744*sockeye_ecology$X90minus.mean)-2535.863 #using chignik relationship
sockeye_ecology$eggs_10 <- (11.744*sockeye_ecology$X2010plus.mean)-2535.863 #using chignik lake relationship

sockeye_ecology$d_eggs <- sockeye_ecology$eggs_10 - sockeye_ecology$eggs_90

#chinook

chinook_ecology <- ecology %>% subset(Species=="chinook")

#using published relationships 
chinook_ecology$eggs_90 <- (17.839*chinook_ecology$X90minus.mean)-5594.2 #using unalakleet relationship
chinook_ecology$eggs_10 <- (17.839*chinook_ecology$X2010plus.mean)-5594.2 #using unalakleet relationship

chinook_ecology$d_eggs <- chinook_ecology$eggs_10 - chinook_ecology$eggs_90


#chum

chum_ecology <- ecology %>% subset(Species=="chum")

#using published relationships 
chum_ecology$eggs_90 <- (29.208*(chum_ecology$X90minus.mean/10))+766.53 #using  kuskokwim relationship
chum_ecology$eggs_10 <- (29.208*chum_ecology$X2010plus.mean/10)+766.53 #using kuskokwim relationship

chum_ecology$d_eggs <- chum_ecology$eggs_10 - chum_ecology$eggs_90


#coho

coho_ecology <- ecology %>% subset(Species=="coho")

#using published relationships
coho_ecology$eggs_90 <- (22.45*(coho_ecology$X90minus.mean))-7619 #using Beacham relationship
coho_ecology$eggs_10 <- (22.45*coho_ecology$X2010plus.mean)-7619 #using Beacham relationship

coho_ecology$d_eggs <- coho_ecology$eggs_10 - coho_ecology$eggs_90



#============================================================================================

ecology_join <- bind_rows(chinook_ecology, sockeye_ecology, chum_ecology, coho_ecology)

ecology_join <- mutate(ecology_join, 'D.egg.proportion'= d_eggs/as.numeric(`eggs_90`))
ecology_join <- mutate(ecology_join, 'D.phos.proportion'= D.PhosphorusPerFish/as.numeric(`gphosperfish_pre1990`))


#=============================================================================================

#SAVE OUTPUTS

write.csv(ecology_join, file=paste0(output.dir,"/eco_conseq_expanded.csv"))

colnames(ecology_join)
Ecological_Consequences <- select(ecology_join, 
                                  "SASAP.Region",
                                  "LocationID",
                                  "Species",
                                  "Percent.90.2010", 
                                  "D.90.2010",
                                  "D.weight.g",
                                  "D.PhosphorusPerFish",
                                  "d_eggs",
                                  "D.egg.proportion",
                                  "D.phos.proportion")

#rename columns to presentable names
names(Ecological_Consequences) <- c('Region',
                                    'Location',
                                    'Species',
                                    '% Change Mean Length',
                                    'D Mean Length (mm)',
                                    'D Mass (g)',
                                    'D MDN delivered per fish (g_Phosphorus)',
                                    'D Fecundity (N_eggs)',
                                    "D.egg.proportion",
                                    "D.phos.proportion")

View(Ecological_Consequences)
write.csv(Ecological_Consequences, file=paste0(output.dir,"/Ecological Consequences.csv"))



table_S6 <- select(ecology_join, 
                   "SASAP.Region",
                   "LocationID",
                   "Species",
                   "D.90.2010",
                   "Percent.90.2010", 
                   "D.weight.g",
                   "D.PhosphorusPerFish",
                   "d_eggs")
write.csv(table_S6, file=paste0(output.dir,"/table_s6_ecol.csv"))

