#=========================================================================================
#
#Project Name: Economic Consequences
#Created by Krista Oke and Katie Kobayashi
#
#Purpose: Economic impacts 
#           We will explore impacts of size and age declines for the economic value commercial fisheries 
#           (e.g. reduced biomass harvested, lower prices).
#          
#           Based on commercial catch projects
#
#"Based on the most recent reported price per pound, a fish that is XXX cm smaller, is worth XXX dollars less."
#=========================================================================================


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

exvessel_prices <- read.csv(paste0(data.dir,"/ExVessel_Prices.csv"),
                            header = TRUE
)

#===========================================================================================

#we are interested in every commercial project that produced a non-NA linear model output


# subset for projects/regions/spp. of interest
econ_linear_models <- 
  subset(linear_model_list, ASLProjectType == "commercial catch")

# Use species, locations, and "change.90minus.2010plus" (-> Dbodysize)
# from linear models list as base for economic consequences table
econ_conseq <- select(econ_linear_models,
                      SASAP.Region, 
                      LocationID, 
                      Species, 
                      X90minus.mean,
                      X2010plus.mean,
                      D.90.2010,
                      X..change.90minus.2010plus,
                      SD_90_10_change,
                      SD_90minus,
                      SD_2010plus 
) %>% 
  subset(!is.na(X..change.90minus.2010plus)) 


colnames(econ_conseq) <- c("SASAP.Region", 
                           "LocationID", 
                           "Species", 
                           "X90minus.mean",
                           "X2010plus.mean",
                           "D.90.2010",
                           "Percent.90.2010",
                           "SD_90_10_change",
                           "SD_90minus",
                           "SD_2010plus" )


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

#estimate mean weight (g) for fish 1990 and earlier, based on mean length (mm/10)
econ_conseq <- mutate(econ_conseq, '90.meanweight.g' =
                        as.numeric(ifelse(Species == 'chinook',
                                          (chinook.a*((X90minus.mean)^chinook.b))*1000, 
                                          ifelse(Species == 'coho', 
                                                 (coho.a*((X90minus.mean)^coho.b))*1000, 
                                                 ifelse(Species == 'pink',
                                                        (pink.a*((X90minus.mean)^pink.b))*1000, 
                                                        ifelse(Species == 'sockeye',
                                                               (sockeye.a*((X90minus.mean)^sockeye.b))*1000, 
                                                               ifelse(Species == 'chum',
                                                                      (chum.a*((X90minus.mean)^chum.b))*1000,
                                                                      "NA"
                                                               )))))))

#estimate mean weight (g) for fish 2010 and later, based on mean length (mm/10)
econ_conseq <- mutate(econ_conseq, '2010.meanweight.g' =
                        as.numeric(ifelse(Species == 'chinook',
                                          (chinook.a*((X2010plus.mean)^chinook.b))*1000, 
                                          ifelse(Species == 'coho', 
                                                 (coho.a*((X2010plus.mean)^coho.b))*1000, 
                                                 ifelse(Species == 'pink',
                                                        (pink.a*((X2010plus.mean)^pink.b))*1000, 
                                                        ifelse(Species == 'sockeye',
                                                               (sockeye.a*((X2010plus.mean)^sockeye.b))*1000, 
                                                               ifelse(Species == 'chum',
                                                                      (chum.a*((X2010plus.mean)^chum.b))*1000,
                                                                      "NA"
                                                               )))))))

#calculate raw (delta, D) and percent (%) change in estimated mean weight pre-1990 vs. post-2010
econ_conseq <- mutate(econ_conseq, 'D.weight.g' =
                        econ_conseq$`2010.meanweight` - econ_conseq$`90.meanweight`
)

#convert weight from g to lbs
#we need this because exvessel prices are price per lb

econ_conseq <- econ_conseq %>%  
  mutate('90.meanweight.lb' = `90.meanweight.g`*0.00220462) %>% 
  mutate('2010.meanweight.lb' = `2010.meanweight.g`*0.00220462) %>% 
  mutate('D.weight.lb' = D.weight.g*0.00220462)

#=============================================================================================

#we want to base each price in the most recent value we have that's species- and region-specific
exvessel.current <- exvessel_prices %>% 
  group_by(Area.Name, Species.Name.Common) %>% 
  summarise(max.year = max(Year)) %>% 
  left_join(., exvessel_prices, by = c("Area.Name", "Species.Name.Common", "max.year"="Year"))

#clean it up and make it joinable to the consequences table
#colnames(exvessel.current) <- c("SASAP.Region", "Species", "max.year", "price") #corrupts tibble
exvessel.current <- exvessel.current %>% rename(SASAP.Region = Area.Name, Species = Species.Name.Common,
                                                price = Average.Exvessel.Price.per.Pound)

exvessel.current$SASAP.Region <- as.factor(exvessel.current$SASAP.Region)
exvessel.current$Species <- as.factor(exvessel.current$Species)

levels(exvessel.current$SASAP.Region)[levels(exvessel.current$SASAP.Region)=="Alaska Peninsula"] <- "Alaska Peninsula and Aleutian Islands"
levels(exvessel.current$Species) <- list("sockeye"="Sockeye", "pink"="Pink", "coho"="Coho", "chum"="Chum", "chinook"="Chinook")

str(exvessel.current)
unique(exvessel.current$SASAP.Region)
unique(exvessel.current$Species)



#Copper River is not uniquely represented in the exvessel report because it is lumped in with Prince William Sound (same management district)
copper.ex <- subset(exvessel.current, SASAP.Region == "Prince William Sound") 
copper.ex$SASAP.Region <-  "Copper River"
exvessel.current <- dplyr::union(exvessel.current, copper.ex, .id=NULL)

#finally, add prices by species and region to each econ_conseq project
econ_conseq <- left_join(econ_conseq, exvessel.current, by=c("Species", "SASAP.Region"))

#calculate price of a "average sized fish" before 1990 and after 2010 and then find the difference
econ_conseq <- mutate(econ_conseq, 'fishprice.90'=`90.meanweight.lb`*`price`)
econ_conseq <- mutate(econ_conseq, 'fishprice.2010'= `2010.meanweight.lb`*`price`)
econ_conseq <- mutate(econ_conseq, 'D.priceperfish'= as.numeric(`fishprice.2010`) - as.numeric(`fishprice.90`))
econ_conseq <- mutate(econ_conseq, 'D.price.proportion'= D.priceperfish/as.numeric(`fishprice.90`))

#=============================================================================================

write.csv(econ_conseq, file=paste0(output.dir,"/econ_conseq_expanded.csv"))

colnames(econ_conseq)
Economic_Consequences <- select(econ_conseq, 
                                "SASAP.Region",
                                "LocationID",
                                "Species",
                                "Percent.90.2010", 
                                "D.90.2010",
                                "D.weight.g", 
                                "D.priceperfish",
                                "D.price.proportion"
)
#rename columns to presentable names
names(Economic_Consequences) <- c(
  'Region',
  'Location',
  'Species',
  '% Change Mean Length',
  'D Mean Length (mm)',
  'D Mass (kg)',
  'D Price Per Fish ($)',
  'D Price Proportion'
)
View(Economic_Consequences)
write.csv(Economic_Consequences, file=paste0(output.dir,"/Economic Consequences.csv"))

table_S7 <- select(econ_conseq, 
                   "SASAP.Region",
                   "LocationID",
                   "Species",
                   "D.weight.lb", 
                   "D.priceperfish")
write.csv(table_S7, file=paste0(output.dir,"/table_s7_econ.csv"))



