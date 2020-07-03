#==================================================================================================
#Project Name: Ecological Consequences
#Created by Krista Oke and Katie Kobayashi
#
#Purpose: Socio-cultural impacts
#           We will explore impacts of size and age declines for 
#           the food security and well-being of subsistence users and communities that rely on salmon 
#           (e.g. fewer calories/fat/protein/servings per fish).
#           Largely a story of Chinook in the Yukon and Kuskokwim
#            
#==================================================================================================


require(tidyverse)

wd <- paste0(getwd(),"/Manuscript_Code")

output.dir <- paste0(wd,"/Output")
figs.dir <- paste0(wd,"/Figs")
data.dir <- paste0(wd,"/Data")
#==========================================================================================

#read in metadata
linear_model_list <- read.csv(paste0(wd,"/Data/consequences Linear Model List.csv"),header = TRUE) %>% select(-X)

chinook_nuts <- read.csv(paste0(data.dir, "/chinook-reform.csv"), header = TRUE)
chum_nuts <- read.csv(paste0(data.dir, "/chum.csv"), header = TRUE)
sock_nuts <- read.csv(paste0(data.dir, "/sockeye_nutrients_cleaned.csv"), header = TRUE)
coho_nuts <- read.csv(paste0(data.dir, "/coho_nutrients_cleaned.csv"), header = TRUE)


# subset for projects/regions/spp. of interest
#we are interested in all subsistence projects that produced a non-NA linear model output
soc_linear_models <- subset(linear_model_list, ASLProjectType %in% c("subsistence catch", "commercial catch") ) %>% 
  subset(!is.na(X..change.90minus.2010plus))
#===========================================================================================

# Use species, locations, and "change.90minus.2010plus" (-> Dbodysize)
# from linear models list as base for subsistence consequences table
soc_conseq <- select(soc_linear_models,
                     SASAP.Region, 
                     LocationID, 
                     Species, 
                     X90minus.mean,
                     X2010plus.mean,
                     D.90.2010,
                     X..change.90minus.2010plus)

colnames(soc_conseq) <- c("SASAP.Region", 
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

#estimate mean weight (g) for fish 1990 and earlier, based on mean length (mm/10)
soc_conseq <- mutate(soc_conseq, '90.meanweight.g' =
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

#estimate mean weight (g) for fish 2010 and later, based on mean length (mm/10)
soc_conseq <- mutate(soc_conseq, '2010.meanweight.g' =
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
soc_conseq <- mutate(soc_conseq, 'D.weight.g' = soc_conseq$`2010.meanweight` - soc_conseq$`90.meanweight`)


#convert weight from g to lbs
#we need this because consumption rates are often in lb

soc_conseq <- soc_conseq %>%  
  mutate('90.meanweight.lb' = `90.meanweight.g`*0.00220462) %>% 
  mutate('2010.meanweight.lb' = `2010.meanweight.g`*0.00220462) %>% 
  mutate('D.weight.lb' = D.weight.g*0.00220462)

#===========================================================================================
#Define nutrition PER GRAM values
#Chinook
chin_serving <- 100

chin_recovery <- 0.55

chin_protein <- chinook_nuts %>% 
  subset(Nutrient == "Protein") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

chin_calories <- chinook_nuts %>% 
  subset(Nutrient == "Energy") %>% 
  subset(Unit == "kcal") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

chin_fat <- chinook_nuts %>% 
  subset(Nutrient == "Total lipid (fat)") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

#Chum

chum_serving <- 100

chum_recovery <- 0.60

chum_protein <- chum_nuts %>% 
  subset(Nutrient == "Protein") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

chum_calories <- chum_nuts %>% 
  subset(Nutrient == "Energy") %>% 
  subset(Unit == "kcal") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

chum_fat <- chum_nuts %>% 
  subset(Nutrient == "Total lipid (fat)") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM


#Coho

coho_serving <- 100

coho_recovery <- 0.57

coho_protein <- coho_nuts %>% 
  subset(Nutrient == "Protein") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

coho_calories <- coho_nuts %>% 
  subset(Nutrient == "Energy") %>% 
  subset(Unit == "kcal") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

coho_fat <- coho_nuts %>% 
  subset(Nutrient == "Total lipid (fat)") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM



#Coho

sock_serving <- 100

sock_recovery <- 0.53

sock_protein <- sock_nuts %>% 
  subset(Nutrient == "Protein") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

sock_calories <- sock_nuts %>% 
  subset(Nutrient == "Energy") %>% 
  subset(Unit == "kcal") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM

sock_fat <- sock_nuts %>% 
  subset(Nutrient == "Total lipid (fat)") %>% 
  .[['Value.100g.']]/100 #divide by 100 to return value PER GRAM




#============================================================================================
#calculate recovered (edible) mass per fish
soc_conseq <- mutate(soc_conseq, '90.mass.recovered' = ifelse(Species == 'chinook', as.numeric(`90.meanweight.g`*chin_recovery), 
                                                              ifelse(Species == 'chum', as.numeric(`90.meanweight.g`*chum_recovery),
                                                                     ifelse(Species == 'coho', as.numeric(`90.meanweight.g`*coho_recovery),
                                                                            ifelse(Species == 'sockeye', as.numeric(`90.meanweight.g`*sock_recovery), "NA")))))
soc_conseq <- mutate(soc_conseq, '2010.mass.recovered' = ifelse(Species == 'chinook', as.numeric(`2010.meanweight.g`*chin_recovery), 
                                                                ifelse(Species == 'chum', as.numeric(`2010.meanweight.g`*chum_recovery), 
                                                                       ifelse(Species == 'coho', as.numeric(`2010.meanweight.g`*coho_recovery),
                                                                              ifelse(Species == 'sockeye', as.numeric(`2010.meanweight.g`*sock_recovery),"NA")))))
soc_conseq$`90.mass.recovered` <- as.numeric(soc_conseq$`90.mass.recovered`)
soc_conseq$`2010.mass.recovered` <- as.numeric(soc_conseq$`2010.mass.recovered`)

soc_conseq <- mutate(soc_conseq, 'd.mass.recovered.lb' = (`2010.mass.recovered` -`90.mass.recovered`)*0.00220462)







#calculate change in protein
soc_conseq <- mutate(soc_conseq, 'protein.90' = ifelse(Species == 'chinook', as.numeric(`90.mass.recovered`*chin_protein), 
                                                       ifelse(Species == 'chum', as.numeric(`90.mass.recovered`*chum_protein),
                                                              ifelse(Species == 'coho', as.numeric(`90.mass.recovered`*coho_protein),
                                                                     ifelse(Species == 'sockeye', as.numeric(`90.mass.recovered`*sock_protein), "NA")))))  
soc_conseq <- mutate(soc_conseq, 'protein.10' = ifelse(Species == 'chinook', as.numeric(`2010.mass.recovered`*chin_protein), 
                                                       ifelse(Species == 'chum', as.numeric(`2010.mass.recovered`*chum_protein),
                                                              ifelse(Species == 'coho', as.numeric(`2010.mass.recovered`*coho_protein),
                                                                     ifelse(Species == 'sockeye', as.numeric(`2010.mass.recovered`*sock_protein), "NA")))))
soc_conseq <- mutate(soc_conseq, 'D.protein'= as.numeric(`protein.10`) - as.numeric(`protein.90`))

#calculate change in fat
soc_conseq <- mutate(soc_conseq, 'fat.90' = ifelse(Species == 'chinook', as.numeric(`90.mass.recovered`*chin_fat), 
                                                   ifelse(Species == 'chum', as.numeric(`90.mass.recovered`*chum_fat),
                                                          ifelse(Species == 'coho', as.numeric(`90.mass.recovered`*coho_fat),
                                                                 ifelse(Species == 'sockeye', as.numeric(`90.mass.recovered`*sock_fat), "NA")))))  
soc_conseq <- mutate(soc_conseq, 'fat.10' = ifelse(Species == 'chinook', as.numeric(`2010.mass.recovered`*chin_fat), 
                                                   ifelse(Species == 'chum', as.numeric(`2010.mass.recovered`*chum_fat), 
                                                          ifelse(Species == 'coho', as.numeric(`2010.mass.recovered`*coho_fat),
                                                                 ifelse(Species == 'sockeye', as.numeric(`2010.mass.recovered`*sock_fat), "NA")))))
soc_conseq <- mutate(soc_conseq, 'D.fat'= as.numeric(`fat.10`) - as.numeric(`fat.90`))

#calculate change in total calories              
soc_conseq <- mutate(soc_conseq, 'calories.90' = ifelse(Species == 'chinook', as.numeric(`90.mass.recovered`*chin_calories), 
                                                        ifelse(Species == 'chum', as.numeric(`90.mass.recovered`*chum_calories), 
                                                               ifelse(Species == 'coho', as.numeric(`90.mass.recovered`*coho_calories), 
                                                                      ifelse(Species == 'sockeye', as.numeric(`90.mass.recovered`*sock_calories), "NA")))))  
soc_conseq <- mutate(soc_conseq, 'calories.10' = ifelse(Species == 'chinook', as.numeric(`2010.mass.recovered`*chin_calories), 
                                                        ifelse(Species == 'chum', as.numeric(`2010.mass.recovered`*chum_calories), 
                                                               ifelse(Species == 'coho', as.numeric(`2010.mass.recovered`*coho_calories), 
                                                                      ifelse(Species == 'sockeye', as.numeric(`2010.mass.recovered`*sock_calories), "NA")))))
soc_conseq <- mutate(soc_conseq, 'D.calories'= as.numeric(`calories.10`) - as.numeric(`calories.90`))

#calculate change in servings
soc_conseq <- mutate(soc_conseq, 'servings.90' = ifelse(Species == 'chinook', as.numeric(`90.mass.recovered`/chin_serving), 
                                                        ifelse(Species == 'chum', as.numeric(`90.mass.recovered`/chum_serving),
                                                               ifelse(Species == 'coho', as.numeric(`90.mass.recovered`/coho_serving),
                                                                      ifelse(Species == 'sockeye', as.numeric(`90.mass.recovered`/sock_serving), "NA")))))  
soc_conseq <- mutate(soc_conseq, 'servings.10' = ifelse(Species == 'chinook', as.numeric(`2010.mass.recovered`/chin_serving), 
                                                        ifelse(Species == 'chum', as.numeric(`2010.mass.recovered`/ chum_serving),
                                                               ifelse(Species == 'coho', as.numeric(`2010.mass.recovered`/coho_serving),
                                                                      ifelse(Species == 'sockeye', as.numeric(`2010.mass.recovered`/sock_serving), "NA")))))
soc_conseq <- mutate(soc_conseq, 'D.servings'= as.numeric(`servings.10`) - as.numeric(`servings.90`))


#Calculate change in meals per fish
chinook_portion_size <- (202 + 252)/2
  chum_portion_size <- (186 + 145)/2
  
  sockeye_portion_size <- (169 + 158)/2
  coho_portion_size <- (149 + 207)/2

soc_conseq <- mutate(soc_conseq, 'meals.90' = ifelse(Species == 'chinook', as.numeric(`90.mass.recovered`/chinook_portion_size), 
                                                        ifelse(Species == 'chum', as.numeric(`90.mass.recovered`/chum_portion_size), 
                                                               ifelse(Species == 'coho', as.numeric(`90.mass.recovered`/coho_portion_size),
                                                                      ifelse(Species == 'sockeye', as.numeric(`90.mass.recovered`/sockeye_portion_size), "NA")))))  
soc_conseq <- mutate(soc_conseq, 'meals.10' = ifelse(Species == 'chinook', as.numeric(`2010.mass.recovered`/chinook_portion_size), 
                                                        ifelse(Species == 'chum', as.numeric(`2010.mass.recovered`/ chum_portion_size),
                                                               ifelse(Species == 'coho', as.numeric(`2010.mass.recovered`/coho_portion_size),
                                                                      ifelse(Species == 'sockeye', as.numeric(`2010.mass.recovered`/sockeye_portion_size), "NA")))))
soc_conseq <- mutate(soc_conseq, 'D.meals'= as.numeric(`meals.10`) - as.numeric(`meals.90`))

soc_conseq <- mutate(soc_conseq, 'D.meals.proportion'= D.meals/as.numeric(`meals.90`))


#================================================================================================
#print it up

write.csv(soc_conseq, file=paste0(output.dir,"/soc_conseq_expanded.csv"))

colnames(soc_conseq)

Subsistence_consequences <- select(soc_conseq,
                                   "SASAP.Region",               
                                   "LocationID",                 
                                   "Species",    
                                   "Percent.90.2010", 
                                   "D.90.2010",
                                   "D.weight.g",
                                   "D.protein",                  
                                   "D.fat",                      
                                   "D.calories",
                                   "D.servings",
                                   "D.meals",
                                   "D.meals.proportion")

names(Subsistence_consequences) <- c('Region',
                                     'Location',
                                     'Species',
                                     '% Change Mean Length',
                                     'D Mean Length (mm)',
                                     'D Mass (g)',
                                     'D Protein per fish (g)',
                                     'D Lipids (fats) per fish (g)',
                                     'D Total Calories per fish (kcal)',
                                     "D Servings",
                                     "D meals",
                                     "D meals proportion"
)

View(Subsistence_consequences)
write.csv(Subsistence_consequences, file=paste0(output.dir,"/Subsistence Consequences.csv"))

table_S5 <- select(soc_conseq,
                                   "SASAP.Region",               
                                   "LocationID",                 
                                   "Species",  
                                   "D.90.2010",
                                   "Percent.90.2010", 
                                   "D.weight.lb" ,
                                   "d.mass.recovered.lb",
                                   "D.protein",                  
                                   "D.fat",                      
                                   "D.calories",
                                   "D.servings",
                                   "D.meals")
write.csv(table_S5, file=paste0(output.dir,"/table_s5_subsistence.csv"))


