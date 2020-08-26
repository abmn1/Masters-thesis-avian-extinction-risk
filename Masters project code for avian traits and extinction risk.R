# Setting up and loading required packages
setwd("~/Downloads")

install.packages("expss")
install.packages("ordinal")
install.packages("ggiraphExtra")
install.packages("ggeffects")
install.packages("jtools")
install.packages("multcomp")
install.packages("sjPlot")
install.packages("sjlabelled")
install.packages("sjmisc")
install.packages("effects")
install.packages("lmerTest")
install.packages("RVAideMemoire")
install.packages("gridExtra")
install.packages("interactions")
install.packages("grid")
library(grid)
library(interactions)
library(effects)
library(ggpubr)
library(RVAideMemoire)
library(lmerTest)
library (dplyr)
library(tidyr)
library(lme4)
library(ordinal)
library(expss)
library(Hmisc)
require(ggiraphExtra)
require(ggeffects)
require(lmtest)
library(jtools)
library(multcomp)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(effects)
library(lmtest)
library(car)
library(gridExtra)

# Filter and delete insectivores, analysis is focused on Granivores and Frugivores

mastersdata<-read.csv("masters_data (1).csv", header=TRUE, sep=",")
mastersdata = filter(mastersdata, !(TrophicNiche %in% c("Invertivore", "Omnivore", "Vertivore", "Aquatic predator", "Nectarivore", "Scavenger", "Herbivore aquatic", "Herbivore terrestrial")))

mastersdata = mastersdata %>% drop_na(TrophicNiche)

# Read in trait data file, threats and IUCN Red list category files

BLThreats<-read.table("BL_Threats_2.csv", header=TRUE, sep=",")

BLThreats <- BLThreats %>% rename(Birdlife_Name = Scientific.name)

# Import/merge the 2020 Red List categories data and Birdlife threats into the Species traits file, merging by species

mastersdata <- merge(x = mastersdata, y = BLThreats, by = "Birdlife_Name", all.x=TRUE)

mastersdata <- distinct(mastersdata, .keep_all = TRUE)

write.csv(mastersdata,'mastersdata.csv')


#Make a new column for EDGE Scores from 2020 red list categories re-coded from 0.1 to 4.1, i.e LC = 0.1, CR = 4.1.

#Fill in missing values for 2020 categories with 'Least Concern'

mastersdata <- mastersdata %>% replace_na(list(RL.Category = "LC"))

#(1) Make a new column replicating RL Categories 2020 called EDGE_SCORE

mastersdata <- mastersdata %>% mutate(EDGE_SCORE = RL.Category)

# (2) Make sure Red List category is a factor variable

mastersdata$EDGE_SCORE <- as.factor(mastersdata$EDGE_SCORE)

# (3) Recode Red List Categories as EDGE scores from 0-4

mastersdata <- mastersdata %>% mutate(EDGE_SCORE=recode(EDGE_SCORE, 'LC'="0.1", 'NT'="1.1", 'VU'="2.1", 'EN'="3.1" , 'CR'="4.1"))

# (4) Drop DD and EX NA rows from the EDGE SCORE column.

mastersdatathreatmap<- mastersdata %>% drop_na(EDGE_SCORE)


# Remove duplicates from the BirdLife threats file based on Birdlife_Names

mastersdatathreatenedmap <- mastersdata[!duplicated(mastersdata$Birdlife_Name), ]

mastersdatathreatenedmap <- select(mastersdatathreatenedmap, Birdlife_Name, EDGE_SCORE, RL.Category)

write.csv(mastersdatathreatenedmap,'mastersdatathreatenedmap.csv')


# Add new columns for proprtion of threatened species maps by a particular threat, code all other threatened types as 0 and threat in question 1

mastersdatathreatmap <- unite(mastersdatathreatmap, "Threat.level.new", Threat.level.1, Threat.level.2, sep = "", remove = FALSE)

# Merge threat categories into one colum and incorporate deatiled threats into a general threat type.
mastersdatathreatmap <- mastersdatathreatmap %>% mutate(Threat.level.new=recode(Threat.level.new,  'PollutionAir-borne pollutants' = "Pollution", 'Other optionsOther threat' = "Other threat", 'Invasive and other problematic species, genes & diseasesDiseases of unknown cause' = "Invasive and other problematic species, genes & diseases", 'Geological eventsEarthquakes/tsunamis' = "Geological events", 'Geological eventsVolcanoes' = "Geological events", 'Transportation & service corridorsUtility & service lines' = "Human development & disturbance",  'Invasive and other problematic species, genes & diseasesViral/prion-induced diseases' = "Invasive and other problematic species, genes & diseases", 'Invasive and other problematic species, genes & diseasesIntroduced genetic material' = "Invasive and other problematic species, genes & diseases", 'Invasive and other problematic species, genes & diseasesProblematic species/disease of unknown origin' = "Invasive and other problematic species, genes & diseases", 'Human intrusions & disturbanceWar, civil unrest & military exercises' = "Human development & disturbance", 'Agriculture & aquacultureMarine & freshwater aquaculture' = 	"Agriculture & aquaculture", 'Climate change & severe weatherDroughts' = "Climate change & severe weather",  'Climate change & severe weatherOther impacts' = "Climate change & severe weather",  'Climate change & severe weatherTemperature extremes' =  "Climate change & severe weather", 'Energy production & miningRenewable energy' = "Energy production & mining", 'Biological resource useLogging & wood harvesting'="Logging & wood harvesting", 'Agriculture & aquacultureLivestock farming & ranching' = "Agriculture & aquaculture", 'Agriculture & aquacultureWood & pulp plantations' = "Agriculture & aquaculture", 'Biological resource useFishing & harvesting aquatic resources' = "Biological resource use", 'Biological resource useGathering terrestrial plants' = "Biological resource use", 'Climate change & severe weatherHabitat shifting & alteration' = "Climate change & severe weather", 'Climate change & severe weatherStorms & flooding' = "Climate change & severe weather", 'Energy production & miningMining & quarrying' = "Energy production & mining", 'Human intrusions & disturbanceWork & other activities' = "Human development & disturbance", 'Agriculture & aquacultureAnnual & perennial non-timber crops'="Agriculture & aquaculture", 'Biological resource useHunting & trapping terrestrial animals'="Hunting & trapping terrestrial animals", 'Natural system modificationsOther ecosystem modifications' = "Natural system modifications", 'Climate change & severe weatherHabitat shifting & alteration	'="Climate change & severe weather" , 'Invasive and other problematic species, genes & diseasesInvasive non-native/alien species/diseases' = "Invasive and other problematic species, genes & diseases" , 'Invasive and other problematic species, genes & diseasesProblematic native species/diseases' = "Invasive and other problematic species, genes & diseases", 'Human intrusions & disturbanceRecreational activities' = "Human development & disturbance", 'Residential & commercial developmentCommercial & industrial areas' = "Human development & disturbance", 'Residential & commercial developmentHousing & urban areas' = "Human development & disturbance", 'Residential & commercial developmentTourism & recreation areas' = "Human development & disturbance", 'Transportation & service corridorsFlight paths' = "Human development & disturbance",	'Transportation & service corridorsRoads & railroads'= "Human development & disturbance", 'PollutionAgricultural & forestry effluents' = "Pollution" , 'PollutionIndustrial & military effluents' = "Pollution", 'Natural system modificationsDams & water management/use' = "Natural system modifications", 'Natural system modificationsFire & fire suppression' = "Natural system modifications")) 		

# Identify most common threats for use in two proportion of threatened species by threats maps and to knock out threat categories affecting under 50 species
table(mastersdatathreatmap$Threat.level.new, useNA="ifany")

#Remove duplicates.
mastersdatathreatmap <- distinct(mastersdatathreatmap, Birdlife_Name, Threat.level.new, .keep_all = TRUE)

mastersdatathreatmap <- na_if(mastersdatathreatmap, "NANA")

write.csv(mastersdatathreatmap,'mastersdatathreatmap.csv')
mastersdata <- read.csv("mastersdatathreatmap.csv")

# Create threat columns for each species.
mastersdatathreatmap <- mastersdatathreatmap %>% mutate(proportion_agriculture = Threat.level.new) 

mastersdatathreatmap  <-  mastersdatathreatmap %>% mutate(proportion_agriculture=recode(proportion_agriculture, 'Agriculture & aquaculture'="1", 
                                                                                        'Biological resource use'= "0", 
                                                                                        'Human development & disturbance'="0", 
                                                                                        'Invasive and other problematic species, genes & diseases'="0", 
                                                                                        'Natural system modifications'="0", 
                                                                                        'Climate change & severe weather' = "0",
                                                                                        'Hunting & trapping terrestrial animals' = "0", 
                                                                                        'Geological events' = "0",
                                                                                        'Energy production & mining' = "0",
                                                                                        'Logging & wood harvesting' = "0",
                                                                                        'Pollution' = "0",
                                                                                        'Other threat' = "0"))


mastersdatathreatmap  <- mastersdatathreatmap  %>% mutate(proportion_logging = Threat.level.new)



mastersdatathreatmap  <- mastersdatathreatmap  %>% mutate(proportion_logging=recode(proportion_logging, 'Logging & wood harvesting' = "1",
                                                                                    'Agriculture & aquaculture'="0", 
                                                                                    'Biological resource use'= "0", 
                                                                                    'Human development & disturbance'="0", 
                                                                                    'Invasive and other problematic species, genes & diseases'="0", 
                                                                                    'Natural system modifications'="0", 
                                                                                    'Climate change & severe weather' = "0",
                                                                                    'Hunting & trapping terrestrial animals' = "0", 
                                                                                    'Geological events' = "0",
                                                                                    'Energy production & mining' = "0",
                                                                                    'Pollution' = "0",
                                                                                    'Energy production & mining' = "0",
                                                                                    'Other threat' = "0"))


mastersdatathreatmap  <- mastersdatathreatmap  %>% mutate(proportion_hunting = Threat.level.new)



mastersdatathreatmap  <- mastersdatathreatmap  %>% mutate(proportion_hunting=recode(proportion_hunting,'Hunting & trapping terrestrial animals' = "1",
                                                                                    'Logging & wood harvesting' = "0",
                                                                                    'Agriculture & aquaculture'="0", 
                                                                                    'Biological resource use'= "0", 
                                                                                    'Human development & disturbance'="0", 
                                                                                    'Invasive and other problematic species, genes & diseases'="0", 
                                                                                    'Natural system modifications'="0", 
                                                                                    'Climate change & severe weather' = "0",
                                                                                    'Geological events' = "0",
                                                                                    'Energy production & mining' = "0",
                                                                                    'Pollution' = "0",
                                                                                    'Energy production & mining' = "0",
                                                                                    'Other threat' = "0"))


mastersdatathreatmap <-  mastersdatathreatmap %>% replace_na(list(proportion_hunting = "0"))
mastersdatathreatmap <-  mastersdatathreatmap %>% replace_na(list(proportion_logging = "0"))
mastersdatathreatmap <-  mastersdatathreatmap %>% replace_na(list(proportion_agriculture = "0"))

write.csv(mastersdatathreatmap,'mastersdatathreatmap.csv')


#Add species richness column for species richness map
rm(list=ls())

mastersdataspeciesrichness <- read.csv("mastersdatathreatenedmap.csv", header=TRUE, sep=",")

mastersdataspeciesrichness <- mastersdataspeciesrichness %>% mutate(species_richness = "1")

write.csv(mastersdataspeciesrichness,'mastersdataspeciesrichness.csv')

mastersdata <- read.csv("mastersdatathreatmap.csv")

mastersdata <- select(mastersdata, -c(Threat.level.1, Threat.level.2, Redlist_category_2014, Primary_Foraging_strata_2, Primary_Foraging_strata_3, Primary_Foraging_strata_4, Primary_Foraging_strata_5))

mastersdata <- mastersdata %>% mutate(Threatened = RL.Category)

mastersdata <- mastersdata %>% mutate(Threatened=recode(Threatened, 'LC'="0", 'NT'="1", 'VU'="1", 'EN'="1" , 'CR'="1"))

mastersdata <- mastersdata %>% mutate(RL.Category.Scale = RL.Category)

mastersdata <- mastersdata %>% mutate(RL.Category.Scale=recode(RL.Category.Scale, 'LC'="0", 'NT'="1", 'VU'="2", 'EN'="3" , 'CR'="4"))

mastersdata <- mastersdata %>% rename(Threat.Type = Threat.level.new) 

mastersdata <- mastersdata %>% rename(HWIndex = Hand.Wing.Index..Claramunt.2011.)

write.csv(mastersdata,'mastersdataanalysis.csv')

mastersdata<-read.csv("mastersdataanalysis.csv")

# Create new threat categories: Logging & wood harvesting, Agriculture & aquaculture, Biological resource use, Human development & disturbance, Invasive and other problematic species, genes & diseases.

mastersdata  <- mastersdata %>% mutate(Threatened_Human = Threat.Type)

mastersdata  <- mastersdata %>% mutate(Threatened_Logging = Threat.Type)

mastersdata  <- mastersdata  %>% mutate(Threatened_Human=recode(Threatened_Human, 'Logging & wood harvesting' = "0",
                                                                'Agriculture & aquaculture'="0", 
                                                                'Biological resource use'= "0", 
                                                                'Human development & disturbance'="1", 
                                                                'Invasive and other problematic species, genes & diseases'="0", 
                                                                'Natural system modifications'="0", 
                                                                'Climate change & severe weather' = "0",
                                                                'Hunting & trapping terrestrial animals' = "0", 
                                                                'Geological events' = "0",
                                                                'Energy production & mining' = "0",
                                                                'Pollution' = "0",
                                                                'Energy production & mining' = "0",
                                                                'Other threat' = "0",
                                                                'NA' = "0"))

mastersdata  <- mastersdata  %>% mutate(Threatened_Logging=recode(Threatened_Logging, 'Logging & wood harvesting' = "1",
                                                                  'Agriculture & aquaculture'="0", 
                                                                  'Biological resource use'= "0", 
                                                                  'Human development & disturbance'="0", 
                                                                  'Invasive and other problematic species, genes & diseases'="0", 
                                                                  'Natural system modifications'="0", 
                                                                  'Climate change & severe weather' = "0",
                                                                  'Hunting & trapping terrestrial animals' = "0", 
                                                                  'Geological events' = "0",
                                                                  'Energy production & mining' = "0",
                                                                  'Pollution' = "0",
                                                                  'Energy production & mining' = "0",
                                                                  'Other threat' = "0",
                                                                  'NA' = "0"))

mastersdata <- mastersdata %>% rename(Threatened_Hunting = proportion_hunting, Threatened_Agri = proportion_agriculture)


# Classify non-threatened species as LC (0).

mastersdata <-  mastersdata %>% replace_na(list(Threatened_Hunting = "0"))

mastersdata <-  mastersdata %>% replace_na(list(Threatened_Human = "0"))

mastersdata <-  mastersdata %>% replace_na(list(Threatened_Logging = "0"))

mastersdata <-  mastersdata %>% replace_na(list(Threatened_Agri = "0"))


# Label variables and classify categorical variables as factors.
mastersdata$Threatened_Agri <- as.factor(mastersdata$Threatened_Agri)
mastersdata$Threatened_Hunting <- as.factor(mastersdata$Threatened_Hunting)
mastersdata$Threatened_Logging <- as.factor(mastersdata$Threatened_Logging)
mastersdata$Threatened_Human <- as.factor(mastersdata$Threatened_Human)
mastersdata$Migration <- as.factor(mastersdata$Migration)
mastersdata$Territory.2016 <- as.factor(mastersdata$Territory.2016)
mastersdata$Habitat <- as.factor(mastersdata$Habitat)
mastersdata$SexualSelectionUnidirectional <- as.factor(mastersdata$SexualSelectionUnidirectional)


mastersdata  <- mastersdata  %>% mutate(Territory.2016=recode(Territory.2016,
                                                              'none' = "0", 'weak'="1", 'strong'= "2"))

mastersdata  <- mastersdata %>% mutate(Migration=recode(Migration, '1' = "1", '2'="2", '3'= "3"))


mastersdata$Migration <- as.factor(mastersdata$Migration)
mastersdata$Territory.2016 <- as.factor(mastersdata$Territory.2016)
mastersdata$Habitat <- as.factor(mastersdata$Habitat)



mastersdata  <- mastersdata  %>% mutate(ForagingNiche=recode(ForagingNiche, 'Fugivore aerial' = "aerial",
                                                             'Fugivore glean'="glean",
                                                             'Fugivore ground'="ground",
                                                             'Granivore arboreal' ="arboreal",
                                                             'Granivore ground' = "ground",
                                                             'Generalist' = "generalist",))

mastersdata  <- mastersdata  %>% mutate(Primary_Foraging_strata_1=recode(Primary_Foraging_strata_1, 'Mid Canopy' = "Canopy"))

write.csv(mastersdata,'mastersdataanalysis.csv')


#----------------------------------
# Heat Map for threatened bird distributions
#---------------------------------
rm(list=ls())

install.packages("raster")
install.packages("sf")
install.packages("ggplot2")
install.packages("tidyr")

library(sf) #for bird distributions
library(raster) # for creating and working with raster data
library(ggplot2) #for plotting the heatmap
library(tidyr)
library(Cairo)

#----------------------------------
#setting up 
#---------------------------------
# - reading in all the birdlife species distribution maps
# - reading in BirdLife data
# - setting an empty raster
# - setting an empty raster_stack
#----------------------------------

Ahmad$EDGE_SCORE <- as.numeric(Ahmad$EDGE_SCORE)
Ahmad<- Ahmad %>% drop_na(EDGE_SCORE)


setwd("/Users/ahmadnafi/Downloads")
maps <- st_read("/Users/ahmadnafi/Dropbox/BOTW2/BOTW.gdb", layer = "All_Species") #Assign the .gdb folder and this contains All_Species info. (can't see this in your file explorer if you open it in arcGIS you can see it)
Ahmad <- read.csv("mastersdatathreatenedmap.csv") # this is to get the species names from your data and the threat level

r <- raster(ncols= 4320, nrows = 1800, ymn = -60) #this is blank raster at same resolution and extents as the bioclim variables - climatic data rasters #you can increase the resolution by increasing cols and rows. If you want to do this, I recommend using 4320x1800 or 8640x3600 as these are the same resolutions as the higher res climate rasters 
raster_stack <- r #set raster_stack to same as r. we will project species distribution onto r. and then layer each raster into the raster_stack

#------------------------
#The Loop
#-------------------------
# - Pull out distribution map for each species
# - Reformat the maps (only needed for some but has to be added to the loop)
# - Combine all the maps into one polygon (many of the species have multiple maps for different aspects of their range. e.g. breeding, witering, introduced etc.)
# - Turn the polygon into a raster i.e. fill in data for the raster grid cells which correspond to the species distribution map
# - set the data of these grid cells to the threat_score of the species. i.e. LC=1, VU=3, CR=5
# - Add layer into a raster_stack
# - condense the raster_stack into a raster_brick (less memory)
#------------------------

for (i in 1:nrow(Ahmad)) {
  print(i) #print out which row we are on to keep track of how far we have gotten
  s <- as.character(Ahmad$Birdlife_Name[i]) #select species from your data
  map_i <- subset(maps, maps$SCINAME == s) #select the maps that correspond to your species
  
  for (j in 1:length(map_i$Shape)) { 
    try(map_i$Shape[[j]] <- st_cast(map_i$Shape[[j]], 'MULTIPOLYGON')) #maps are normally stored as multipolygons. but not always. this reformats each map into a multipolugon (needed for creating the raster)
  }
  try(map_i$Shape <- st_cast(map_i$Shape, 'MULTIPOLYGON')) #sometimes the above forloop doesnt reformat it as it is stored differently. In this case, this works. Tryloops can be really helpful normally if there is an error the whole loop stops. wrapping in a tryloop means R will TRY it and if it doesnt work R will move on.
  
  polygon <- as_Spatial(st_combine(map_i$Shape)) #combines the all the maps into one. and then converts the data into a SpatialPolygon
  raster_i <- rasterize(polygon, r) #project the polygon onto a raster i.e. give dara tothe raster grid cells corresponding to the species distribution map (polygon) 
  rastercells <- which(getValues(raster_i) > 0)  #select the raster_cells which have been given data
  raster_i[rastercells] <- Ahmad$"EDGE_SCORE"[i] #filled these cells with corresponding data using ifs bellow but you can use this line of code. It fills all the rastercells of the species distribution with the threat score for that species
  raster_stack <- addLayer(raster_stack, raster_i) #stacks the raster layer
  if (i %in% c(500,1000,1500,nrow(Ahmad))) { 
    raster_stack <- brick(raster_stack) #raster_stacks can be very memory inefficient, so at specific points I have condensed to a raster_brick (basically the same thing. brick has less functionality but much less memory usage)
    print(paste0("Bricked_",i))
  }
  #Will then start the loop with the next species and add it to the raster stack.
}

#------------------------------
#Plotting the Heatmap
#------------------------------
#- Creating a raster_layer where each grid cell is the average of all the values in the layers of the raster_stack
#- Plot using ggplot2
#- Playing with the scale
#-----------------------------------

av_full1 <- stackApply(raster_stack, indices = rep(1, nrow(Ahmad)), fun = mean, na.rm = T) #at each grid cell it takes the mean of throughout the layers na.rm means it will only take the means of the ones that have data
writeRaster(av_full1, "Species_Average1.grd", format = "raster")
av_full1 <- raster("Species_Average1.grd")


#ggplot
rasdf <- as.data.frame(av_full1, xy=TRUE) %>% drop_na() #this converts the raster data into a dataframe which is required for plotting

#ggplot changed scale
quantile(rasdf$index_1, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

ggplot() +
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) +
  geom_raster(aes(x=x,y=y, fill=index_1), data=rasdf, interpolate = TRUE) +
  #scale_fill_gradientn(colors = c("skyblue", "red", "red1", "red2","red3","red4","firebrick4"))
  #scale_fill_gradientn(colors = c("skyblue2", "white", "red"), na.value = NA) + 
  #scale_fill_gradientn(colors = c("lightskyblue", "aliceblue", "white", "salmon", "tomato", "red", "red", "red1", "red1", "red2", "red2", "red3", "red3", "red4","firebrick4"), na.value = NA) +
  scale_fill_gradientn(name = "EDGE SCORE", colors = c("#a2ccfb","#fac738", "#FF0510", "#a80000", "#900000", "#790000", "#690000", "#590000", "#490000", "#470000", "#450000", "#420000", "#3d0000", "#380000", "#330000"), na.value = NA) + 
  theme_classic() + theme(legend.position="right", legend.title = element_text(size=12.5), legend.text = element_text(size = 11.5)) + guides(fill = guide_colourbar(barwidth = 2, barheight = 7, ticks = FALSE)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=19)) +
  labs(y = "Latitude", x = "Longitude") +
  annotate("text", x = -178, y = 85, label= "(b)", size = 8)
ggsave(filename = "mapthreat.png", type = "cairo", dpi=1000)



#----------------------------------
# Species richness map
#----------------------------------
maps <- st_read("/Users/ahmadnafi/Dropbox/BOTW2/BOTW.gdb", layer = "All_Species") #Assign the .gdb folder and this contains All_Species info. (can't see this in your file explorer if you open it in arcGIS you can see it)
Ahmad <- read.csv("mastersdataspeciesrichness.csv") # this is to get the species names from your data and the threat level

r <- raster(ncols= 4320, nrows = 1800, ymn = -60) #this is blank raster at same resolution and extents as the bioclim variables - climatic data rasters #you can increase the resolution by increasing cols and rows. If you want to do this, I recommend using 4320x1800 or 8640x3600 as these are the same resolutions as the higher res climate rasters 
raster_stack <- r #set raster_stack to same as r. we will project species distribution onto r. and then layer each raster into the raster_stack

#------------------------
#The Loop
#-------------------------
# - Pull out distribution map for each species
# - Reformat the maps (only needed for some but has to be added to the loop)
# - Combine all the maps into one polygon (many of the species have multiple maps for different aspects of their range. e.g. breeding, wintering, introduced etc.)
# - Turn the polygon into a raster i.e. fill in data for the raster grid cells which correspond to the species distribution map
# - set the data of these grid cells to the threat_score of the species. i.e. LC=1, VU=3, CR=5
# - Add layer into a raster_stack
# - condense the raster_stack into a raster_brick (less memory)
#------------------------

for (i in 1:nrow(Ahmad)) {
  print(i) #print out which row we are on to keep track of how far we have gotten
  s <- as.character(Ahmad$Birdlife_Name[i]) #select species from your data
  map_i <- subset(maps, maps$SCINAME == s) #select the maps that correspond to your species
  
  for (j in 1:length(map_i$Shape)) { 
    try(map_i$Shape[[j]] <- st_cast(map_i$Shape[[j]], 'MULTIPOLYGON')) #maps are normally stored as multipolygons. but not always. this reformats each map into a multipolugon (needed for creating the raster)
  }
  try(map_i$Shape <- st_cast(map_i$Shape, 'MULTIPOLYGON')) #sometimes the above forloop doesnt reformat it as it is stored differently. In this case, this works. Tryloops can be really helpful normally if there is an error the whole loop stops. wrapping in a tryloop means R will TRY it and if it doesnt work R will move on.
  
  polygon <- as_Spatial(st_combine(map_i$Shape)) #combines the all the maps into one. and then converts the data into a SpatialPolygon
  raster_i <- rasterize(polygon, r) #project the polygon onto a raster i.e. give data to the raster grid cells corresponding to the species distribution map (polygon) 
  rastercells <- which(getValues(raster_i) > 0)  #select the raster_cells which have been given data
  raster_i[rastercells] <- Ahmad$"species_richness"[i] #filled these cells with corresponding data using ifs bellow but you can use this line of code. It fills all the rastercells of the species distribution with the threat score for that species
  raster_stack <- addLayer(raster_stack, raster_i) #stacks the raster layer
  if (i %in% c(500,1000,1500,nrow(Ahmad))) { 
    raster_stack <- brick(raster_stack) #raster_stacks can be very memory inefficient, so at specific points I have condensed to a raster_brick (basically the same thing. brick has less functionality but much less memory usage)
    print(paste0("Bricked_",i))
  }
  #Will then start the loop with the next species and add it to the raster stack.
}

#------------------------------
#Plotting the Heatmap
#------------------------------
#- Creating a raster_layer where each grid cell is the average of all the values in the layers of the raster_stack
#-----------------------------------

av_full2 <- stackApply(raster_stack, indices = rep(1, nrow(Ahmad)), fun = sum, na.rm = T) #at each grid cell it takes the mean of throughout the layers na.rm means it will only take the means of the ones that have data
writeRaster(av_full2, "Species_Averagerich.grd", format = "raster")
av_full2 <- raster("Species_rich.grd")



# plot using ggplot2
av_full2[av_full2 == 0] <- NA
rasdf2 <- as.data.frame(av_full2, xy=TRUE) %>% drop_na() #this converts the raster data into a dataframe which is required for plotting

#ggplot adjust changed scale

ggplot() +
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,900)) +
  geom_raster(aes(x=x,y=y, fill=layer), data=rasdf2, interpolate = TRUE) +
  scale_fill_gradientn(name = "Richness", colors = c("#a2ccfb","#fac738", "#fd3f42", "#FF2828", "#E41919", "#CD0B0B", "#BC0A0A", "#A80707", "#790000", "#590000"), na.value = NA) + 
  theme_classic() + theme(legend.position="right", legend.title = element_text(size=12.5), legend.text = element_text(size = 11.5)) + guides(fill = guide_colourbar(barwidth = 2, barheight = 7, ticks = FALSE)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=19)) +
  labs(y = "Latitude", x = "Longitude") +
  annotate("text", x = -178, y = 85, label= "(a)", size = 8)
ggsave(filename = "rich.png", type = "cairo", dpi=1000)


#----------------------------------
# Map for Proportion of threatened species by agriculture
#----------------------------------

maps <- st_read("/Users/ahmadnafi/Dropbox/BOTW2/BOTW.gdb", layer = "All_Species") #Assign the .gdb folder and this contains All_Species info. (can't see this in your file explorer if you open it in arcGIS you can see it)
Ahmad <- read.csv("mastersdataagriculture.csv") # this is to get the species names from your data and the threat level


r <- raster(ncols= 4320, nrows = 1800, ymn = -60) #this is blank raster at same resolution and extents as the bioclim variables - climatic data rasters #you can increase the resolution by increasing cols and rows. If you want to do this, I recommend using 4320x1800 or 8640x3600 as these are the same resolutions as the higher res climate rasters 
raster_stack <- r #set raster_stack to same as r. we will project species distribution onto r. and then layer each raster into the raster_stack

#------------------------
#The Loop
#-------------------------
# - Pull out distribution map for each species
# - Reformat the maps (only needed for some but has to be added to the loop)
# - Combine all the maps into one polygon (many of the species have multiple maps for different aspects of their range. e.g. breeding, wintering, introduced etc.)
# - Turn the polygon into a raster i.e. fill in data for the raster grid cells which correspond to the species distribution map
# - set the data of these grid cells to the threat_score of the species. i.e. LC=1, VU=3, CR=5
# - Add layer into a raster_stack
# - condense the raster_stack into a raster_brick (less memory)
#------------------------

for (i in 1:nrow(Ahmad)) {
  print(i) #print out which row we are on to keep track of how far we have gotten
  s <- as.character(Ahmad$Birdlife_Name[i]) #select species from your data
  map_i <- subset(maps, maps$SCINAME == s) #select the maps that correspond to your species
  
  for (j in 1:length(map_i$Shape)) { 
    try(map_i$Shape[[j]] <- st_cast(map_i$Shape[[j]], 'MULTIPOLYGON')) #maps are normally stored as multipolygons. but not always. this reformats each map into a multipolugon (needed for creating the raster)
  }
  try(map_i$Shape <- st_cast(map_i$Shape, 'MULTIPOLYGON')) #sometimes the above forloop doesnt reformat it as it is stored differently. In this case, this works. Tryloops can be really helpful normally if there is an error the whole loop stops. wrapping in a tryloop means R will TRY it and if it doesnt work R will move on.
  
  polygon <- as_Spatial(st_combine(map_i$Shape)) #combines the all the maps into one. and then converts the data into a SpatialPolygon
  raster_i <- rasterize(polygon, r) #project the polygon onto a raster i.e. give data to the raster grid cells corresponding to the species distribution map (polygon) 
  rastercells <- which(getValues(raster_i) > 0)  #select the raster_cells which have been given data
  raster_i[rastercells] <- Ahmad$"proportion_agriculture"[i] #filled these cells with corresponding data using ifs bellow but you can use this line of code. It fills all the rastercells of the species distribution with the threat score for that species
  raster_stack <- addLayer(raster_stack, raster_i) #stacks the raster layer
  if (i %in% c(500,1000,1500,nrow(Ahmad))) { 
    raster_stack <- brick(raster_stack) #raster_stacks can be very memory inefficient, so at specific points I have condensed to a raster_brick (basically the same thing. brick has less functionality but much less memory usage)
    print(paste0("Bricked_",i))
  }
  #Will then start the loop with the next species and add it to the raster stack.
}

#------------------------------
#Plotting the Heatmap
#------------------------------
#- Creating a raster_layer where each grid cell is the average of all the values in the layers of the raster_stack
#- Plot using ggplot2
#- Playing with the scale
#-----------------------------------

agri_raster<- stackApply(raster_stack, indices = rep(1, nrow(Ahmad)), fun = sum, na.rm = T) #at each grid cell it takes the mean of throughout the layers na.rm means it will only take the means of the ones that have data
writeRaster(agri_raster, "agri_raster.grd", format = "raster")
agri_raster <- raster("agri_raster.grd")

# To create proportion map, divide the number of species threatened by agriculture raster by the species richness raster to create a new proportion raster
av_full2 <- raster("Species_Averagerich.grd")
agri_proportion_raster <- agri_raster / av_full2
writeRaster(agri_proportion_raster, "agri_proportion_raster.grd", format = "raster")
agri_proportion_raster <- raster("agri_proportion_raster.grd")

#ggplot
rasdf3 <- as.data.frame(agri_proportion_raster, xy=TRUE) %>% drop_na() #this converts the raster data into a dataframe which is required for plotting

#ggplot changed scale
quantile(rasdf3$layer, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

ggplot() +
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) +
  geom_raster(aes(x=x,y=y, fill=layer), data=rasdf3, interpolate = TRUE) +
  #scale_fill_manual(values = c("skyblue", "skyblue2", "white", "salmon", "tomato", "red", "red1", "red2","red3","red4","firebrick4")) +
  #scale_fill_gradientn(colors = c("skyblue2", "white", "red"), na.value = NA) + 
  #scale_fill_gradientn(colors = c("lightskyblue", "aliceblue", "white", "salmon", "tomato", "red", "red", "red1", "red1", "red2", "red2", "red3", "red3", "red4","firebrick4"), na.value = NA) +
  scale_fill_gradientn(name = "Proportion", colors = c("#a2ccfb","#fac738", "#FF0510", "#CD0B0B", "#BC0A0A", "#B8141c", "#A80707","#9d0606" ,"#930b0b", "#7A0000", "#710000"), na.value = NA) + 
  theme_classic() + theme(legend.position="right") + theme(legend.title = element_text(size=12.5), legend.text = element_text(size = 11.5)) + guides(fill = guide_colourbar(barwidth = 2, barheight = 7, ticks = FALSE)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=19)) +
  labs(y = "Latitude", x = "Longitude") +
  annotate("text", x = -178, y = 85, label= "(c)", size = 8)
ggsave(filename = "agriculturemap.png", type = "cairo", dpi=1000)


#----------------------------------
# Map for Proportion of threatened species by hunting
#----------------------------------

maps <- st_read("/Users/ahmadnafi/Dropbox/BOTW2/BOTW.gdb", layer = "All_Species") #Assign the .gdb folder and this contains All_Species info. (can't see this in your file explorer if you open it in arcGIS you can see it)
Ahmad <- read.csv("mastersdatahunting.csv") # this is to get the species names from your data and the threat level

r <- raster(ncols= 4320, nrows = 1800, ymn = -60) #this is blank raster at same resolution and extents as the bioclim variables - climatic data rasters #you can increase the resolution by increasing cols and rows. If you want to do this, I recommend using 4320x1800 or 8640x3600 as these are the same resolutions as the higher res climate rasters 
raster_stack <- r #set raster_stack to same as r. we will project species distribution onto r. and then layer each raster into the raster_stack

#------------------------
#The Loop
#-------------------------
# - Pull out distribution map for each species
# - Reformat the maps (only needed for some but has to be added to the loop)
# - Combine all the maps into one polygon (many of the species have multiple maps for different aspects of their range. e.g. breeding, wintering, introduced etc.)
# - Turn the polygon into a raster i.e. fill in data for the raster grid cells which correspond to the species distribution map
# - set the data of these grid cells to the threat_score of the species. i.e. LC=1, VU=3, CR=5
# - Add layer into a raster_stack
# - condense the raster_stack into a raster_brick (less memory)
#------------------------

for (i in 1:nrow(Ahmad)) {
  print(i) #print out which row we are on to keep track of how far we have gotten
  s <- as.character(Ahmad$Birdlife_Name[i]) #select species from your data
  map_i <- subset(maps, maps$SCINAME == s) #select the maps that correspond to your species
  
  for (j in 1:length(map_i$Shape)) { 
    try(map_i$Shape[[j]] <- st_cast(map_i$Shape[[j]], 'MULTIPOLYGON')) #maps are normally stored as multipolygons. but not always. this reformats each map into a multipolugon (needed for creating the raster)
  }
  try(map_i$Shape <- st_cast(map_i$Shape, 'MULTIPOLYGON')) #sometimes the above forloop doesnt reformat it as it is stored differently. In this case, this works. Tryloops can be really helpful normally if there is an error the whole loop stops. wrapping in a tryloop means R will TRY it and if it doesnt work R will move on.
  
  polygon <- as_Spatial(st_combine(map_i$Shape)) #combines the all the maps into one. and then converts the data into a SpatialPolygon
  raster_i <- rasterize(polygon, r) #project the polygon onto a raster i.e. give data to the raster grid cells corresponding to the species distribution map (polygon) 
  rastercells <- which(getValues(raster_i) > 0)  #select the raster_cells which have been given data
  raster_i[rastercells] <- Ahmad$"proportion_hunting"[i] #filled these cells with corresponding data using ifs bellow but you can use this line of code. It fills all the rastercells of the species distribution with the threat score for that species
  raster_stack <- addLayer(raster_stack, raster_i) #stacks the raster layer
  if (i %in% c(500,1000,1500,nrow(Ahmad))) { 
    raster_stack <- brick(raster_stack) #raster_stacks can be very memory inefficient, so at specific points I have condensed to a raster_brick (basically the same thing. brick has less functionality but much less memory usage)
    print(paste0("Bricked_",i))
  }
  #Will then start the loop with the next species and add it to the raster stack.
}

#------------------------------
#Plotting the Heatmap
#------------------------------
#- Creating a raster_layer where each grid cell is the average of all the values in the layers of the raster_stack
#- Plot using ggplot2
#- Playing with the scale
#-----------------------------------

hunting_raster<- stackApply(raster_stack, indices = rep(1, nrow(Ahmad)), fun = sum, na.rm = T) #at each grid cell it takes the mean of throughout the layers na.rm means it will only take the means of the ones that have data
writeRaster(hunting_raster, "hunting_raster.grd", format = "raster")
hunting_raster <- raster("hunting_raster.grd")

# Create proportion map, divide the number of species threatened by hunting raster by the species richness raster to create a new proportion raster
hunting_proportion_raster <- hunting_raster / av_full2
writeRaster(hunting_proportion_raster, "hunting_proportion_raster.grd", format = "raster")
hunting_proportion_raster <- raster("hunting_proportion_raster.grd")


#ggplot
rasdf4 <- as.data.frame(hunting_proportion_raster, xy=TRUE) %>% drop_na() #this converts the raster data into a dataframe which is required for plotting

quantile(rasdf$index_1, prob = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

ggplot() +
  borders("world", colour = "grey", fill = "grey", ylim = c(-60,90)) +
  geom_raster(aes(x=x,y=y, fill=layer), data=rasdf4, interpolate = TRUE) +
  scale_fill_gradientn(name = "Proportion", colors = c("#a2ccfb","#fac738", "#FF6f3d", "#FF0510", "#CD0B0B", "#BC0A0A", "#B8141c", "#A80707","#9d0606" ,"#930b0b", "#7A0000", "#710000"), na.value = NA) + 
  theme_classic() + theme(legend.position="right") + theme(legend.title = element_text(size=12.5), legend.text = element_text(size = 11.5)) + guides(fill = guide_colourbar(barwidth = 2, barheight = 7, ticks = FALSE)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=19)) +
  labs(y = "Latitude", x = "Longitude") +
  annotate("text", x = -178, y = 85, label= "(d)", size = 8)
ggsave(filename = "huntingmap.png", type = "cairo", dpi=1000)


# -------------------------------------------------------------------------------------------------------------------
# Aanlysis using GLMMs and CLMMs for predictors and threat status, threat category and threat severity by threat type.
# -------------------------------------------------------------------------------------------------------------------

mastersdata <- distinct(mastersdata, Birdlife_Name, .keep_all = TRUE)

# Determine number of threatened species
length(unique(mastersdata$Family))
n_distinct(mastersdata$Family)

table(mastersdataagri$Threatened_Agri)
table(mastersdatahunting$Threatened_Hunting)
table(mastersdatalogging$Threatened_Logging)
table(mastersdatahuman$Threatened_Human)

# Check distributions of continuous variables
hist(mastersdata$Bill_Width)
hist(mastersdata$HWIndex)
hist(mastersdata$SpecGenFinalMass, xlim=c(0,6000))
hist(mastersdata$Range_Size, xlim=c(-2000,4000))


# Check correlated variables
mastersdata <- mastersdata %>% drop_na(Habitat, HWIndex, Bill.Width, Migration, Habitat, Body.Mass, SexualSelectionUnidirectional, Territory.2016)
cor.test(mastersdataagri$lgGenLength, mastersdataagri$lgBody.Mass) #Discard Generation Length from analysis
cor.test(mastersdata$Bill_Width, mastersdata$Gape_width, method=c("pearson"))#Discard Gape Width from analysis
cor.test(mastersdataagri$lgBill_Width, mastersdataagri$lgBody.Mass, method=c("pearson"))
cor.test(mastersdataagri$HWI, mastersdataagri$lgBody.Mass, method=c("pearson"))

str(mastersdata)
table(mastersdata)

# Read in dataset.
mastersdata<-read.csv("mastersdataanalysis.csv")

# Prepare dataset by labelling variables and classifying categorical variables as factors.
mastersdata$Primary_Foraging_strata_1 <- as.factor(mastersdata$Primary_Foraging_strata_1)

str(mastersdata)
mastersdata$Territory.2016 <- factor(mastersdata$Territory.2016,
                                     levels = c(0,1,2),
                                     labels = c("None", "Weak", "Strong"))

mastersdata$Habitat <- factor(mastersdata$Habitat,
                              levels = c(1,2,3),
                              labels = c("Dense", "Semi Open", "Open"))

mastersdata$SexualSelectionUnidirectional <- factor(mastersdata$SexualSelectionUnidirectional,
                                                    levels = c(0,1,2,3),
                                                    labels = c("Monogamy", "Predmnt monog.", " Frqnt polygyny", "Polygamy"))

mastersdata$Migration <- factor(mastersdata$Migration,
                                levels = c(1,2,3),
                                labels = c("Sedentary", "Partially Migratory", " Migratory"))

table(mastersdata$Threatened)
mastersdata$lgHWIndex <- log(mastersdata$HWIndex)
mastersdata$lgBill.Width <- log(mastersdata$Bill.Width)
mastersdata$lgGape_width <- log(mastersdata$Gape_width)
mastersdata$lgGenLength <- log(mastersdata$GenLength)
mastersdata$lgRange.Size <- log(mastersdata$Range.Size)
mastersdata$lgBody.Mass <- log(mastersdata$Body.Mass)
mastersdata$lgHWIndex <- log(mastersdata$HWIndex)

mastersdata$Threatened_Agri <- as.factor(mastersdata$Threatened_Agri)
mastersdata$Threatened_Hunting <- as.factor(mastersdata$Threatened_Hunting)
mastersdata$Threatened_Logging <- as.factor(mastersdata$Threatened_Logging)
mastersdata$Threatened_Human <- as.factor(mastersdata$Threatened_Human)
mastersdata$Migration <- as.factor(mastersdata$Migration)
mastersdata$Territory.2016 <- as.factor(mastersdata$Territory.2016)
mastersdata$Habitat <- as.factor(mastersdata$Habitat)
mastersdata$SexualSelectionUnidirectional <- as.factor(mastersdata$SexualSelectionUnidirectional)
mastersdata$ForagingNiche <- as.factor(mastersdata$ForagingNiche)
mastersdata$Threatened <- as.factor(mastersdata$Threatened)


# Drop missing species with missing values for variables used in analysis
mastersdata <- mastersdata %>% drop_na(Threatened_Agri, Habitat, HWIndex, Bill.Width, Migration, Habitat, Body.Mass, SexualSelectionUnidirectional, Territory.2016)


label(mastersdata$Body.Mass) <- "Body Mass"
label(mastersdata$lgBill.Width)<- "Log(Bill Width)"
label(mastersdata$HWIndex)<- "Hand Wing Index"
label(mastersdata$Migration)<- "Migratory"
label(mastersdata$Habitat)<- "Habitat"
label(mastersdata$Territory.2016) <- "Territoriality"
label(mastersdata$SexualSelectionUnidirectional)<- "Sexual Selection"
label(mastersdata$Range.Size)<- "Range Size"
label(mastersdata$Centroid_Lat)<- "Latitude"
label(mastersdata$Primary_Foraging_strata_1)<- "Foraging Strata"
label(mastersdata$lgBody.Mass) <- "Log(Body Mass)"
label(mastersdata$lgBill.Width)<- "Log(Bill Width)"
label(mastersdata$lgRange.Size)<- "Log(Range Size)"
label(mastersdata$Threatened_Agri)<- "Probability threatened by Agriculture"
label(mastersdata$Threatened_Human)<- "Probability threatened by Human development"
label(mastersdata$Threatened_Logging)<- "Probability threatened by Logging"
label(mastersdata$Threatened_Hunting)<- "Probability threatened by Hunting"


# Split the dataset by threat type including all non threatened species.
mastersdataagri <- mastersdata
mastersdatahunting <- mastersdata
mastersdatalogging <- mastersdata
mastersdatahuman <- mastersdata

mastersdataagri <- arrange(mastersdataagri, Birdlife_Name, desc(Threatened_Agri))
mastersdataagri <- mastersdataagri[!duplicated(mastersdataagri$Birdlife_Name),]

mastersdatahunting <- arrange(mastersdatahunting, Birdlife_Name, desc(Threatened_Hunting))
mastersdatahunting <- mastersdatahunting[!duplicated(mastersdatahunting$Birdlife_Name),]

mastersdatalogging <- arrange(mastersdatalogging, Birdlife_Name, desc(Threatened_Logging))
mastersdatalogging <- mastersdatalogging[!duplicated(mastersdatalogging$Birdlife_Name),]

mastersdatahuman <- arrange(mastersdatahuman, Birdlife_Name, desc(Threatened_Human))
mastersdatahuman <- mastersdatahuman[!duplicated(mastersdatahuman$Birdlife_Name),]

write.csv(mastersdata_hunting, "mastersthreathunting.csv")
write.csv(mastersdata_agri, "mastersthreatagriculture.csv")
write.csv(mastersdata_human, "mastersthreatlogging.csv")
write.csv(mastersdata_logging, "mastersdatathreathuman.csv")

# Create function to calculate probability change for coefficients.
inverse_logit = function(x){
  exp(x)/(1+exp(x))
}

# Agriculture models.
glmmagriculture_log <- glmer(Threatened_Agri ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                             + Habitat + Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                             +  (1 |Family), family = binomial(link = logit), data = mastersdataagri, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmagriculture_log)


# Check variance inflation factor of model.
vif(glmmagriculture_log)
anova(glmmagriculture_log, test='Chisq')

# Test estimated between each category level.
summary(glht(glmmagriculture_log, linfct = mcp(Habitat = "Tukey")), test = adjusted("holm"))

# Odds ratio estimates and r squared.
tab_model(glmmagriculture_log)

glmmagriculture_log4 <- glmer(Threatened_Agri ~ HWIndex +  (1 |Family), family = binomial(link = logit), data = mastersdataagri, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmagriculture_log4)
tab_model(glmmagriculture_log4)
inverse_logit(fixef(glmmagriculture_log4))


glmmagriculture_log5 <- glmer(Threatened_Agri ~ Migration*lgRange.Size
                              +  (1 |Family), family = binomial(link = logit), data = mastersdataagri, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

# Calculate probabilities and odds ratio estimates.
inverse_logit(fixef(glmmagriculture_log5))
summary(glmmagriculture_log5)
tab_model(glmmagriculture_log5)
t5<- inverse_logit(fixef(glmmagriculture_log5))
tab_model(t5)

# Stepwise selection to reduce full model based on AIC.
drop1(glmmagriculture_log)
glmmagriculture_log1 <- glmer(Threatened_Agri ~ lgBody.Mass + HWIndex + lgBill.Width + HWIndex + lgBill.Width
                              + Habitat + Territory.2016 + Migration + lgRange.Size 
                              + (1 |Family), family = binomial(link = logit), data = mastersdataagri, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmagriculture_log1)
tab_model(glmmagriculture_log1)
inverse_logit(fixef(glmmagriculture_log1))
drop1(glmmagriculture_log1)

# Compare log likelihood of reduced and full model.
anova(glmmagriculture_log, glmmagriculture_log1, test = 'Chisq')

# Calaculate Odds Ratios and 95% CIs.
t1 <- Confint(glmmagriculture_log, level=0.95, type="LR")

t4<- exp(t1)

# Calculate probability estimates.
t9<- inverse_logit(fixef(glmmagriculture_log))

# Plot predictor probabilities.
agthreat1 <- effect_plot(glmmagriculture_log, pred = lgBody.Mass, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Body Mass(g))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat8 <- interact_plot(model = glmmagriculture_log5, pred = lgRange.Size,
                           modx = Migration, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Range Size (km2))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25), legend.title = element_text(size =25), legend.text = element_text(size=25))

agthreat3 <- effect_plot(glmmagriculture_log, pred = lgBill.Width, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Bill Width(mm))", line.thickness = 1, colors = "lightsalmon1", point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat2 <- effect_plot(glmmagriculture_log, pred = HWIndex, interval = TRUE, partial.residuals = TRUE, x.label = "Hand Wing Index", colors = "skyblue3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat4 <- effect_plot(glmmagriculture_log, pred = SexualSelectionUnidirectional, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Sexual Selection", colors = "seagreen4", line.thickness = 1 , point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=25))
agthreat5 <- effect_plot(glmmagriculture_log, pred = Habitat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Habitat Preference", colors = "#e6550d", line.thickness = 1,point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat6 <- effect_plot(glmmagriculture_log, pred = Territory.2016, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Territoriality",colors = "paleturquoise3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat7 <- effect_plot(glmmagriculture_log, pred = Primary_Foraging_strata_1, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Foraging Strata", colors = "burlywood2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(25))
agthreat11 <- effect_plot(glmmagriculture_log, pred = Centroid_Lat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Centroid Latitude", colors = "olivedrab2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
agthreat10 <- effect_plot(glmmagriculture_log, pred = Migration, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Migration", colors = "purple2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=25))

agthreat9 <- ggarrange(agthreat3, agthreat2, agthreat4, agthreat6, agthreat7, agthreat5, agthreat10, agthreat11, ncol = 4, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", size=25), widths = 0.3, heights = 0.3)
annotate_figure(agthreat9,
                top = text_grob("Agriculture", size=25),
                left = text_grob("Probability", rot = 90, size=25))
agthreat12 <- ggarrange(agthreat1, agthreat8, ncol = 2, nrow = 1, labels = c("A", "B", size=25), widths = 0.3, heights = 0.3)
annotate_figure(agthreat12,
                top = text_grob("Agriculture", size = 25),
                left = text_grob("Probability", size = 25, rot = 90))


# Forest plots of odds ratio estimates.

set_theme(
  base = theme_bw(),
  title.size = 2
)        
t <- plot_model(glmmagriculture_log, title = "Agriculture", value.size = 7, colors = c("skyblue4"), axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4) + 
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed")  


scale_fill_manual("red","skyblue3","skyblue3","skyblue3","red","red","skyblue3","skyblue3","skyblue3","red","skyblue3","skyblue3","skyblue3","red","skyblue3", "red")

cor.test(mastersdata_agri$Bill_Width, mastersdata_agri$Gape_width, method=c("pearson"))

str(mastersdata_agri)


# Models: Hunting.

glmmhunting_log <- glmer(Threatened_Hunting ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                         + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                         +  (1 |Family), family = binomial(link = logit), data = mastersdatahunting, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhunting_log)
tab_model(glmmhunting_log)


glmmhunting_log6 <- glmer(Threatened_Hunting ~ HWIndex
                          +  (1 |Family), family = binomial(link = logit), data = mastersdatahunting, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhunting_log6)
tab_model(glmmhunting_log6)
inverse_logit(fixef(glmmhunting_log6))


glmmhunting_log7 <- glmer(Threatened_Hunting ~ Migration*lgRange.Size 
                          +  (1 |Family), family = binomial(link = logit), data = mastersdatahunting, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

inverse_logit(fixef(glmmhunting_log7))
summary(glmmhunting_log7)
tab_model(glmmhunting_log7)

# Stepwise selection to reduce full model based on AIC.
drop1(glmmhunting_log)

# Reduced Model
glmmhunting_log1 <- glmer(Threatened_Hunting ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                          +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                          +  (1 |Family), family = binomial(link = logit), data = mastersdatahunting, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhunting_log1)
tab_model(glmmhunting_log1)
inverse_logit(fixef(glmmhunting_log1))
drop1(glmmhunting_log1)

# Compare log likelihood of reduced and full model.
anova(glmmhunting_log1, glmmhunting_log, test='chisq')

# Calaculate Odds Ratios and 95% CIs.
l1 <- Confint(glmmhunting_log, level=0.95, type="LR")

l2 <- exp(l1)
tab_model(l2)

# Calculate probability estimates.
l3<- inverse_logit(fixef(glmmhunting_log))
tab_model(l3)


# Plot predictor probabilities.
huntthreat1 <- effect_plot(glmmhunting_log, pred = lgBody.Mass, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Body Mass(g))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
huntthreat3 <- effect_plot(glmmhunting_log, pred = lgBill.Width, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Bill Width(mm))", line.thickness = 1, colors = "lightsalmon1", point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
huntthreat8 <- interact_plot(model = glmmhunting_log7, pred = lgRange.Size,
                             modx = Migration, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Range Size (km2))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))  + theme(axis.text=element_text(size=16), axis.title=element_text(size=25), legend.title = element_text(size =25), legend.text = element_text(size=25))

huntthreat3 <- effect_plot(glmmhunting_log, pred = lgBill.Width, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Bill Width(mm))", line.thickness = 1, colors = "lightsalmon1", point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
huntthreat2 <- effect_plot(glmmhunting_log, pred = HWIndex, interval = TRUE, partial.residuals = TRUE, x.label = "Hand Wing Index", colors = "skyblue3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
huntthreat4 <- effect_plot(glmmhunting_log, pred = SexualSelectionUnidirectional, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Sexual Selection", colors = "seagreen4", line.thickness = 1 , point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=25))
huntthreat5 <- effect_plot(glmmhunting_log, pred = Habitat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Habitat Preference", colors = "#e6550d", line.thickness = 1,point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
huntthreat6 <- effect_plot(glmmhunting_log, pred = Territory.2016, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Territoriality",colors = "paleturquoise3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
huntthreat7 <- effect_plot(glmmhunting_log, pred = Primary_Foraging_strata_1, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Foraging Strata", colors = "burlywood2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16), axis.title=element_text(size=25))
huntthreat11 <- effect_plot(glmmhunting_log, pred = Centroid_Lat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Centroid Latitude", colors = "olivedrab2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
huntthreat10 <- effect_plot(glmmhunting_log, pred = Migration, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Migration", colors = "purple2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=12), axis.title=element_text(size=25))

huntthreat9 <- ggarrange(huntthreat2, huntthreat3, huntthreat4, huntthreat5, huntthreat6, huntthreat7, huntthreat10, huntthreat11, ncol = 4, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", size=25), widths = 0.3, heights = 0.3)
annotate_figure(huntthreat9,
                top = text_grob("Hunting", size=25),
                left = text_grob("Probability", size=25 ,rot = 90))

huntthreat12 <- ggarrange(huntthreat1, huntthreat8, ncol = 2, nrow = 1, labels = c("C", "D", size=25), widths = 0.3, heights = 0.3)
annotate_figure(huntthreat12,
                top = text_grob("Hunting", size=25),
                left = text_grob("Probability", size=25, rot = 90))

# Forest plots of odds ratio estimates.
set_theme(
  base = theme_bw(),
  title.size = 2
)     
g <- plot_model(glmmhunting_log, title = "Hunting", value.size = 7, colors = c("skyblue4"), axis.lim = c(.01, 100), axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)
g1 <- g + geom_hline(yintercept =1,linetype = "dashed") + font_size(labels.x = 28, labels.y = 28) +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


# Models: Logging.
glmmlogging_log <- glmer(Threatened_Logging ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional 
                         + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                         + (1 |Family), family = binomial(link = logit), data = mastersdatalogging, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(glmmlogging_log)

tab_model(glmmlogging_log)

drop1(glmmlogging_log)

glmmlogging_log3 <- glmer(Threatened_Logging ~ Migration*lgRange.Size 
                          +  (1 |Family), family = binomial(link = logit), data = mastersdatalogging, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmlogging_log3)
tab_model(glmmlogging_log3)
inverse_logit(fixef(glmmlogging_log3))



glmmlogging_log2 <- glmer(Threatened_Logging ~ HWIndex + (1 |Family), family = binomial(link = logit), data = mastersdatalogging, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(glmmlogging_log2)
tab_model(glmmlogging_log2)
inverse_logit(fixef(glmmlogging_log2))

# Stepwise selection to reduce full model based on AIC.
drop1(glmmlogging_log)

# Reduced model.
glmmlogging_log1 <- glmer(Threatened_Logging ~ lgBody.Mass + HWIndex + lgBill.Width 
                          + Habitat + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                          + (1 |Family), family = binomial(link = logit), data = mastersdatalogging, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(glmmlogging_log1)
tab_model(glmmlogging_log1)
inverse_logit(fixef(glmmlogging_log1))
drop1(glmmlogging_log1)

# Compare log likelihood of reduced and full model.
anova(glmmlogging_log, glmmlogging_log1, test='chisq')

# Calaculate Odds Ratios and 95% CIs.
s1 <- Confint(glmmlogging_log, level=0.95, type="LR")

s4<- exp(s1)


# Calculate probability estimates
s5 <- inverse_logit(fixef(glmmlogging_log))


# Plot predictor probabilities.
loggthreat1 <- effect_plot(glmmlogging_log, pred = lgBody.Mass, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Body Mass(g))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat8 <- interact_plot(model = glmmlogging_log3, pred = lgRange.Size,
                             modx = Migration, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Range Size (km2))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6), legend.title = element_text(size = 25), legend.text = element_text(size=25)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25))


loggthreat2 <- effect_plot(glmmlogging_log, pred = HWIndex, interval = TRUE, partial.residuals = TRUE, x.label = "Hand Wing Index", colors = "skyblue3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat3 <- effect_plot(glmmlogging_log, pred = lgBill.Width, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Bill Width(mm))", line.thickness = 1, colors = "lightsalmon1", point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat4 <- effect_plot(glmmlogging_log, pred = SexualSelectionUnidirectional, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Sexual Selection", colors = "seagreen4", line.thickness = 1 , point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=9),axis.title=element_text(size=25))
loggthreat5 <- effect_plot(glmmlogging_log, pred = Habitat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Habitat Preference", colors = "#e6550d", line.thickness = 1,point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat6 <- effect_plot(glmmlogging_log, pred = Territory.2016, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Territoriality",colors = "paleturquoise3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat7 <- effect_plot(glmmlogging_log, pred = Primary_Foraging_strata_1, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Foraging Strata",colors = "burlywood2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat11 <- effect_plot(glmmlogging_log, pred = Centroid_Lat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Centroid Latitude", colors = "olivedrab2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
loggthreat10 <- effect_plot(glmmlogging_log, pred = Migration, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Migration", colors = "purple2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=25))

loggthreat9 <- ggarrange(loggthreat2, loggthreat3, loggthreat4, loggthreat5, loggthreat6, loggthreat7, loggthreat10, loggthreat11, ncol = 4, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", size=25), widths = 0.3, heights = 0.3)
annotate_figure(loggthreat9,
                top = text_grob("Logging", size=25),
                left = text_grob("Probability", size=25, rot = 90))

loggthreat12 <- ggarrange(loggthreat1, loggthreat8, ncol = 2, nrow = 1, labels = c("E", "F", size=25), widths = 0.3, heights = 0.3)
annotate_figure(loggthreat12,
                top = text_grob("Logging", size=25),
                left = text_grob("Probability", rot = 90, size=25))

# Forest plots of odds ratio estimates.
set_theme(
  base = theme_bw(),
  title.size = 2
)     
h<- plot_model(glmmlogging_log, title = "Logging", value.size = 7, colors = c("skyblue4"), axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

h1 <- h + geom_hline(yintercept =1,linetype = "dashed") +font_size(labels.x = 28, labels.y = 28) +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 

# Models Human disturbance.
glmmhuman_log <- glmer(Threatened_Human ~ lgBody.Mass + HWIndex + lgBill.Width +SexualSelectionUnidirectional
                       + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                       +  Territory.2016 + Primary_Foraging_strata_1 + (1 |Family), family = binomial(link = logit), data = mastersdatahuman, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhuman_log)
tab_model(glmmhuman_log)
# Stepwise selection to reduce full model based on AIC.
drop1(glmmhuman_log)

# Reduced model.

glmmhuman_log1 <- glmer(Threatened_Human ~ lgBody.Mass + HWIndex + lgBill.Width
                        + Habitat +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                        +  (1 |Family), family = binomial(link = logit), data = mastersdatahuman, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhuman_log1)
tab_model(glmmhuman_log1)
inverse_logit(fixef(glmmhuman_log1))
drop1(glmmhuman_log1)

# Compare log likelihood of reduced and full model.
anova(glmmhuman_log, glmmhuman_log1, test='chisq')

glmmhuman_log3 <- glmer(Threatened_Human ~ Migration*lgRange.Size 
                        +  (1 |Family), family = binomial(link = logit), data = mastersdatahuman, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(glmmhuman_log3)
tab_model(glmmhuman_log3)
inverse_logit(fixef(glmmhuman_log3))


glmmhuman_log2 <- glmer(Threatened_Human ~ HWIndex + (1 |Family), family = binomial(link = logit), data = mastersdatahuman, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(glmmhuman_log2)
tab_model(glmmhuman_log2)
inverse_logit(fixef(glmmhuman_log2))


# Calaculate Odds Ratios and 95% CIs.
g1 <- Confint(glmmhuman_log, level=0.95, type="LR")

exp(g1)


# Calculate probability estimates
inverse_logit(fixef(glmmhuman_log))


# Plot predictor probabilities.
humthreat1 <- effect_plot(glmmhuman_log, pred = lgBody.Mass, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Body Mass(g))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat8 <- interact_plot(model = glmmhuman_log3, pred = lgRange.Size,
                            modx = Migration, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Range Size (km2))", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6)) + theme(axis.text=element_text(size=16),axis.title=element_text(size=25), legend.title = element_text(size = 25), legend.text = element_text(size=25))

humthreat2 <- effect_plot(glmmhuman_log, pred = HWIndex, interval = TRUE, partial.residuals = TRUE, x.label = "Hand Wing Index", colors = "skyblue3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat3 <- effect_plot(glmmhuman_log, pred = lgBill.Width, interval = TRUE, partial.residuals = TRUE, x.label = "Log(Bill Width(mm))", line.thickness = 1, colors = "lightsalmon1", point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat4 <- effect_plot(glmmhuman_log, pred = SexualSelectionUnidirectional, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Sexual Selection", colors = "seagreen4", line.thickness = 1 , point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=9),axis.title=element_text(size=25))
humthreat5 <- effect_plot(glmmhuman_log, pred = Habitat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Habitat Preference", colors = "#e6550d", line.thickness = 1,point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat6 <- effect_plot(glmmhuman_log, pred = Territory.2016, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Territoriality",colors = "paleturquoise3", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat7 <- effect_plot(glmmhuman_log, pred = Primary_Foraging_strata_1, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Foraging Strata", colors = "burlywood2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat11 <- effect_plot(glmmhuman_log, pred = Centroid_Lat, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Centroid Latitude", colors = "olivedrab2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=16),axis.title=element_text(size=25))
humthreat10 <- effect_plot(glmmhuman_log, pred = Migration, interval = TRUE, partial.residuals = TRUE, cat.geom = "line", x.label = "Migration", colors = "purple2", line.thickness = 1, point.size = 0.5, rug = TRUE,  y.label = "") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.6))+ theme(axis.text=element_text(size=12),axis.title=element_text(size=25))
humthreat9 <- ggarrange(humthreat2, humthreat3, humthreat4, humthreat10, humthreat5, humthreat6, humthreat7, humthreat11, ncol = 4, nrow = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", size=25), widths = 0.3, heights = 0.3)
annotate_figure(humthreat9,
                top = text_grob("Human development", size=25),
                left = text_grob("Probability", rot = 90, size=25))

humthreat12 <- ggarrange(humthreat1, humthreat8, ncol = 2, nrow = 1, labels = c("G", "H", size=25), widths = 0.3, heights = 0.3)
annotate_figure(humthreat12,
                top = text_grob("Human development", size=25),
                left = text_grob("Probability", rot = 90, size=25))

# Forest plots of odds ratio estimates.
set_theme(
  base = theme_bw(),
  title.size = 2
)     
u <- plot_model(glmmhuman_log, title = "Human development", value.size = 7, colors = c("skyblue4"),  axis.lim = c(.001, 100), axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

u1 <- u + geom_hline(yintercept =1,linetype = "dashed") + font_size(labels.x =15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 

# Place all Odds ratios estimates for threatened models in a grid.

threatodds <- ggarrange(t, g1, h1, u1, ncol = 4, nrow = 1, labels = c("A", "B", "C", "D"), widths = 0.3, heights = 0.3)
annotate_figure(threatodds,
                top = text_grob("Odds Ratio estimates of incidence of threat"))

# Red list models: agriculture.
mastersdata<- read.csv("mastersdatarlall.csv")

mastersdata$Primary_Foraging_strata_1 <- as.factor(mastersdata$Primary_Foraging_strata_1)

str(mastersdata)

mastersdata$lgBill.Width <- log(mastersdata$Bill.Width)
mastersdata$lgRange.Size <- log(mastersdata$Range.Size)
mastersdata$lgBody.Mass <- log(mastersdata$Body.Mass)

label(mastersdata$Body.Mass) <- "Body Mass"
label(mastersdata$HWIndex)<- "Hand Wing Index"
label(mastersdata$Migration)<- "Migratory"
label(mastersdata$Habitat)<- "Habitat"
label(mastersdata$Territory.2016) <- "Territoriality"
label(mastersdata$SexualSelectionUnidirectional)<- "Sexual Selection"
label(mastersdata$Range.Size)<- "Range Size"
label(mastersdata$Centroid_Lat)<- "Latitude"
label(mastersdata$Primary_Foraging_strata_1)<- "Foraging Strata"
label(mastersdata$lgBody.Mass) <- "Log(Body Mass)"
label(mastersdata$lgBill.Width)<- "Log(Bill Width)"
label(mastersdata$lgRange.Size)<- "Log(Range Size)"

str(mastersdata)
mastersdata$Territory.2016 <- factor(mastersdata$Territory.2016,
                                     levels = c(0,1,2),
                                     labels = c("None", "Weak", "Strong"))

mastersdata$Habitat <- factor(mastersdata$Habitat,
                              levels = c(0,1,2),
                              labels = c("Dense", "Semi Open", "Open"))


mastersdata$SexualSelectionUnidirectional <- factor(mastersdata$SexualSelectionUnidirectional,
                                                    levels = c(0,1,2,3),
                                                    labels = c("Monogamy", "Predmnt monogamy", " Frequent polygyny", "Polygamy"))

mastersdata$Migration <- factor(mastersdata$Migration,
                                levels = c(0,1,2),
                                labels = c("Sedentary", "Partially Migratory", " Migratory"))


mastersdata$RL.Agri <- as.factor(mastersdata$RL.Agri)
mastersdata <- mastersdata %>% drop_na(RL.Agri)
mastersdata <- mastersdata %>% drop_na(RL_Hunting, RL_Logging, RL_Human, Habitat, HWIndex, Bill.Width, SexualSelectionUnidirectional, Territory.2016)

mastersdata$SexualSelectionUnidirectional <- as.factor(mastersdata$SexualSelectionUnidirectional)
mastersdata$RL.Agri <- as.factor(mastersdata$RL.Agri)
mastersdata$RL_Human <- as.factor(mastersdata$RL_Human)
mastersdata$RL_Logging<- as.factor(mastersdata$RL_Logging)
mastersdata$RL_Hunting <- as.factor(mastersdata$RL_Hunting)
mastersdata$Migration <- as.factor(mastersdata$Migration)
mastersdata$Habitat <- as.factor(mastersdata$Habitat)
mastersdata$Territory.2016 <- as.factor(mastersdata$Territory.2016)

str(mastersdata)


# Split models including only species threatened by threat types.

mastersdataglmm <- split(mastersdata, mastersdata$Threat.Type)

mastersdatahuntinglc <- mastersdataglmm$`Hunting & trapping terrestrial animals`
mastersdataagriculturelc <- mastersdataglmm$`Agriculture & aquaculture`
mastersdatahumanlc <- mastersdataglmm$`Human development & disturbance`
mastersdatalogginglc <- mastersdataglmm$`Logging & wood harvesting`


# Determine number of threatened species
length(unique(mastersdata$Family))
n_distinct(mastersdata$Family)

table(mastersdataagriculturelc$RL.Agri)
table(mastersdatahuntinglc$RL_Hunting)
table(mastersdatalogginglc$RL_Logging)
table(mastersdatahumanlc$RL_Human)


# Agriculture models
lmmagriculture2 <- clmm(RL.Agri ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                        + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                        + (1 | Family), data = mastersdataagriculturelc)

summary(lmmagriculture2)
tab_model(lmmagriculture2)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmagriculture2)

# Reduced model.
lmmagriculture3 <- clmm(RL.Agri ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                        + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                        + (1 | Family), data = mastersdataagriculturelc)

summary(lmmagriculture3)
tab_model(lmmagriculture3)
inverse_logit(fixef(lmmagriculture3))
drop1(lmmagriculture3)

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmagriculture3, lmmagriculture2, type = "II")


# Calaculate Odds Ratios and 95% CIs
d1 <- Confint(lmmagriculture2, level=0.95, type="LR")

exp(d1)

# Calculate probability estimates
inverse_logit(coef(lmmagriculture2))

# Forest plots of odds ratio estimates
set_theme(
  base = theme_bw(),
  title.size = 2
)     
r <- plot_model(lmmagriculture2, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3", "3|4"),  title = "Agriculture", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

r1 <- r + geom_hline(yintercept =1,linetype = "dashed") + font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


drop1(lmmagriculture1, test="Chisq")

anova(lmmagriculture1)


# Red list models: hunting
lmmhunting2 <- clmm(RL_Hunting ~ lgBody.Mass+ HWIndex + lgBill.Width + SexualSelectionUnidirectional
                    + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size +(1|Family), Hess= TRUE, data=mastersdatahuntinglc)
summary(lmmhunting2)
tab_model(lmmhunting2)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmhunting2)

# Reduced model.
lmmhunting3 <- clmm(RL_Hunting ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                    + Habitat +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                    + (1|Family), data=mastersdatahuntinglc)
summary(lmmhunting3)
tab_model(lmmhunting3)
inverse_logit(fixef(lmmhunting3))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmhunting3, lmmhunting2, type = "II")


drop1(lmmhunting3)

# Calaculate Odds Ratios and 95% CIs
e1 <- Confint(lmmhunting2, level=0.95, type="LR")

exp(e1)


# Forest plots of odds ratio estimates
set_theme(
  base = theme_bw(),
  title.size = 2
)     
k <- plot_model(lmmhunting2, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3", "3|4"),  title = "Hunting", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

k1 <- k + geom_hline(yintercept =1,linetype = "dashed") +font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


# Red list models: logging
lmmlogging2<- clmm(RL_Logging ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                   + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                   +  (1 | Family), data = mastersdatalogginglc)

summary(lmmlogging2)
tab_model(lmmlogging2)

drop1(lmmlogging2)

lmmlogging3<- clmm(RL_Logging ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                   + Habitat  + Migration + Centroid_Lat + lgRange.Size*Migration
                   +  (1 | Family), data = mastersdatalogginglc)

summary(lmmlogging3)
tab_model(lmmlogging3)
inverse_logit(fixef(lmmlogging3))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmlogging3, lmmlogging2, type = "II")


# Calaculate Odds Ratios and 95% CIs
t1 <- Confint(lmmlogging2, level=0.95, type="LR")

exp(t1)


# Forest plots of odds ratio estimates
set_theme(
  base = theme_bw(),
  title.size = 2
)     
o<- plot_model(lmmlogging2, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3", "3|4"),  title = "Logging", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

o1 <- o + geom_hline(yintercept =1,linetype = "dashed") + font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


# Red list models: Human disturbance
lmmhuman <- clmm(RL_Human ~ log(Body.Mass) + HWIndex + log(Bill.Width) + SexualSelectionUnidirectional
                 + Habitat +	Territory.2016  + Migration + Primary_Foraging_strata_1 + Centroid_Lat + log(Range.Size)
                 + (1|Family), data = mastersdatahumanlc)
summary(lmmhuman)
tab_model(lmmhuman)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmhuman)

# Reduced model.
lmmhuman1 <- clmm(RL_Human ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                  + Habitat + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                  + (1|Family), data = mastersdatahumanlc)

summary(lmmhuman1)
tab_model(lmmhuman1)
inverse_logit(fixef(lmmhuman1))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmhuman1, lmmhuman, type = "II")

# Calaculate Odds Ratios and 95% CIs
t8 <- Confint(lmmhuman, level=0.95, type="LR")

exp(t8)

# Calculate probability estimates
inverse_logit(coef(lmmhuman))


# Forest plots of odds ratio estimates
set_theme(
  base = theme_bw(),
  title.size = 2
)     
i<- plot_model(lmmhuman, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3", "3|4"), title = "Human Development", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

i1 <- i + geom_hline(yintercept =1,linetype = "dashed") +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 

# Place all Odds ratios estimates for IUCN RED List models in a grid

IUCNodds <- ggarrange(r1, k1, o1 , i1, ncol = 4, nrow = 1, labels = c("A", "B", "C", "D"), widths = 0.3, heights = 0.3)
annotate_figure(IUCNodds,
                top = text_grob("Odds Ratio estimates of IUCN red list category"))


lrtest(lmmhuman, lmmhuman2)
confint(lmmhuman1)

# Severity of threat models: Agriculture
mastersdata_severity <- read.csv("mastersdatarlall.csv")




mastersdata_severity$Territory.2016 <- factor(mastersdata_severity$Territory.2016,
                                              levels = c(0,1,2),
                                              labels = c("None", "Weak", "Strong"))

mastersdata_severity$Habitat <- factor(mastersdata_severity$Habitat,
                                       levels = c(0,1,2),
                                       labels = c("Dense", "Semi Open", "Open"))


mastersdata_severity$SexualSelectionUnidirectional <- factor(mastersdata_severity$SexualSelectionUnidirectional,
                                                             levels = c(0,1,2,3),
                                                             labels = c("Monogamy", "Predmnt monogamy", " Frequent polygyny", "Polygamy"))

mastersdata_severity$Migration <- factor(mastersdata_severity$Migration,
                                         levels = c(0,1,2),
                                         labels = c("Sedentary", "Partially Migratory", " Migratory"))

str(mastersdata_severity)

# Split species threatened by threat type, include all non-threatened species as comparisons in analysis for each threat type.
mastersdata_severity$Severity_Agri <- as.factor(mastersdata_severity$Severity_Agri)
mastersdata_severity$Severity_Hunting <- as.factor(mastersdata_severity$Severity_Hunting)
mastersdata_severity$Severity_Logging <- as.factor(mastersdata_severity$Severity_Logging)
mastersdata_severity$Severity_Human <- as.factor(mastersdata_severity$Severity_Human)
mastersdata_severity$Migration <- as.factor(mastersdata_severity$Migration)
mastersdata_severity$Habitat <- as.factor(mastersdata_severity$Habitat)

mastersdata_severity$EDGE_SCORE <- as.factor(mastersdata_severity$EDGE_SCORE)
mastersdata_severity$SexualSelectionUnidirectional <- as.factor(mastersdata_severity$SexualSelectionUnidirectional)
mastersdata_severity$Territory.2016 <- as.factor(mastersdata_severity$Territory.2016)

mastersdata_severity <- mastersdata_severity %>% drop_na(Severity_Agri, Severity_Human, Severity_Logging, Severity_Hunting, Habitat, HWIndex, Bill.Width, SexualSelectionUnidirectional)
str(mastersdata_severity)

mastersdata_severity$lgBill.Width <- log(mastersdata_severity$Bill.Width)
mastersdata_severity$lgGape_width <- log(mastersdata_severity$Gape_width)
mastersdata_severity$lgGenLength <- log(mastersdata_severity$GenLength)
mastersdata_severity$lgRange.Size <- log(mastersdata_severity$Range.Size)
mastersdata_severity$lgBody.Mass <- log(mastersdata_severity$Body.Mass)
mastersdata_severity$zHWIndex <- scale(mastersdata_severity$HWIndex)

label(mastersdata_severity$Body.Mass) <- "Body Mass"
label(mastersdata_severity$lgBill.Width)<- "Log(Bill Width)"
label(mastersdata_severity$HWIndex)<- "Hand Wing Index"
label(mastersdata_severity$Migration)<- "Migratory"
label(mastersdata_severity$Habitat)<- "Habitat"
label(mastersdata_severity$Territory.2016) <- "Territoriality"
label(mastersdata_severity$SexualSelectionUnidirectional)<- "Sexual Selection"
label(mastersdata_severity$Range.Size)<- "Range Size"
label(mastersdata_severity$Centroid_Lat)<- "Latitude"
label(mastersdata_severity$Primary_Foraging_strata_1)<- "Foraging Strata"
label(mastersdata_severity$lgBody.Mass) <- "Log(Body Mass)"
label(mastersdata_severity$lgBill.Width)<- "Log(Bill Width)"
label(mastersdata_severity$lgRange.Size)<- "Log(Range Size)"


mastersdatasevagri <- mastersdata_severity 
mastersdatasevhunting <- mastersdata_severity
mastersdatasevlogging <- mastersdata_severity
mastersdatasevhuman <- mastersdata_severity   

mastersdatasevagri <- mastersdatasevagri %>% arrange(Birdlife_Name, desc(Severity_Agri))
mastersdatasevagri <- mastersdatasevagri[!duplicated(mastersdatasevagri$Birdlife_Name),]

mastersdatasevhunting <- mastersdatasevhunting %>% arrange(Birdlife_Name,desc(Severity_Hunting))
mastersdatasevhunting <- mastersdatasevhunting[!duplicated(mastersdatasevhunting$Birdlife_Name),]

mastersdatasevlogging <- mastersdatasevlogging %>% arrange(Birdlife_Name, desc(Severity_Logging))
mastersdatasevlogging <- mastersdatasevlogging[!duplicated(mastersdatasevlogging$Birdlife_Name),]

mastersdatasevhuman <- mastersdatasevhuman %>% arrange(Birdlife_Name, desc(Severity_Human))
mastersdatasevhuman <- mastersdatasevhuman[!duplicated(mastersdatasevhuman$Birdlife_Name),]

# Determine number of species by severity.
with(mastersdatasevagri, table(Severity))
with(mastersdatasevhunting, table(Severity))
with(mastersdatasevlogging, table(Severity))
with(mastersdatasevhuman, table(Severity))


# Threat severity models: Agriculture.
lmmsevagri1 <- clmm(Severity_Agri ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                    + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                    + (1|Family), data=mastersdatasevagri)

summary(lmmsevagri1)
tab_model(lmmsevagri1)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmsevagri1)

# Reduced model.
lmmsevagri2 <- clmm(Severity_Agri ~ lgBody.Mass + HWIndex + lgBill.Width
                    + Habitat +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                    + (1|Family), data=mastersdatasevagri)
summary(lmmsevagri2)
tab_model(lmmsevagri2)
inverse_logit(fixef(lmmsevagri2))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmsevagri2, lmmsevagri1, type = "II")

# Calculate Odds Ratios and 95% CIs
v1 <- Confint(lmmsevagri1, level=0.95, type="LR")

exp(v1)

# Calculate probability estimates
inverse_logit(coef(lmmsevagri1))

# Forest plots of odds ratio estimates
set_theme(
  base = theme_bw(),
  title.size = 2
)     
v<- plot_model(lmmsevagri1, colors = c("skyblue4"),  value.size = 7, rm.terms = c("0|1", "1|2", "2|3"),  title = "Agriculture", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

v1<- v + geom_hline(yintercept =1,linetype = "dashed") +font_size(labels.x = 28, labels.y = 28) +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


# Threat severity models: Hunting

lmmsevhunting2 <- clmm(Severity_Hunting ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                       + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                       +  (1|Family), data=mastersdatasevhunting)


summary(lmmsevhunting2)
tab_model(lmmsevhunting2)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmsevhunting2)

# Reduced model.
lmmsevhunting3 <- clmm(Severity_Hunting ~ lgBody.Mass + HWIndex + lgBill.Width + Habitat
                       +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                       +  (1|Family), 
                       data=mastersdatasevhunting)

summary(lmmsevhunting3)
tab_model(lmmsevhunting3)
inverse_logit(fixef(lmmsevhunting3))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmsevhunting3, lmmsevhunting2, type = "II")

# Calaculate Odds Ratios and 95% CIs
q1 <- Confint(lmmsevhunting2, level=0.95, type="LR")

exp(q1)

# Calculate probability estimates
inverse_logit(coef(lmmsevhunting2))


# Plot forest plots of odds ratios
set_theme(
  base = theme_bw(),
  title.size = 2
)     
a<- plot_model(lmmsevhunting2, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3"),  title = "Hunting", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

a1 <- a + geom_hline(yintercept =1,linetype = "dashed") +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 


# Threat severity models: Logging

lmmsevlogging1 <- clmm(Severity_Logging ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                       + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                       + (1|Family), data=mastersdatasevlogging)

summary(lmmsevlogging1)
tab_model(lmmsevlogging1)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmsevlogging1)

# Reduced model.
lmmsevlogging2 <- clmm(Severity_Logging ~ lgBody.Mass + HWIndex + lgBill.Width
                       + Habitat +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                       + (1|Family), data=mastersdatasevlogging)

summary(lmmsevlogging2)
tab_model(lmmsevlogging2)
inverse_logit(fixef(lmmsevlogging2))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmsevlogging2, lmmsevlogging1, type = "II")

# Calaculate Odds Ratios and 95% CIs
r1 <- Confint(lmmsevlogging1, level=0.95, type="LR")

exp(r1)

# Calculate probability estimates
inverse_logit(coef(lmmsevlogging1))


# Plot forest plots of odds ratios
set_theme(
  base = theme_bw(),
  title.size = 2
)     
s<- plot_model(lmmsevlogging1, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3"),  title = "Logging", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

s1<- s + geom_hline(yintercept =1,linetype = "dashed") +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 

# Threat severity models: Human Disturbance

lmmsevhuman1 <- clmm(Severity_Human ~ lgBody.Mass + HWIndex + lgBill.Width + SexualSelectionUnidirectional
                     + Habitat +	Territory.2016 + Migration + Primary_Foraging_strata_1 + Centroid_Lat + lgRange.Size
                     + (1|Family), data=mastersdatasevhuman)

summary(lmmsevhuman1)
tab_model(lmmsevhuman1)

# Stepwise selection to reduce full model based on AIC.
drop1(lmmsevhuman1)

# Reduced model.
lmmsevhuman2 <- clmm(Severity_Human ~ lgBody.Mass + HWIndex + lgBill.Width 
                     + Habitat +	Territory.2016 + Migration + Centroid_Lat + lgRange.Size
                     +  (1|Family), data=mastersdatasevhuman)

summary(lmmsevhuman2)
tab_model(lmmsevhuman2)
inverse_logit(fixef(lmmsevhuman2))

# Compare log likelihood of reduced and full model.
Anova.clmm(lmmsevhuman2, lmmsevhuman1, type = "II")

# Calaculate Odds Ratios and 95% CIs
y1 <- Confint(lmmsevhuman1, level=0.95, type="LR")

exp(y1)

# Calculate probability estimates
inverse_logit(coef(lmmsevhuman1))

# Plot forest plots of odds ratios
set_theme(
  base = theme_bw(),
  title.size = 2
)     

n<- plot_model(lmmsevhuman1, colors = c("skyblue4"), value.size = 7, rm.terms = c("0|1", "1|2", "2|3"), title = "Human development", axis.labels = c("Log(Range Size)","Latitude", "Foraging Strata: Understory","Foraging Strata: Ground", "Migratory", "Partially Migratory", "Territoriality: Strong", "Territoriality: Weak","Habitat: Open", "Habitat: Semi Open", "Sexual Selection: Polygamy", "Sexual Selection: Frequent Polygyny", "Sexual Selection: Predominant Monogamy",  "Log(Bill Width)", "Hand Wing Index", "Log(Body Mass)"), show.p = TRUE, show.values = TRUE, value.offset = .4)

n1<- n + geom_hline(yintercept =1,linetype = "dashed") +
  font_size(labels.x = 15, labels.y = 21, axis_title.x = 21) + geom_hline(yintercept =1,linetype = "dashed") 

# Place all Odds ratios estimates for severity models in a grid

severityodds <- ggarrange(v1, a1, s1 , n1, ncol = 4, nrow = 1, labels = c("A", "B", "C", "D"), widths = 0.3, heights = 0.3)
annotate_figure(severityodds,
                top = text_grob("Odds Ratio estimates of threat severity"))