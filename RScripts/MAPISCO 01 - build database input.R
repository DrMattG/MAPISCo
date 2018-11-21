### MAPISCO 01 - build database input
### READS AND RE-WORKS ORIGINAL DATA FILES, BUILDS A 'MASTER' SPECIES
###  LIST WITH UNIQUE SPID NUMBERS, MATCHES UNIQUE SPID NUMBERS TO NEW
###  TABLES AND SAVES THE RE-WORKED TABLES TO DISK, READING FOR READING
###  INTO DATABASE.

library(gdata)
library(plyr)

setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/')


#################################################################################### 
### SECTION 1 - LOAD AND REWORK ORIGINAL DATA FILES:
####################################################################################

### RED LIST THREAT STATUS
  # Load:
redlist12 <- read.csv('000_INPUT/FULL REDLIST 2012-1 AS DOWNLOADED.csv', header=T)
  # Build binomial:
redlist12$binomial <- paste(redlist12$Genus, redlist12$Species)
  # Pick out first listed english names:  
redlist12$english <- as.vector(redlist12$Common.names..Eng.)
  # Function to pick out first elements from list:
pickfirst <- function(x) { return(x[1]) }
  # Split the "English name" string by commas, trim whitespaces from result,
  #  pick out the first name listed only, unlist result and save as new column:
redlist12$name <- unlist(lapply(lapply(strsplit(redlist12$english,","),trim),pickfirst))
  # Select subset STATUS data:
rl_status <- subset(redlist12, select=c("Species.ID",
                                        "Red.List.status",
                                        "Red.List.criteria",
                                        "Red.List.criteria.version",
                                        "Year.assessed",
                                        "Population.trend",
                                        "binomial"))
names(rl_status) <- c("rlid","status","crit","ver","year","poptrend", "binomial")
rl_status$status <- as.factor(rl_status$status)
rl_status$crit <- as.vector(rl_status$crit)
rl_status$ver <- as.character(rl_status$ver)
rl_status$year <- as.factor(rl_status$year)
rl_status$poptrend <- as.factor(rl_status$poptrend)
rl_status$binomial <- as.vector(rl_status$binomial)
  # Select and subset taxonomic data and english names:
rl_tax <- subset(redlist12, select=c("Species.ID",
                                     "Kingdom",
                                     "Class",
                                     "name",
                                     "binomial"))
names(rl_tax) <- c("rlid","kingdom","class","vernacular","binomial")
rm(redlist12)

### Monocots & Legumes SRLI data:
temp1 <- read.csv('000_INPUT/Original/Monocots_Legumes/Monocots RED LISTING BY SPECIES.csv', header=T)
temp2 <- read.csv('000_INPUT/Original/Monocots_Legumes/Legumes RED LISTING BY SPECIES.csv', header=T)
srli_status <- rbind(temp1, temp2)
rm(temp1, temp2)
srli_status$binomial <- paste(srli_status$Genus, srli_status$Species)


### AQUACULTURE DATA - Species binomials occur more than once (why?).
  # Load:
aculture <- read.csv('000_INPUT/Aquaculture use.csv', header=T)
aculture_classes <- levels(aculture$Class)
  # Build binomial:
aculture$binomial <- paste(aculture$Genus, aculture$Species)
  # Add 'flag' (0/1, occurs in list or not):
aculture$aculture <- 1
names(aculture) <- c("order","class","family",
                     "genus","species","speccode",
                     "usedforaquaculture","binomial","aculture")
  # Select subset and take out duplicate rows:
aculture <- subset(aculture, select=c("binomial","aculture"))
aculture <- unique(aculture)


### AQUACULTURE DATA - REDLIST
  # Load:
rl_ac_2.4.1 <- read.csv('000_INPUT/RL_THREAT_2.4.1_APR2012.csv', header=T)
rl_ac_2.4.2 <- read.csv('000_INPUT/RL_THREAT_2.4.2_APR2012.csv', header=T)
rl_ac_2.4.3 <- read.csv('000_INPUT/RL_THREAT_2.4.3_APR2012.csv', header=T)
rl_ac_2.4.1$aculture_rl <- "Subsistence/artisinal"
rl_ac_2.4.2$aculture_rl <- "Industrial"
rl_ac_2.4.3$aculture_rl <- "Scale unknown"
rl_ac <- rbind(rl_ac_2.4.1, rl_ac_2.4.2, rl_ac_2.4.3)
rm(rl_ac_2.4.1, rl_ac_2.4.2, rl_ac_2.4.3)
  # Build binomial:
rl_ac$binomial <- paste(rl_ac$Genus, rl_ac$Species)
  # Subset:
rl_ac <- subset(rl_ac, select=c("binomial","aculture_rl"))


### CWR DATA - provided by Nigel Maxted (IUCN CWR SG). Not for distribution
###  pending publication of this database.
  # Load:
cwr <- read.csv('000_INPUT/CWR Database Ver 2 as list.csv', header=T)
  # Build binomial:
cwr$binomial <- as.vector(cwr$sp)
  # Remove duplicate row (IE IGNORE SUBSPP. AND/OR VARIETIES!) and subset:
cwr <- subset(cwr, select=c("binomial"))
cwr$cwr <- 1
cwr <- unique(cwr)


### BGCI "Medicinal" plant species data
### (http://www.bgci.org/plant_search.php?action=Find&lang=eng&ftrGenus=
###  &ftrRedList=&ftrSpecies=&ftrRedList1997=&ftrEpithet=&ftrCWR=
###  &ftrMedicinal=on&x=55&y=8#results)
  # Load:
bgci_medicinal <- read.csv('000_INPUT/Medicinal plants_BGCI-PlantSearch_APR2012.csv',header=T)
 
### BGCI Hawkins 2008 Plants for Life - Annex 5
bgci_pfl <- read.csv('000_INPUT/BGCI_Hawkins2008Annex5-MedicinalPlantPriorities.csv',header=T)
  # Load:

  # Build binomial:
bgci_medicinal$binomial <- paste(bgci_medicinal$Genus, bgci_medicinal$Species)
bgci_pfl$binomial <- paste(bgci_pfl$genus, bgci_pfl$species)
## Find those where a whole genus has a higher priority in Hawkins 2008:
bgci_pfl_genusprior <- bgci_pfl[bgci_pfl$species=="spp.",]
  ## Find all species within such genuses in PlantSearch data, and match:
bgci_medicinal$p1 <- 
  bgci_pfl_genusprior$priority[match(bgci_medicinal$Genus, 
                                     bgci_pfl_genusprior$genus)]
  ## Now match all remaining unique species:
bgci_medicinal$p2 <-
  bgci_pfl$priority[match(bgci_medicinal$binomial, bgci_pfl$binomial)]
  ## Combine the two into single priority:  
bgci_medicinal$p1[is.na(bgci_medicinal$p1)] <- 1
bgci_medicinal$p2[is.na(bgci_medicinal$p2)] <- 1
bgci_medicinal$bgci_med_priority <- 
  apply(cbind(bgci_medicinal$p1,bgci_medicinal$p2),1,max)
  # Subset:
bgci <- subset(bgci_medicinal, select=c("binomial",
                                        "bgci_med_priority"))
rm(bgci_medicinal, bgci_pfl, bgci_pfl_genusprior)


### FISHBASE "COMMERCIAL" SPECIES:
  # Load:
fb <- read.csv('000_INPUT/FishBase-AllCommercial-Apr2012.csv')
  # Subset and remove duplicates (species occuring in multiple regions)

# fb$Country[fb$Country=="Admiralty Islands"] <- "Papua New Guinea"
# fb$Country[fb$Country=="Alaska (USA)"] <- "United States"
# fb$Country[fb$Country=="Andaman Island"] <- "India"
# fb$Country[fb$Country=="Antarctic"] <- "Antarctica"
# fb$Country[fb$Country=="Ascension Island"] <- "Saint Helena, Ascension and Tristan da Cunha"
# fb$Country[fb$Country=="Bolivia"] <- "Bolivia, Plurinational State of"
# fb$Country[fb$Country=="Canary Islands"] <- "Spain"
# fb$Country[fb$Country=="Chagos Islands"] <- "British Indian Ocean Territory"
# fb$Country[fb$Country=="Cocos I. Costa Rica"] <- "Costa Rica"
# fb$Country[fb$Country=="Congo, Dem. Rep. of the"] <- "Congo, The Democratic Republic of the"
# fb$Country[fb$Country=="Congo, Republic of"] <- "Congo"
# fb$Country[fb$Country=="Curaçao Island"] <- "Netherlands Antilles"
# fb$Country[fb$Country=="Côte d'Ivoire"] <- "Cote d'Ivoire"
# fb$Country[fb$Country=="Faeroe Islands"] <- "Denmark"
# fb$Country[fb$Country=="Falkland Is. (Malvinas)"] <- "Falkland Islands (Malvinas)"
# fb$Country[fb$Country=="Fiji Islands"] <- "Fiji"
# fb$Country[fb$Country=="Galapagos Islands"] <- "Ecuador"
# fb$Country[fb$Country=="Germany, Fed. Rep."] <- "Germany"
# fb$Country[fb$Country=="Hawaii (USA)"] <- "United States"
# fb$Country[fb$Country=="Iran (Islamic Rep. of)"] <- "Iran, Islamic Republic of"
# fb$Country[fb$Country=="Kerguelen Islands"] <- "French Southern Territories"
# fb$Country[fb$Country=="Korea, Dem. People's Rep"] <- "Korea, Democratic People's Republic of"
# fb$Country[fb$Country=="Lao People's Dem. Rep."] <- "Lao People's Democratic Republic"
# fb$Country[fb$Country=="Libyan Arab Jamahiriya"] <- "Libya"
# fb$Country[fb$Country=="Macau"] <- "China"
# fb$Country[fb$Country=="Madeira Islands"] <- "Portugal"
# fb$Country[fb$Country=="Marquesas Islands"] <- "French Polynesia"
# fb$Country[fb$Country=="Micronesia,Fed.States of"] <- "Micronesia, Federated States of"
# fb$Country[fb$Country=="Moldova, Republic of"] <- "Moldova"
# fb$Country[fb$Country=="North Marianas"] <- "United States"
# fb$Country[fb$Country=="Ryukyu Islands"] <- "Japan"
# fb$Country[fb$Country=="Réunion"] <- "Reunion"
# fb$Country[fb$Country=="Saint Helena"] <- "Saint Helena, Ascension and Tristan da Cunha"
# fb$Country[fb$Country=="Scotland (UK)"] <- "United Kingdom"
# fb$Country[fb$Country=="Tahiti"] <- "French Polynesia"
# fb$Country[fb$Country=="Taiwan"] <- "Taiwan, Province of China"
# fb$Country[fb$Country=="Tanzania, United Rep. of"] <- "Tanzania, United Republic of"
# fb$Country[fb$Country=="Tuamotu Islands"] <- "French Polynesia"
# fb$Country[fb$Country=="Turkmenistán"] <- "Turkmenistan"
# fb$Country[fb$Country=="US Virgin Islands"] <- "Virgin Islands, U.S."
# fb$Country[fb$Country=="USA (contiguous states)"] <- "United States"
# fb$Country[fb$Country=="Venezuela"] <- "Venezuela, Bolivarian Republic of"
# fb$Country[fb$Country=="Virgin Islands (UK)"] <- "Virgin Islands, British"
# fb$Country[fb$Country=="Zimbabwe "] <- "Zimbabwe"

fb_elsewhere <- subset(fb, select=c("Species","Use.elsewhere"))
fb_elsewhere <- unique(fb_elsewhere)
fb_elsewhere$Species <- as.vector(fb_elsewhere$Species)
names(fb_elsewhere) <- c("binomial","fb_comm_overall")
# fb_country <- subset(fb, select=c("Species","Use","Country"))
# fb_country <- fb_country[order(fb_country$Species),]
# names(fb_country) <- c("binomial","use","country")
rm(fb)

### AZE SITE-SPECIES DATA:
  # Load:
aze <- read.csv('000_INPUT/AZE_Global_20111014_mnf.csv', header=T)
  # Build binomial:
aze$binomial <- as.vector(aze$Scientific)
  # Find the number of species per AZE site: 
temp <- data.frame(site=trim(unlist(strsplit(as.vector(aze$MapID),","))),
                   species=rep(aze$Scientific,
                               lapply(strsplit(as.vector(aze$MapID),","),length))
                   )
aze_sites <- ddply(temp, .(site), "nrow")
  # Per AZE species, find the number of co-occuring species in all its sites:
temp$site_count <- aze_sites$nrow[match(temp$site, aze_sites$site)]
aze_spp <- ddply(temp, .(species), summarise, co_count=sum(site_count))
names(aze_spp) <- c("binomial","aze_cocount")
aze_spp$binomial <- as.vector(aze_spp$binomial)
  # Match taxonomic class data:
aze_class <- unique(data.frame(binomial=aze$binomial,taxgroup=aze$Class))
aze_spp$taxgroup <- aze_class$taxgroup[match(aze_spp$binomial, aze_class$binomial)]
rm(aze, aze_sites, temp)


### RELATIVES OF DOMESTICATED ANIMALS:
  # Load:
doman <- read.csv('000_INPUT/Relatives of domesticated animals April 2012.csv',header=T)
  # Build binomial:
doman$binomial <- as.vector(doman$binomial)
  # Set flag:
doman$doman <- 1
  # Subset:
doman <- subset(doman, select=c("binomial", "taxgroup", "doman"))


### IBA SPECIES-SITES
  # Load:
iba <- read.csv('000_INPUT/IBA trigger species EX NOTES.csv',header=T)
  # Build binomial:
iba$binomial <- trim(as.vector(iba$SciName))
  ## Create own unique site code from names; avoids 'blank' site codes in orig data.
iba$EditSiteCode <- iba$SiteInternationalName
levels(iba$EditSiteCode) <- seq(1:length(levels(iba$EditSiteCode)))
  ## Count no. of spp in each unique IBA:
countSpp <- function(x){
  return(length(levels(as.factor(x$binomial))))
}
temp <- subset(iba, select=c("binomial","EditSiteCode"))
  ## iba_sites is an index of unique IBA sites with a count of all species in them.
iba_sites <- ddply(temp, .(EditSiteCode), "countSpp", .progress="text")
iba$countSpp <- iba_sites$countSpp[match(iba$EditSiteCode, iba_sites$EditSiteCode)]
  ## Calc mean no. of species in all IBA's each spp occurs in.
meanSpp <- function(x){
  return(mean(x$countSpp))
}
iba_spp <- subset(iba, select=c('binomial','countSpp'))
  ## iba_spp_mn countains the mean no. of species in all IBA's each spp occurs in.
iba_spp_mn <- ddply(iba_spp, .(binomial), "meanSpp", .progress="text")
  # Remove intermediate files:
rm(iba, iba_sites, iba_spp, temp)

### Palms:

# palms <- read.csv('000_INPUT/Mapisco palms June 2012.csv',header=T)
# names(palms) <- c("rlid","Family","Genus",
#                   "Species","Authority","X",
#                   "rl_status","Harv","ES","Hab",
#                   "rl_crit","rl_crit_ver","year_assessed",
#                   "poptrend","source")
# palms$binomial <- paste(palms$Genus, palms$Species)
  

### COUNTRY DATA
### FROM RED LIST
countries <- read.csv('000_INPUT/RL SPECIES BY COUNTRY.csv', header=T)
countries$binomial <- rl_status$binomial[match(countries$rlid, rl_status$rlid)]
countries <- subset(countries, select=c("country","binomial"))
countries$src <- "rl"
### FROM PLANT SRLI's:
temp1 <- read.csv('000_INPUT/Original/Monocots_Legumes/Monocots SPECIES BY COUNTRY.csv', header=T)
temp2 <- read.csv('000_INPUT/Original/Monocots_Legumes/Legumes SPECIES BY COUNTRY.csv', header=T)
names(temp1)[2] <- "sp_code"
srli_countries <- rbind(temp1, temp2)
rm(temp1, temp2)
srli_countries$Country <- as.vector(srli_countries$Country)
  # Rename countries to match RL standard names:
srli_countries$Country[srli_countries$Country=="Bolivia"] <- "Bolivia, Plurinational State of"
srli_countries$Country[srli_countries$Country=="Venezuela"] <- "Venezuela, Bolivarian Republic of"
srli_countries$Country[srli_countries$Country=="C\xf4te d'Ivoire"] <- "Cote d' Ivoire"
srli_countries$Country[srli_countries$Country=="Tanzania"] <- "Tanzania, United Republic of"
srli_countries$Country[srli_countries$Country=="Libyan Arab Jamahiriya"] <- "Libya"
srli_countries$Country[srli_countries$Country=="Saint Helena"] <- "Saint Helena, Ascension and Tristan da Cunha"
srli_countries$Country[srli_countries$Country=="R\xe9union"] <- "Reunion"
srli_countries$Country[srli_countries$Country=="Disputed Territories"] <- "Disputed Territory"
srli_countries$Country[srli_countries$Country=="Moldova, Republic of"] <- "Moldova"
srli_countries$binomial <- paste(srli_countries$Genus, srli_countries$Species)
srli_countries <- subset(srli_countries, select=c("Country", "binomial"))
names(srli_countries)[1] <- "country"
srli_countries$src <- "srli"
countries <- unique(countries)


### HABITAT DATA 
# Forests - RED LIST:
rl_hab_forest <- read.csv('./000_INPUT/RL_HABITAT_1_Forest-2012-1.csv', header=T)
rl_hab_forest$binomial <- paste(rl_hab_forest$Genus, rl_hab_forest$Species)
rl_hab_forest <- subset(rl_hab_forest, select=c("binomial"))
rl_hab_forest$forest <- 1

temp1 <- read.csv('000_INPUT/Original/Monocots_Legumes/Monocots SPECIES BY HABITAT.csv', header=T)
temp2 <- read.csv('000_INPUT/Original/Monocots_Legumes/Legumes SPECIES BY HABITAT.csv', header=T)
srli_hab <- rbind(temp1, temp2)
rm(temp1, temp2)
srli_hab$binomial <- paste(srli_hab$Genus, srli_hab$Species)
srli_forest <- srli_hab[grep("Forest", srli_hab$Description),]
srli_forest <- subset(srli_forest, select=c("binomial"))
srli_forest$forest <- 1
srli_forest <- unique(srli_forest)

hab_forest <- rbind(rl_hab_forest, srli_forest)
hab_forest <- unique(hab_forest)
rm(rl_hab_forest, srli_forest)

# Wetlands:
rl_hab_wland <- read.csv('./000_INPUT/RL_HABITAT_5_WetlandsInland-2012-1.csv', header=T)
rl_hab_wland$binomial <- paste(rl_hab_wland$Genus, rl_hab_wland$Species)
rl_hab_wland <- subset(rl_hab_wland, select=c("binomial"))
rl_hab_wland$wetland <- 1

srli_wland <- srli_hab[grep("Wetlands", srli_hab$Description),]
srli_wland <- subset(srli_wland, select=c("binomial"))
srli_wland$wetland <- 1
srli_wland <- unique(srli_wland)

hab_wland <- rbind(rl_hab_wland, srli_wland)
hab_wland <- unique(hab_wland)
rm(rl_hab_wland, srli_wland, srli_hab, srli_countries)

### Taxonomic and grouping data where necessary:
  # Freshwater fishes:
fwfishes <- read.csv('000_INPUT/RL_TAX-Fishes-SYS-Freshwater-2012-1.csv',header=T)
fwfishes_classes <- levels(fwfishes$Class)
fwfishes$binomial <- paste(fwfishes$Genus, fwfishes$Species)
fwfishes <- subset(fwfishes, select=c("binomial"))
fwfishes$fwfishes <- 1
  # Marine fishes:
mfishes <- read.csv('000_INPUT/RL_TAX-Fishes-SYS-Marine-2012-1.csv',header=T)
mfishes_classes <- levels(mfishes$Class)
mfishes$binomial <- paste(mfishes$Genus, mfishes$Species)
mfishes <- subset(mfishes, select=c("binomial"))
mfishes$mfishes <- 1
  # Terrestrial mammals:
tmammals <- read.csv('000_INPUT/RL_TAX-Mammalia_SYSTEM-Terrestrial-2012-1.csv',header=T)
tmammals$binomial <- paste(tmammals$Genus, tmammals$Species)
tmammals <- subset(tmammals, select=c("binomial"))
tmammals$tmammals <- 1
  # Marine mammals:
mmammals <- read.csv('000_INPUT/RL_TAX-Mammalia_SYSTEM-Marine-2012-1.csv',header=T)
mmammals$binomial <- paste(mmammals$Genus, mmammals$Species)
mmammals <- subset(mmammals, select=c("binomial"))
mmammals$mmammals <- 1



#################################################################################### 
### SECTION 2 - BUILD FULL SPECIES LIST WITH ID NUMBERS, LINK BACK TO TABLES:
####################################################################################

### BUILD SPECIES BINOMIAL - SPECIES ID TABLE:
temp <- unique(c(aculture$binomial,
                 aze_spp$binomial,
                 bgci$binomial,
                 cwr$binomial,
                 doman$binomial,
                 fb_elsewhere$binomial,
                 #fb_country$binomial,
                 iba_spp_mn$binomial,
                 rl_ac$binomial,
                 rl_status$binomial,
                 srli_status$binomial,
                 levels(countries$binomial),
                 hab_forest$binomial,
                 hab_wland$binomial,
                 fwfishes$binomial,
                 mfishes$binomial,
                 tmammals$binomial,
                 mmammals$binomial
                 ))
spp <- data.frame(binomial=temp,
                  spid=seq(1:length(temp)))
rm(temp)
length(spp$binomial)
length(unique(spp$binomial))


### MATCH SPECIES ID NUMBERS TO NEW TABLES, REMOVE BINOMIAL FROM LATTER:
aculture$spid <- spp$spid[match(aculture$binomial,spp$binomial)]
aculture$binomial <- NULL
aze_spp$spid <- spp$spid[match(aze_spp$binomial, spp$binomial)]
aze_spp$binomial <- NULL
bgci$spid <- spp$spid[match(bgci$binomial, spp$binomial)]
bgci$binomial <- NULL
countries$spid <- spp$spid[match(countries$binomial, spp$binomial)]
countries$binomial <- NULL
countries$rlid <- NULL
cwr$spid <- spp$spid[match(cwr$binomial, spp$binomial)]
cwr$binomial <- NULL
doman$spid <- spp$spid[match(doman$binomial, spp$binomial)]
doman$binomial <- NULL
fb_elsewhere$spid <- spp$spid[match(fb_elsewhere$binomial, spp$binomial)]
fb_elsewhere$binomial <- NULL
iba_spp_mn$spid <- spp$spid[match(iba_spp_mn$binomial, spp$binomial)]
iba_spp_mn$binomial <- NULL
rl_ac$spid <- spp$spid[match(rl_ac$binomial, spp$binomial)]
rl_ac$binomial <- NULL
hab_forest$spid <- spp$spid[match(hab_forest$binomial, spp$binomial)]
hab_forest$binomial <- NULL
hab_wland$spid <- spp$spid[match(hab_wland$binomial, spp$binomial)]
hab_wland$binomial <- NULL
rl_status$spid <- spp$spid[match(rl_status$binomial, spp$binomial)]
rl_status$binomial <- NULL
srli_status$spid <- spp$spid[match(srli_status$binomial, spp$binomial)]
srli_status$binomial <- NULL
srli_status$Species <- NULL
srli_status$Genus <- NULL
srli_status$Sp_code <- NULL
fwfishes$spid <- spp$spid[match(fwfishes$binomial, spp$binomial)]
fwfishes$binomial <- NULL
mfishes$spid <- spp$spid[match(mfishes$binomial, spp$binomial)]
mfishes$binomial <- NULL
tmammals$spid <- spp$spid[match(tmammals$binomial, spp$binomial)]
tmammals$binomial <- NULL
mmammals$spid <- spp$spid[match(mmammals$binomial, spp$binomial)]
mmammals$binomial <- NULL
rl_tax$spid <- spp$spid[match(rl_tax$binomial, spp$binomial)]
rl_tax$binomial <- NULL

### Write outputs:
write.csv(aculture, '000_OUTPUT/Dbase tables/aculture.csv',row.names=F)
write.csv(aze_spp, '000_OUTPUT/Dbase tables/aze_spp.csv',row.names=F)
write.csv(bgci, '000_OUTPUT/Dbase tables/bgci.csv',row.names=F)
write.csv(countries, '000_OUTPUT/Dbase tables/country_spid.csv',row.names=F)
write.csv(cwr, '000_OUTPUT/Dbase tables/cwr.csv',row.names=F)
write.csv(doman, '000_OUTPUT/Dbase tables/doman.csv',row.names=F)
write.csv(fb_elsewhere, '000_OUTPUT/Dbase tables/fb_elsewhere.csv',row.names=F)
write.csv(fwfishes, '000_OUTPUT/Dbase tables/fwfishes.csv', row.names=F)
write.csv(hab_forest, '000_OUTPUT/Dbase tables/hab_forest.csv',row.names=F)
write.csv(hab_wland, '000_OUTPUT/Dbase tables/hab_wland.csv',row.names=F)
write.csv(iba_spp_mn, '000_OUTPUT/Dbase tables/iba_spp_mn.csv',row.names=F)
write.csv(mfishes, '000_OUTPUT/Dbase tables/mfishes.csv', row.names=F)
write.csv(mmammals, '000_OUTPUT/Dbase tables/mmammals.csv', row.names=F)
write.csv(rl_ac, '000_OUTPUT/Dbase tables/rl_ac.csv',row.names=F)
write.csv(rl_status, '000_OUTPUT/Dbase tables/rl_status.csv',row.names=F)
write.csv(rl_tax, '000_OUTPUT/Dbase tables/rl_tax.csv',row.names=F)
write.csv(spp, '000_OUTPUT/Dbase tables/spp.csv',row.names=F)
write.csv(srli_status, '000_OUTPUT/Dbase tables/srli_status.csv',row.names=F)
write.csv(tmammals, '000_OUTPUT/Dbase tables/tmammals.csv', row.names=F)          
   


#################################################################################### 
### SECTION 3 - BUILD TAXONOMIC GROUP TABLE:
####################################################################################

### Re-work taxonomic data from above tables:

  # Amphibians:
amphibians <- data.frame(spid=rl_tax[rl_tax$class=="AMPHIBIA","spid"])
amphibians <- rbind(amphibians, data.frame(spid=aze_spp[aze_spp$taxgroup=="AMPHIBIA","spid"]))
amphibians$tgroup <- "amphibians"
  
  # Birds:
birds <- data.frame(spid=rl_tax[rl_tax$class=="AVES","spid"])
birds <- rbind(birds, data.frame(spid=doman[doman$taxgroup=="birds","spid"]))
birds <- rbind(birds, data.frame(spid=iba_spp_mn$spid))
birds <- rbind(birds, data.frame(spid=aze_spp[aze_spp$taxgroup=="AVES","spid"]))
birds$tgroup <- "birds"

  # Fishes:
fishes <- data.frame(spid=c(aculture$spid,
                            fb_elsewhere$spid,
                            fwfishes$spid, 
                            mfishes$spid
                            ))
fishes$tgroup <- "fishes"

  # Mammals:
mammals <- data.frame(spid=rl_tax[rl_tax$class=="MAMMALIA","spid"])
mammals <- rbind(mammals, data.frame(spid=doman[doman$taxgroup=="mammals","spid"]))
mammals <- rbind(mammals, data.frame(spid=aze_spp[aze_spp$taxgroup=="MAMMALIA","spid"]))
mammals$tgroup <- "mammals"

  # Plants:
plants <- data.frame(spid=rl_tax[rl_tax$kingdom=="PLANTAE","spid"])
plants <- rbind(plants, data.frame(spid=cwr$spid), 
                        data.frame(spid=bgci$spid),
                        data.frame(spid=srli_status$spid)
                )
plants$tgroup <- "plants"

  # Reptiles:
reptiles <- data.frame(spid=rl_tax[rl_tax$class=="REPTILIA","spid"])
reptiles <- rbind(reptiles, data.frame(spid=doman[doman$taxgroup=="reptiles","spid"]))
reptiles <- rbind(reptiles, data.frame(spid=aze_spp[aze_spp$taxgroup=="REPTILIA","spid"]))
reptiles$tgroup <- "reptiles"

  # Validation of duplicates within (should be many) and between (should be none) tax. tables:
    # Within:
  nrow(amphibians); nrow(unique(amphibians))
  amphibians <- unique(amphibians); nrow(amphibians)
  nrow(birds); nrow(unique(birds))
  birds <- unique(birds); nrow(birds)  
  nrow(fishes); nrow(unique(fishes))  
  fishes <- unique(fishes); nrow(fishes)
  nrow(mammals); nrow(unique(mammals))
  mammals <- unique(mammals); nrow(mammals)
  nrow(plants); nrow(unique(plants))
  plants <- unique(plants); nrow(plants)
  nrow(reptiles); nrow(unique(reptiles))
  reptiles <- unique(reptiles); nrow(reptiles)
    # Between:
  group <- rbind(amphibians, birds, fishes, mammals, plants, reptiles)
  nrow(group); nrow(unique(group))
  
  # Build "other" tax group from remaining spid's in spp table:
  other <- data.frame(spid=spp$spid[!(spp$spid %in% group$spid)])
  other$tgroup <- 'other'
  group <- rbind(group, other)
  # Check representation in 'other' group:
#   test <- rl_tax[rl_tax$spid %in% other$spid,]
#   test$kingdom <- as.factor(as.vector(test$kingdom)); levels(test$kingdom)
#   test$class <- as.factor(as.vector(test$class)); levels(test$class)

write.csv(group, '000_OUTPUT/Dbase tables/group.csv', row.names=F)




#################################################################################### 
### SECTION 4 - COUNTRY-LEVEL DATA, NO SPID INDEXING NECESSARY::
####################################################################################

### Load deforestation rate data:
deforest <- read.csv('./000_INPUT/ForestExtentChange.csv')
deforest <- subset(deforest, select=c("country","change2005.2010khayr"))
names(deforest)[length(names(deforest))] <- "deforest_rate"
deforest$country <- as.vector(deforest$country)
  # Alter names to match country names in RL (ISO 3166):
deforest$country[deforest$country=="United Republic of Tanzania"] <- "Tanzania, United Republic of"
deforest$country[deforest$country=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of the"
deforest$country[deforest$country=="Bolivia (Plurinational State of)"] <- "Bolivia, Plurinational State of"
deforest$country[deforest$country=="Venezuela (Bolivarian Republic of)"] <- "Venezuela, Bolivarian Republic of"
deforest$country[deforest$country=="Democratic People's Republic of Korea"] <- "Korea, Democratic People's Republic of"
deforest$country[deforest$country=="Mongolia "] <- "Mongolia"
deforest$country[deforest$country=="Republic of Korea"] <- "Korea, Republic of"
deforest$country[deforest$country=="United States Virgin Islands"] <- "Virgin Islands, U.S."
deforest$country[deforest$country=="British Virgin Islands"]  <- "Virgin Islands, British"
deforest$country[deforest$country=="Libyan Arab Jamahiriya"] <- "Libya"
deforest$country[deforest$country=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"
deforest$country[deforest$country=="Occupied Palestinian Territory"] <- "Palestinian Territory, Occupied"
deforest$country[deforest$country=="C\x99te d'Ivoire"] <- "Cote d'Ivoire"
deforest$country[deforest$country=="Wallis and Futuna Islands"] <- "Wallis and Futuna"
deforest$country[deforest$country=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
deforest$country[deforest$country=="Holy See"] <- "Holy See (Vatican City State)"
deforest$country[deforest$country=="Monaco "] <- "Monaco"
deforest$country[deforest$country=="Svalbard and Jan Mayen Islands"] <- "Svalbard and Jan Mayen"
deforest$country[deforest$country=="Saint Barth\x8elemy"] <- "Saint Barthelemy"
deforest$country[deforest$country=="Falkland Islands (malvinas) *"] <- "Falkland Islands (Malvinas)"
deforest$country[deforest$country=="Belgium "] <- "Belgium"
deforest$country[deforest$country=="R\x8eunion"] <- "Reunion"
deforest$country[deforest$country=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
deforest$country[deforest$country=="Republic of Moldova"] <- "Moldova"
deforest$country[deforest$country=="United States of America"] <- "United States"
deforest$country[deforest$country=="Cote d'Ivoire"] <- "Cote d' Ivoire"
# Test countries in DEFOREST table not occuring in Red List countries:
#deforest$country[deforest$country %in% countries$country==F]
write.csv(deforest, '000_OUTPUT/Dbase tables/deforest.csv',row.names=F)


### Load forest carbon data:
fcarbon <- read.csv('./000_INPUT/ForestCarbon.csv',header=T)
fcarbon <- subset(fcarbon, select=c("country","TperHa2010"))
fcarbon$country <- as.vector(fcarbon$country)
  # Alter names to match country names in RL (ISO 3166):
fcarbon$country[fcarbon$country=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
fcarbon$country[fcarbon$country=="C\x99te d'Ivoire"] <- "Cote d' Ivoire"
fcarbon$country[fcarbon$country=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of the"
fcarbon$country[fcarbon$country=="Democratic People's Republic of Korea"] <- "Korea, Democratic People's Republic of"
fcarbon$country[fcarbon$country=="Belgium "] <- "Belgium"
fcarbon$country[fcarbon$country=="Bolivia (Plurinational State of)"] <- "Bolivia, Plurinational State of"
fcarbon$country[fcarbon$country=="Republic of Moldova"] <- "Moldova"
fcarbon$country[fcarbon$country=="R\x8eunion"] <- "Reunion"
fcarbon$country[fcarbon$country=="United States of America"] <- "United States"
fcarbon$country[fcarbon$country=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
fcarbon$country[fcarbon$country=="United Republic of Tanzania"] <- "Tanzania, United Republic of"
fcarbon$country[fcarbon$country=="Mongolia "] <- "Mongolia"
fcarbon$country[fcarbon$country=="Canadaa"] <- "Canada"
fcarbon$country[fcarbon$country=="Republic of Korea"] <- "Korea, Republic of"
fcarbon$country[fcarbon$country=="Libyan Arab Jamahiriya"] <- "Libya"
fcarbon$country[fcarbon$country=="United States Virgin Islands"] <- "Virgin Islands, U.S."
fcarbon$country[fcarbon$country=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"
fcarbon$country[fcarbon$country=="Holy See"] <- "Holy See (Vatican City State)"
fcarbon$country[fcarbon$country=="Occupied Palestinian Territory"] <- "Palestinian Territory, Occupied"
fcarbon$country[fcarbon$country=="Monaco "] <- "Monaco"
fcarbon$country[fcarbon$country=="Svalbard and Jan Mayen Islands"] <- "Svalbard and Jan Mayen"
fcarbon$country[fcarbon$country=="British Virgin Islands"]  <- "Virgin Islands, British"
fcarbon$country[fcarbon$country=="Saint Barth\x8elemy"] <- "Saint Barthelemy"
fcarbon$country[fcarbon$country=="Wallis and Futuna Islands"] <- "Wallis and Futuna"
fcarbon$country[fcarbon$country=="Falkland Islands (malvinas) *"] <- "Falkland Islands (Malvinas)"
fcarbon$country[fcarbon$country=="Venezuela (Bolivarian Republic of)"] <- "Venezuela, Bolivarian Republic of"

write.csv(fcarbon, '000_OUTPUT/Dbase tables/fcarbon.csv',row.names=F)

### Load water data:
aquastat <- read.csv('./000_INPUT/aquastat.csv', header=T)
names(aquastat) <- c("country","yr","m3FWpCpy","source")
aquastat$country <- as.vector(aquastat$country)
  # Alter names to match country names in RL (ISO 3166):  
aquastat$country[aquastat$country=="Bolivia (Plurinational State of)"] <- "Bolivia, Plurinational State of"
aquastat$country[aquastat$country=="Brunei"] <- "Brunei Darussalam"
aquastat$country[aquastat$country=="C\xf4te d'Ivoire"] <- "Cote d' Ivoire"
aquastat$country[aquastat$country=="Democratic People's Republic of Korea"] <- "Korea, Democratic People's Republic of"
aquastat$country[aquastat$country=="Democratic Republic of the Congo"] <- "Congo, The Democratic Republic of the"
aquastat$country[aquastat$country=="Holy See"] <- "Holy See (Vatican City State)"
aquastat$country[aquastat$country=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"
aquastat$country[aquastat$country=="Laos"] <- "Lao People's Democratic Republic"
aquastat$country[aquastat$country=="Occupied Palestinian Territory"] <- "Palestinian Territory, Occupied"
aquastat$country[aquastat$country=="Phillippines"] <- "Philippines"
aquastat$country[aquastat$country=="Republic of Korea"] <- "Korea, Republic of"
aquastat$country[aquastat$country=="Republic of Moldova"] <- "Moldova"
aquastat$country[aquastat$country=="Sudan and South Sudan"] <- "Sudan"
aquastat$country[aquastat$country=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
aquastat$country[aquastat$country=="East Timor"] <- "Timor-Leste"
aquastat$country[aquastat$country=="United Republic of Tanzania"] <- "Tanzania, United Republic of"
aquastat$country[aquastat$country=="United States of America"] <- "United States"
aquastat$country[aquastat$country=="Venezuela"] <- "Venezuela, Bolivarian Republic of"
aquastat$country[aquastat$country=="Vietnam"] <- "Viet Nam"
aquastat <- aquastat[1:200,]
write.csv(aquastat, '000_OUTPUT/Dbase tables/aquastat.csv',row.names=F)


#### TEST MATCH BETWEEN COUNTRIES IN LOC DATA AND DEFOREST DATA:
# test_countries <- levels(as.factor(country_bin$country))
# check <- rep(NA, length(test_countries))
# for (i in 1:length(test_countries)) {
#   check[i] <- sum(as.numeric(deforest$country==test_countries[i]))
# }
# test <- cbind(test_countries, check)
# test

# #### TEST MATCH BETWEEN COUNTRIES IN LOC DATA AND DEFOREST DATA:
# test_countries <- levels(as.factor(country_bin$country))
# check <- rep(NA, length(test_countries))
# for (i in 1:length(test_countries)) {
#   check[i] <- sum(as.numeric(fcarbon$country==test_countries[i]))
# }
# test <- cbind(test_countries, check)
# test

#### TEST MATCH BETWEEN COUNTRIES IN LOC DATA AND WATER SUPPLY DATA:
# test_countries <- levels(as.factor(country_bin$country))
# check <- rep(NA, length(test_countries))
# for (i in 1:length(test_countries)) {
#   check[i] <- sum(as.numeric(aquastat$country==test_countries[i]))
# }
# test <- cbind(test_countries, check)
# test

#levels(as.factor(aquastat$country))


