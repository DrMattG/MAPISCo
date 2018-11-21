### MAPISCO 02 - build and query database.r
### COMBINE RE-WORKED FILES INTO DATABASE AND PERFORM QUERIES

library(RSQLite)

#################################################################################### 
### SECTION 1 - LOAD DATABASE (mapisco.db) AND READ IN ALL TABLES:
####################################################################################

setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/')
drv <- dbDriver("SQLite") 
con <- dbConnect(drv, "mapisco.db")
# 
# regions <- read.csv('./000_INPUT/Country regions list.csv',header=T)
# ukots <- read.csv('./000_INPUT/UKOTs list.csv',header=T)

# setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/000_OUTPUT/Dbase tables')
# 
# aculture <- read.csv('aculture.csv',header=T)
# aquastat <- read.csv('aquastat.csv',header=T)
# aze_spp <- read.csv('aze_spp.csv', header=T)
# bgci <- read.csv('bgci.csv', header=T)
# country_spid <- read.csv('country_spid.csv', header=T)
# cwr <- read.csv('cwr.csv', header=T)
# deforest <- read.csv('deforest.csv', header=T)
# doman <- read.csv('doman.csv', header=T)
# fb_elsewhere <- read.csv('fb_elsewhere.csv', header=T)
# fcarbon <- read.csv('fcarbon.csv', header=T)
# fwfishes <- read.csv('fwfishes.csv',header=T)
# group <- read.csv('group.csv',header=T)
# hab_forest <- read.csv('hab_forest.csv',header=T)
# hab_wland <- read.csv('hab_wland.csv',header=T)
# iba_spp_mn <- read.csv('iba_spp_mn.csv', header=T)
# mfishes <- read.csv('mfishes.csv',header=T)
# mmammals <- read.csv('mmammals.csv',header=T)
# rl_ac <- read.csv('rl_ac.csv', header=T)
# rl_status <- read.csv('rl_status.csv', header=T)
# rl_tax <- read.csv('rl_tax.csv',header=T)
# spp <- read.csv('spp.csv', header=T)
# srli_status <- read.csv('srli_status.csv', header=T)
# tmammals <- read.csv('tmammals.csv',header=T)

# dbWriteTable(con, "aculture", aculture, row.names=F)
# dbWriteTable(con, "fwater", aquastat, row.names=F)
# dbWriteTable(con, "aze_spp", aze_spp, row.names=F)
# dbWriteTable(con, "bgci", bgci, row.names=F)
# dbWriteTable(con, "country_spid", country_spid, row.names=F)
# dbWriteTable(con, "cwr", cwr, row.names=F)
# dbWriteTable(con, "deforest", deforest, row.names=F)
# dbWriteTable(con, "doman", doman, row.names=F)
# dbWriteTable(con, "fb_elsewhere", fb_elsewhere, row.names=F)
# dbWriteTable(con, "fcarbon", fcarbon, row.names=F)
# dbWriteTable(con, "fwfishes", fwfishes, row.names=F)
# dbWriteTable(con, "grouping", group, row.names=F)
# dbWriteTable(con, "hab_forest", hab_forest, row.names=F)
# dbWriteTable(con, "hab_wland", hab_wland, row.names=F)
# dbWriteTable(con, "iba_spp_mn", iba_spp_mn, row.names=F)
# dbWriteTable(con, "mfishes", mfishes, row.names=F)
# dbWriteTable(con, "mmammals", mmammals, row.names=F)
# dbWriteTable(con, "regions", regions, row.names=F)
# dbWriteTable(con, "rl_ac", rl_ac, row.names=F)
# dbWriteTable(con, "rl_status", rl_status, row.names=F)
# dbWriteTable(con, "rl_tax", rl_tax, row.names=F)
# dbWriteTable(con, "spp", spp, row.names=F)
# dbWriteTable(con, "srli_status", srli_status, row.names=F)
# dbWriteTable(con, "tmammals", tmammals, row.names=F)
# dbWriteTable(con, "ukots", ukots, row.names=F)
# rm(aculture, aquastat, aze_spp, bgci, country_spid, cwr,
#    deforest, doman, fb_elsewhere, fcarbon, iba_spp_mn,
#    rl_ac, hab_forest, hab_wland, rl_status, rl_tax,
#    mmammals, tmammals, mfishes, fwfishes, group, spp,srli_status,
#    regions, ukots)

# setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/')

### TEST QUERY:
dbGetQuery(con, "SELECT *
                 FROM rl_status
                 LIMIT 10")


#################################################################################### 
### SECTION 2 - QUERY EXISTING COUNTRY-LEVEL TABLES TO CREATE SPECIES-BY-COUNTRY TABLES
####################################################################################

# ## Build indexed tables for each region first.
# 
# sAsia <- dbGetQuery(con, "SELECT DISTINCT spid
#                            FROM country_spid 
#                            WHERE country 
#                            IN (SELECT country 
#                                FROM regions 
#                                WHERE region='South Asia') 
#                           ")
# sAsia$sAsia <- 1
# dbWriteTable(con, "sAsia", sAsia, row.names=F)
# rm(sAsia)
# 
# seAsia <- dbGetQuery(con, "SELECT DISTINCT spid
#                            FROM country_spid 
#                            WHERE country 
#                            IN (SELECT country 
#                                FROM regions 
#                                WHERE region='Southeast Asia') 
#                           ")
# seAsia$seAsia <- 1
# dbWriteTable(con, "seAsia", seAsia, row.names=F)
# rm(seAsia)
# 
# sAmerica <- dbGetQuery(con, "SELECT DISTINCT spid
#                            FROM country_spid 
#                            WHERE country 
#                            IN (SELECT country 
#                                FROM regions 
#                                WHERE region='South America') 
#                           ")
# sAmerica$sAmerica <- 1
# dbWriteTable(con, "sAmerica", sAmerica, row.names=F)
# rm(sAmerica)

### This beast joins DEFOREST and FCARBON tables, and multiplies both
###  values into a new column CarbonDeforest, estimating Tonnes of carbon
###  lost per ha per year if deforestation rates continue on 2005-2010 levels.
# dbGetQuery(con, "SELECT fcarbon.country,
#                        TperHa2010, 
#                        deforest_rate, 
#                        TperHa2010*deforest_rate 'CarbonDeforest' 
#                 FROM fcarbon 
#                 JOIN deforest 
#                  ON fcarbon.country=deforest.country")

### This MONSTER expands the query above by first joining it with the COUNTRY_SPID
###  table, on country values.
### This results in another subquery with the estimated C loss from deforestation
###  matched against many individual species occuring in many countries.
### The next (outer) query then averages the estimated C loss from deforestation
###  by 'spid', so per species across all countries it occurs in.

# avcdeforest_spid <- dbGetQuery(con, "SELECT spid, avg(CarbonDeforest) 'AvCDeforest'
#                                      FROM (
#                                         SELECT country_spid.country 'country', 
#                                                country_spid.spid 'spid', 
#                                                CarbonDeforest 
#                                         FROM country_spid 
#                                         JOIN (SELECT fcarbon.country,
#                                                      TperHa2010, 
#                                                      deforest_rate, 
#                                                      TperHa2010*deforest_rate 'CarbonDeforest' 
#                                               FROM fcarbon 
#                                               JOIN deforest 
#                                               ON fcarbon.country=deforest.country) a 
#                                         ON country_spid.country=a.country
#                                      ) GROUP BY spid")
# dbWriteTable(con, "avcdeforest_spid", avcdeforest_spid, row.names=F)
# rm(avcdeforest_spid)

### This replicates the operation above for freshwater supply:

# avFWsupply <- dbGetQuery(con, "SELECT spid, avg(m3FWpCpy) 'AvFWsupply'
#                                FROM (
#                                      SELECT country_spid.country, 
#                                             country_spid.spid, 
#                                             m3FWpCpy 
#                                      FROM country_spid 
#                                      JOIN fwater 
#                                       ON country_spid.country=fwater.country
#                                ) GROUP BY spid")
# dbWriteTable(con, "avFWsupply", avFWsupply, row.names=F)
# rm(avFWsupply)

# dbGetQuery(con, "SELECT * FROM avFWsupply LIMIT 25")

### Indexed table for all UKOTs and specific example:

# ukot_spid <- dbGetQuery(con, 
#                         "SELECT DISTINCT spid 
#                          FROM country_spid 
#                          WHERE country IN (SELECT country 
#                                            FROM ukots)"
#                         )
# ukot_spid$ukot <- 1
# biot_spid <- dbGetQuery(con, "SELECT spid 
#                               FROM country_spid 
#                               WHERE country='British Indian Ocean Territory'")
# biot_spid$biot <- 1
# dbWriteTable(con, "ukot_spid", ukot_spid, row.names=F)
# dbWriteTable(con, "biot_spid", biot_spid, row.names=F)
# rm(biot_spid, ukot_spid)



#################################################################################### 
### SECTION 3 - MASTER QUERY GENERATING FULL SPECIES LIST AGAINST ALL DATA SOURCES
####################################################################################

OUT <- dbGetQuery(con, "
                   SELECT spp.spid,
                          spp.binomial,
                          tgroup,
                          vernacular,
                          status,
                          rl_category 'srli_status',
                          aculture, 
                          aculture_rl, 
                          cwr,
                          bgci_med_priority,
                          fb_comm_overall,
                          aze_cocount,
                          doman,
                          meanSpp,
                          sAsia,
                          seAsia,
                          sAmerica,
                          ukot,
                          biot,
                          forest,
                          wetland,
                          AvCDeforest,
                          AvFWsupply
                   FROM spp
                   LEFT OUTER JOIN grouping
                    ON spp.spid=grouping.spid
                   LEFT OUTER JOIN rl_tax
                    ON spp.spid=rl_tax.spid
                   LEFT OUTER JOIN rl_status 
                    ON spp.spid=rl_status.spid
                   LEFT OUTER JOIN srli_status
                    ON spp.spid=srli_status.spid
                   LEFT OUTER JOIN (SELECT DISTINCT * FROM aculture) a 
                    ON spp.spid=a.spid
                   LEFT OUTER JOIN rl_ac
                    ON spp.spid=rl_ac.spid
                   LEFT OUTER JOIN (SELECT DISTINCT spid,cwr FROM cwr) b 
                    ON spp.spid=b.spid
                   LEFT OUTER JOIN bgci
                    ON spp.spid=bgci.spid
                   LEFT OUTER JOIN fb_elsewhere
                    ON spp.spid=fb_elsewhere.spid
                   LEFT OUTER JOIN aze_spp
                    ON spp.spid=aze_spp.spid
                   LEFT OUTER JOIN doman
                    ON spp.spid=doman.spid
                   LEFT OUTER JOIN iba_spp_mn
                    ON spp.spid=iba_spp_mn.spid
                   LEFT OUTER JOIN sAsia
                    ON spp.spid=sAsia.spid
                   LEFT OUTER JOIN seAsia
                    ON spp.spid=seAsia.spid
                   LEFT OUTER JOIN sAmerica
                    ON spp.spid=sAmerica.spid
                   LEFT OUTER JOIN ukot_spid
                    ON spp.spid=ukot_spid.spid
                   LEFT OUTER JOIN biot_spid
                    ON spp.spid=biot_spid.spid
                   LEFT OUTER JOIN hab_forest
                    ON spp.spid=hab_forest.spid
                   LEFT OUTER JOIN hab_wland
                    ON spp.spid=hab_wland.spid
                   LEFT OUTER JOIN avcdeforest_spid
                    ON spp.spid=avcdeforest_spid.spid
                   LEFT OUTER JOIN avFWsupply
                    ON spp.spid=avFWsupply.spid
                  ")
# write.csv(OUT, '000_OUTPUT/FULL LIST FLAT.csv', row.names=F, na="")




