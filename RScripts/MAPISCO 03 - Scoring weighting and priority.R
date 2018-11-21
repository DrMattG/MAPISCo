#### MAPISCO 03 - SCORING, WEIGHTING & PRIORITISATION 
####

library(reshape2)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(RSQLite)

setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/000_OUTPUT')
all <- read.csv('FULL LIST FLAT.csv', header=T)


#################################################################################### 
### SECTION 1 - SCORING OF DATA SOURCES, RESCALING SCORES
####################################################################################

###### SCORING

  ### Red list status
  print(levels(all$status))
  table_status <- rbind(c("",  NA),
                        c("CR",9),
                        c("EW",8),
                        c("EN",7),
                        c("VU",6),
                        c("NT",5),
                        c("DD",4),
                        c("LR/cd",3),
                        c("LR/lc",3),
                        c("LR/nt",3),
                        c("LC",2),
                        c("EX",1)
                        )
  table_status <- as.data.frame(table_status)
  names(table_status) <- c("status","score")
  table_status$score <- as.vector(table_status$score)
  all$s_status <- table_status$score[match(all$status, table_status$status)]
  all$s_status <- as.numeric(all$s_status)
#     test:
#       data.frame(all$status, all$s_status)
  
  ### SRLI plants:
  print(levels(all$srli_status))
  table_srli <- rbind(c("",  NA),
                        c("CR",9),
                        c("EW",8),
                        c("EN",7),
                        c("VU",6),
                        c("NT",5),
                        c("DD",4),
                        c("LR/cd",3),
                        c("LR/lc",3),
                        c("LR/nt",3),
                        c("LC",2),
                        c("NE",0),
                        c("EX",1)
                        )
  table_srli <- as.data.frame(table_srli)
  names(table_srli) <- c("srli","score")
  table_srli$score <- as.vector(table_srli$score)
  all$s_srli <- table_srli$score[match(all$srli_status, table_srli$srli)]
  all$s_srli <- as.numeric(all$s_srli)

  ### Habitat & area:
  all$s_aze <- all$aze_cocount
  all$s_iba <- all$meanSpp
  
  ### Sustainable harvesting:
  all$s_aqc <- all$aculture

  ### Aquaculture
  #  - Red List
  
  aculture_rl_score <- rbind(c("Industrial",3),
                             c("Subsistence/artisinal",2),
                             c("Scale unknown",1),
                             c("",NA)
                             )
  aculture_rl_score <- as.data.frame(aculture_rl_score)
  names(aculture_rl_score) <- c("aculture_rl","score")
  aculture_rl_score$score <- as.vector(aculture_rl_score$score)
  all$s_aqc_rl <- aculture_rl_score$score[match(all$aculture_rl, aculture_rl_score$aculture_rl)]
  all$s_aqc_rl <- as.numeric(all$s_aqc_rl)
  # test:
#   data.frame(all$aculture_rl, all$s_aqc_rl)[all$aculture_rl!="",]

  # - FishBase - "Commercial" species
  fb_score <- as.data.frame(rbind(c("highly commercial",6),
                                  c("commercial",5),
                                  c("minor commercial",4),
                                  c("subsistence fisheries",3),
                                  c("of potential interest",2),
                                  c("of no interest",1),
                                  c("",NA)
                            ))
  names(fb_score) <- c("fb","score")
  fb_score$score <- as.vector(fb_score$score)
  all$s_commf <- fb_score$score[match(all$fb_comm_overall, fb_score$fb)]
  all$s_commf <- as.numeric(all$s_commf)
    # test:
#     data.frame(all$fb_comm_overall, all$s_commf)[all$fb_comm_overall!="",]
  
  ### Conservation of Genetic Diversity:
  all$s_cwr <- all$cwr
  all$s_bgci <- all$bgci
  all$s_doman <- all$doman
  
  ### Ecosystem Service provision:
  all$s_carfor <- all$AvCDeforest
  all$s_wat <- all$AvFWsupply


###### RESCALING:

  all$s_status <- all$s_status/max(all$s_status[complete.cases(all$s_status)])
  all$s_srli <- all$s_srli/max(all$s_srli[complete.cases(all$s_srli)])
  all$s_aze <- all$s_aze/max(all$s_aze[complete.cases(all$s_aze)])
  all$s_iba <- all$s_iba/max(all$s_iba[complete.cases(all$s_iba)])
  all$s_aqc_rl <- all$s_aqc_rl/max(all$s_aqc_rl[complete.cases(all$s_aqc_rl)])
  all$s_commf <- all$s_commf/max(all$s_commf[complete.cases(all$s_commf)])
  all$s_bgci <- all$s_bgci/max(all$s_bgci[complete.cases(all$s_bgci)])
    # So s_carfor is the tonnes of carbon lost per ha of forest, provided
    #  deforestation continues at the estimates 2005-2010 rate.
    # To make sure the highest rate of loss = highest score:
  temp <- abs(all$s_carfor-max(all$s_carfor, na.rm=T))
  all$s_carfor <- temp/max(temp, na.rm=T)
    # This makes sure that "carfor" is only applicable to forest species:
  all$s_carfor[is.na(all$forest)] <- NA
  
  # s_wat is freshwater provision (m3/capita/yr). Lower = higher score:
    temp <- all$s_wat
    maxval <- max(all$s_wat,na.rm=T)
    all$s_wat <- abs(all$s_wat-maxval)
    rm(maxval)
  all$s_wat <- all$s_wat/max(all$s_wat, na.rm=T)
    # This makes sure that s_wat is only applicable to forest or wetland species:
  all$s_wat[apply(cbind(all$forest, all$wetland),1,sum,na.rm=T)==0] <- NA



#################################################################################### 
### SECTION 2 - FUNCTIONS FOR OUTPUTTING AND ANALYSIS
####################################################################################


##### WEIGHTING AND PRIORITISATION:
# Function to run prioritisation calculation with specific parameters:

#### Weighting values:
### wStat = THREAT STATUS 
### wHab = Habitat & area protection
### wHarv = Sustainable harvesting
### wGenD = Genetic diversity
### wES = Ecosystem Service provision

### NOTE THAT wES SHOULD BE SET TO 0 FOR A GLOBAL PRIORITY LIST,
###  BECAUSE COUNTRY DATA HAS ONLY BEEN ADDED FOR S&SE ASIA, AND
###  S AMERICA!

  max_func <- function(x){
    if(sum(complete.cases(x))==0) {return(NA);}
    if(sum(complete.cases(x))==1) {return(x[complete.cases(x)])}
    if(sum(complete.cases(x))==2) {return(max(x))}
  }

  calcP_SUM <- function(dat=all, wts=list(wStat=1, wHab=1, wHarv=1, wGenD=1, wES=1)) {
    
    dat$Stat <- apply(cbind(dat$s_status, dat$s_srli),1,max_func)
    dat$Hab <- apply(cbind(dat$s_aze, dat$s_iba),1, sum, na.rm=T)/2
    dat$Harv <- apply(cbind(dat$s_aqc, dat$s_aqc_rl, dat$s_commf),1, sum, na.rm=T)/3
    dat$GenD <- apply(cbind(dat$s_cwr, dat$s_bgci, dat$s_doman),1, sum, na.rm=T)/3
    dat$ES <- apply(cbind(dat$s_carfor, dat$s_wat),1, sum, na.rm=T)/2

    temp <- cbind(wts$wStat*dat$Stat, 
                      wts$wHab*dat$Hab, 
                      wts$wHarv*dat$Harv, 
                      wts$wGenD*dat$GenD, 
                      wts$wES*dat$ES)
    
    dat$pscore <- apply(temp, 1, sum, na.rm=T)/5
    
    dat <- dat[order(dat$pscore, decreasing=T),]

    dat$Rank <- as.factor(dat$pscore)
    levels(dat$Rank) <- seq(length(levels(dat$Rank)),1,-1)
  
    return(dat)
  }

  calcP_AV <- function(dat=all, wts=list(wStat=1, wHab=1, wHarv=1, wGenD=1, wES=1)) {
    
    dat$Stat <- apply(cbind(dat$s_status, dat$s_srli),1,max_func)
    dat$Hab <- apply(cbind(dat$s_aze, dat$s_iba),1,mean,na.rm=T)
    dat$Harv <- apply(cbind(dat$s_aqc, dat$s_aqc_rl, dat$s_commf),1,mean,na.rm=T)
    dat$GenD <- apply(cbind(dat$s_cwr, dat$s_bgci, dat$s_doman),1,mean,na.rm=T)
    dat$ES <- apply(cbind(dat$s_carfor, dat$s_wat),1,mean,na.rm=T)
    
    temp <- cbind(wts$wStat*dat$Stat, 
                      wts$wHab*dat$Hab, 
                      wts$wHarv*dat$Harv, 
                      wts$wGenD*dat$GenD, 
                      wts$wES*dat$ES)
    
    dat$pscore <- apply(temp, 1, sum, na.rm=T)/5
    
    dat <- dat[order(dat$pscore, decreasing=T),]
    
    dat$Rank <- as.factor(dat$pscore)
    levels(dat$Rank) <- seq(length(levels(dat$Rank)),1,-1)
    
    return(dat)
  }


###### PREPARE OUTPUT:

  prepOut <- function(dat)
  {
    output <- with(dat, {
                  data.frame(ID=spid,
                             SciName=binomial,
                             TaxGroup=tgroup,
                             EngName=vernacular,
                             UKOT=ukot,
                             Forest=forest,
                             Wetland=wetland,
                             ThreatStatus=status,
                             SRLIStatus=srli_status,
                             coAZEspp=aze_cocount,
                             mnIBAspp=round(meanSpp,3),
                             Aquaculture=aculture,
                             AquacultureRL=aculture_rl,
                             CommercialHarv=fb_comm_overall,
                             CWRspp=cwr,
                             Medicinal=bgci_med_priority,
                             WildRelDomAn=doman,
                             AvCDeforest=round(AvCDeforest,3),
                             AvFWsupply=round(AvFWsupply,3),
                             s_status=s_status,
                             s_srli=s_srli,
                             s_aze=s_aze,
                             s_iba=s_iba,
                             s_aqc=s_aqc,
                             s_aqc_rl=s_aqc_rl,
                             s_commf=s_commf,
                             s_cwr=s_cwr,
                             s_bgci=s_bgci,
                             s_doman=s_doman,
                             s_carfor=s_carfor,
                             s_wat=s_wat,
                             Stat=Stat,
                             Hab=Hab,
                             Harv=Harv,
                             GenD=GenD,
                             ES=ES,
                             PriorityScore=pscore,
                             Rank=Rank
                             )
                    })
    return(output)
  }


### topStats(): CALCULATE TOP X STATISTICS OF A GIVEN LISTt:

  topStats <- function(dat, topX=500){
    #dat$rank <- as.factor(dat$pscore)
    #levels(dat$rank) <- seq(length(levels(dat$rank)),1,-1)
    toplist <- dat[1:topX,]
    toplist$tgroup <- as.factor(as.vector(toplist$tgroup))
    
    RowNames <- c("# taxonomic groups",
                  "# unique ranks",
                  "# Critically Endangered spp.",
                  "# Least Concern spp.",
                  "Top 5 species")
    
    Vals <- as.vector(NULL)
    # Number of taxonomic groups in top 100:
    Vals[1] <- as.character(length(levels(toplist$tgroup)))
    # Number of unique ranks in top 100:
    Vals[2] <- as.character(length(unique(toplist$Rank)))
    # Number of CR species in top 100:
    Vals[3] <- as.character(sum(toplist$status=="CR"))
    # Number of LC species in top 100:
    Vals[4] <- as.character(sum(toplist$status=="LC"))
    # Top 5:
    Vals[5] <- paste(dat$vernacular[1:5],collapse=", ")
    
    Group <- data.frame(Group=levels(dat$tgroup))
    Group$Number <- 0
    toplist_perc <- as.data.frame(round((table(toplist$tgroup)/topX)*100,2))
    toplist_count <- as.data.frame(table(toplist$tgroup))
    Group$Number <- toplist_count$Freq[match(Group$Group, toplist_count$Var1)]
    Group$Perc <- toplist_perc$Freq[match(Group$Group, toplist_perc$Var1)]
    Group$Number[is.na(Group$Number)] <- 0
    
    # Group <- ddply(dat[1:topX,],.(tgroup),summarise,V1=round((length(binomial)/topX)*100,2), .drop=F)
    # names(Group) <- c("Group","Number")
    
    out <- as.data.frame(t(data.frame(Vals)))
    names(out) <- RowNames
    
    species <- data.frame(spid=toplist$spid, scientific=toplist$binomial, vernacular=toplist$vernacular)
    
    return(list(stats=out, bygroup=Group, species=species))
  }

### cumSens: CALCULATE CHANGES IN RANKS (dRank) AND POSITION (dPos) FOR THE FULL LIST AND A TOP X.
###  Argument 'vals' is a table with weights (wts) for the five co-benefits as columns, and an
###   arbitrary number of rows representing combinations of co-benefit weights:

  cumSens_SUM <- function(vals, topX=500) {
    
    pb <- txtProgressBar(min=0,max=nrow(vals), style=3)
    
    vals$dRank <- NA
    vals$dPos <- NA
    vals$dRank_tX <- NA
    vals$dPos_tX <- NA
    
    list1 <- calcP_SUM()
    list1 <- list1[order(list1$pscore, list1$spid ,decreasing=T),]
    sp_occ <- data.frame(spid=list1$spid[1:topX], 
                         binomial=list1$binomial[1:topX], 
                         vernacular=list1$vernacular[1:topX])
    list1$Pos <- 1:nrow(list1)
    list1 <- subset(list1, select=c("spid","binomial","pscore","Rank","Pos"))
    list1$Rank <- as.numeric(as.vector(list1$Rank))
    
    for (i in 1:nrow(vals)) {
      list2 <- calcP_SUM(wts=as.list(vals[i,]))
      list2 <- list2[order(list2$pscore,list2$spid,decreasing=T),]
      sp_occ <- rbind(sp_occ, data.frame(spid=list2$spid[1:topX], 
                                         binomial=list2$binomial[1:topX], 
                                         vernacular=list2$vernacular[1:topX]))
      list2$Pos <- 1:nrow(list2)
      list2 <- subset(list2, select=c("spid","binomial","pscore","Rank","Pos"))
      list2$Rank <- as.numeric(as.vector(list2$Rank))
      
      list1$newRank <- list2$Rank[match(list1$spid, list2$spid)]
      list1$newPos <-  list2$Pos[match(list1$spid, list2$spid)]
      
      vals$dRank[i] <- mean(abs(list1$newRank-list1$Rank))
      vals$dPos[i] <- mean(abs(list1$newPos-list1$Pos))
      vals$dRank_tX[i] <- mean(abs(list1$newRank[1:topX]-list1$Rank[1:topX]))
      vals$dPos_tX[i] <- mean(abs(list1$newPos[1:topX]-list1$Pos[1:topX]))
      
      setTxtProgressBar(pb, i,)
    }
    sp_occ <- ddply(sp_occ, .(binomial), nrow)
    close(pb)
    return(list(rankChanges=vals,sppFreqTopX=sp_occ))
  }

# hd() is a 'head()' type function that outputs a selection of columns
#  from default list type output. The 'display' parameter sets the number
#  of lines to be printed.

hd <- function(dat, display=10) {
  out <- prepOut(dat)
  out <- with(out, { data.frame(SciName=SciName,
                         TaxGroup=TaxGroup,
                         Stat=round(Stat,3), 
                         Hab=round(Hab,3), 
                         Harv=round(Harv,3), 
                         GenD_S=round(GenD,3),
                         ES_S=round(ES,3),
                         PScore=round(PriorityScore,3),
                         Rank=round(as.numeric(as.vector(Rank)),3)
                                )
                     })
  return(out[1:display,])
}




#################################################################################### 
### SECTION 3 - ANALYSIS AND DESCRIPTIVE STATISTICS OF RESULTING LIST
####################################################################################


list1_sum <- calcP_SUM() # Not used, only to test function is working OK.
list1_av <- calcP_AV() # Not used, only to test function is working OK.

list1 <- calcP_SUM()    # Basic priority list, calculated as SUM divided by no. of sources.
OUT3 <- prepOut(list1)  # Output only used to save .csv file.
write.csv(OUT3, 'GLOBAL LIST - DEFAULTS - WITH ES GLOBAL - NEW3.csv', row.names=F, na="")

  ### Store flat file output in database, for display purposes

#   setwd('~/Documents/docs/MAPISCO/000_Data/')
#   drv <- dbDriver("SQLite") 
#   con <- dbConnect(drv, "mapisco.db")
#   dbWriteTable(con, "main_output", OUT3, row.names=F, overwrite=T)
#   dbDisconnect(con)
#   setwd('/Users/MacSizedRoon/Documents/docs/MAPISCO/HANDOVER/000_DATA/000_OUTPUT')

### RESULTS - 

  # Total number of unique species:
nrow(list1)  
  # Highest priority score
round(max(list1$pscore),3)
  # Priority score for 10th species:
round(list1$pscore[10],3)
  # Sci names, scores and ranks in Top 10 (up to and including Rank 10):
subset(list1[1:10,], select=c("binomial","tgroup","pscore","Rank"))
# Highest scores, ranks and species per tax. group:

  # Score resolution for full list, no. of ranks and total no. of species:
round((length(levels(list1$Rank))/nrow(list1))*100,2); length(levels(list1$Rank)); nrow(list1)

  # Percentages of species scored for each CB:
round((sum(list1[1:500,"Stat"]!=0)/500)*100,1)
round((sum(list1[1:500,"ES"]!=0)/500)*100,1)
round((sum(list1[1:500,"Hab"]!=0)/500)*100,1)
round((sum(list1[1:500,"Harv"]!=0)/500)*100,1)
round((sum(list1[1:500,"GenD"]!=0)/500)*100,1)



  # Numbers and percentages of taxonomic groups in top 500:
topStats(list1, 500)$bygroup

### Table 5:
Table5 <- cbind(ddply(list1, .(tgroup), summarise, 
                      TaxGroupMaxRank=min(as.numeric(as.vector(Rank))), 
                      TaxGroupMaxScore=round(max(pscore),3), 
                      species=binomial[1]), 
                topStats(list1, 500)$bygroup)


  ### Table 6:  

temp <- list1[1:500,]
Table6 <- data.frame(
           binomial=temp$binomial,
           tgroup=temp$tgroup,
           Stat=round(temp$Stat,3),
           Hab=round(temp$Hab,3),
           Harv=round(temp$Harv,3),
           GenD=round(temp$GenD,3),
           ES=round(temp$ES,3), 
           Score=round(temp$pscore,3), 
           Rank=round(as.numeric(as.vector(temp$Rank)),3))

### SENSITIVITY STATS & PLOTS

### Figure 3:

  # Mean changes in ranks for full list and Top 500. Plot as figure.
  # First set up input weight matrix for cumSens() function:
series <- seq(0.9,0,-0.1)
invals <- matrix(1,nrow=5*length(series),ncol=5)
invals[1:10,1] <- series
invals[11:20,2] <- series
invals[21:30,3] <- series
invals[31:40,4] <- series
invals[41:50,5] <- series
invals <- data.frame(invals)
names(invals) <- c("wStat","wHab","wHarv","wGenD","wES")
  # Run function:
wts_sens <- cumSens_SUM(invals, topX=500)
  # Plot:
plotdat <- wts_sens$rankChanges
plotdat$weight <- rep(seq(-0.1,-1,-0.1),5)
plotdat$Group <- c(rep("Threat status",10), 
                   rep("Habitat",10), 
                   rep("Harvesting",10), 
                   rep("Gen. diversity",10), 
                   rep("ES",10))

  # Mean change in rank for ALL species:

  # p <- ggplot(plotdat, aes(x=weight,y=dRank, group=Group, colour=Group)) + scale_x_reverse()
  # p <- p + geom_line()
  # p <- p + labs(x="Decrease in weight (from 1)", 
  #               y="Mean absolute change in rank")
  # p <- p + scale_fill_hue(c = 50, l = 70, h=c(270, 150))
  # p + opts(
  #   panel.background=theme_blank(),
  #   panel.border=theme_rect(colour="grey",size=0.5),
  #   panel.grid.major=theme_line(colour="grey",size=0.2),
  #   axis.title.x=theme_text(size=20),
  #   axis.title.y=theme_text(size=20, angle=90),
  #   axis.text.x=theme_text(size=15),
  #   axis.text.y=theme_text(size=15),
  #   legend.text=theme_text(size=15),
  #   legend.title=theme_text(size=15))

  # Mean change in rank for top X species only:

p <- ggplot(plotdat, aes(x=weight,y=dRank_tX, group=Group, colour=Group)) + scale_x_reverse()
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_colour_manual(values=brewer.pal("Dark2",n=5))
p <- p + labs(x="Decrease in weight (from 1)", 
              y="Mean absolute change in rank of Top 500 species")
#p <- p + scale_fill_hue(c = 50, l = 70, h=c(270, 150))
p + opts(
  #panel.background=theme_blank(),
  #panel.border=theme_rect(colour="grey",size=0.5),
  #panel.grid.major=theme_line(colour="grey",size=0.2),
  axis.title.x=theme_text(size=18),
  axis.title.y=theme_text(size=18, angle=90),
  axis.text.x=theme_text(size=15),
  axis.text.y=theme_text(size=15),
  legend.text=theme_text(size=15),
  legend.title=theme_text(size=15))

# Mean absolute change in rank from 1 to 0.5..
#  ..for Threat status:
round(plotdat[plotdat$Group=="Threat status" & plotdat$weight==-0.5,"dRank_tX"],1)
#  ..for ES:
round(plotdat[plotdat$Group=="ES" & plotdat$weight==-0.5,"dRank_tX"],1)
#  ..for ES:
round(plotdat[plotdat$Group=="Gen. diversity" & plotdat$weight==-0.5,"dRank_tX"],1)
#  ..for Hab&Area:
round(plotdat[plotdat$Group=="Habitat" & plotdat$weight==-0.5,"dRank_tX"],1)
#  ..for Harvesting:
round(plotdat[plotdat$Group=="Harvesting" & plotdat$weight==-0.5,"dRank_tX"],1)

### Figure 4:

p <- ggplot(wts_sens$sppFreqTopX, aes(x=V1))
p <- p + geom_histogram(binwidth=3, fill="Darkgrey", colour="Black")
p <- p + labs(x="Frequency of occurrence in Top 500", 
              y="Frequency")
p
p + opts(
  axis.title.x=theme_text(size=16),
  axis.title.y=theme_text(size=16, angle=90),
  axis.text.x=theme_text(size=13),
  axis.text.y=theme_text(size=13),
  legend.text=theme_text(size=13),
  legend.title=theme_text(size=13))



#  Compare 0-1 change for Harvesting and Threat Status
print(t1 <- round(plotdat[plotdat$Group=="Threat status" & plotdat$weight==-1,"dRank_tX"],1))
print(t2 <- round(plotdat[plotdat$Group=="Harvesting" & plotdat$weight==-1,"dRank_tX"],1))
t1/t2

### Number of different species in top 500 as weightings are varied:
sp_occ <- wts_sens$sppFreqTopX
nrow(sp_occ)
# Maximum occurrence of species in top 500:
max(sp_occ$V1)
# Histogram of spp occurence in the top 500:
hist(sp_occ$V1)
# Number of species >30 times in top 500:
nrow(sp_occ[sp_occ$V1>30,])
round((length(sp_occ$V1[sp_occ$V1>30])/nrow(sp_occ))*100,1)
# Number of species <10 times in top 100:
nrow(sp_occ[sp_occ$V1<10,])
round((length(sp_occ$V1[sp_occ$V1<10])/nrow(sp_occ))*100,1)
# Groups amongst species occurring >30 times in top 500:
temp <- list1[list1$binomial %in% as.vector(sp_occ[sp_occ$V1>30,"binomial"]),]
temp$TaxGroup <- as.factor(as.vector(temp$tgroup))
temp2 <- ddply(temp, .(TaxGroup), nrow)
temp2$V2 <- round((temp2$V1/sum(temp2$V1))*100,1)
temp2

  # Table A5-2:
  #  (List of highest ranking species occuring >30 times in the Top500):
TableA5_2 <- hd(list1[list1$binomial %in% sp_occ[sp_occ$V1>30,"binomial"],], 507)
write.csv(TableA5_2, "Table A5-2.csv", row.names=F)

### By taxonomic group, -0.5 weighting change, representation in top 500:

### STATS FOR THE GLOBAL LIST TOP 100, NO ES:

stats_1 <- topStats(calcP_SUM(dat=all, wts=list(wStat=1,wHab=1,wHarv=1,wGenD=1,wES=1)), 500)
stats_Stat <- topStats(calcP_SUM(dat=all, wts=list(wStat=0.5,wHab=1,wHarv=1,wGenD=1,wES=1)), 500)
stats_Hab <- topStats(calcP_SUM(dat=all, wts=list(wStat=1,wHab=0.5,wHarv=1,wGenD=1,wES=1)), 500)
stats_Harv <- topStats(calcP_SUM(dat=all, wts=list(wStat=1,wHab=1,wHarv=0.5,wGenD=1,wES=1)), 500)
stats_GenD <- topStats(calcP_SUM(dat=all, wts=list(wStat=1,wHab=1,wHarv=1,wGenD=0.5,wES=1)), 500)    
stats_ES <- topStats(calcP_SUM(dat=all, wts=list(wStat=1,wHab=1,wHarv=1,wGenD=1,wES=0.5)), 500)    

compare1 <- rbind(stats_1$stats, # default
                  stats_Hab$stats, # wStat = 0.5
                  stats_Harv$stats, # wHab = 0.5
                  stats_GenD$stats, # wHarv = 0.5
                  stats_ES$stats) # wGenD = 0.5

compare2 <- data.frame(Group=stats_1$bygroup$Group,
                       Default=(stats_1$bygroup$Number/sum(stats_1$bygroup$Number))*100,
                       wStat=(stats_Stat$bygroup$Number/sum(stats_Stat$bygroup$Number))*100,
                       wHab=(stats_Hab$bygroup$Number/sum(stats_Hab$bygroup$Number))*100,
                       wHarv=(stats_Harv$bygroup$Number/sum(stats_Harv$bygroup$Number))*100,
                       wGenD=(stats_GenD$bygroup$Number/sum(stats_GenD$bygroup$Number))*100,
                       wES=(stats_ES$bygroup$Number/sum(stats_ES$bygroup$Number))*100
                           )
compare3 <- compare2
compare3$d_wStat <- compare3$wStat-compare3$Default
compare3$d_wHab <- compare3$wHab-compare3$Default
compare3$d_wHarv <- compare3$wHarv-compare3$Default
compare3$d_wGenD <- compare3$wGenD-compare3$Default
compare3$d_wES <- compare3$wES-compare3$Default
compare3$wStat <- NULL; compare3$wHab <- NULL; compare3$wHarv <- NULL
compare3$wGenD <- NULL; compare3$wES <- NULL; compare3$Default <- NULL
compare3a <- compare3
compare2 <- melt(compare2)
compare3 <- melt(compare3)
levels(compare2$variable) <- c("Default","Status","Habitat","Harvesting","Gen. diversity","ES")
levels(compare3$variable) <- c("Status","Habitat","Harvesting","Gen. diversity","ES")


### CHANGES IN TAXONOMIC GROUPS IN TOP 100, BY CHANGE IN WEIGHT:

#   p <- ggplot(compare2, aes(x=variable, y=value, fill=Group))
#   p <- p + geom_bar(stat="identity", label=c("1","2","3","4","5")) 
#   p <- p + coord_flip()
#   p <- p + scale_fill_brewer(palette="PuOr")
#   p <- p + labs(y="Percentage spp. of each group in Top 500")
#   p <- p + scale_y_continuous()
#   p + opts(
#            title="Effect of decreasing co-benefit weighting by 0.5",
#            plot.title=theme_text(size=20),
#            panel.background=theme_blank(),
#            panel.grid.major=theme_line(colour="grey",size=0.2),
#            panel.grid.minor=theme_line(colour="grey",size=0.2),
#            axis.title.x=theme_text(size=20),
#            axis.title.y=theme_blank(),
#            axis.text.x=theme_text(size=15),
#            axis.text.y=theme_text(size=15),
#            legend.text=theme_text(size=15),
#            legend.title=theme_text(size=15)
#            )

### Figure 5:

  p1 <- ggplot(compare3, aes(y=value, x=Group, fill=Group))
  #p1 <- p1 + geom_bar(fill="lightblue", colour="darkgrey")
  p1 <- p1 + geom_bar()
  p1 <- p1 + scale_fill_brewer(palette="PuOr")
  p1 <- p1 + labs(y="% Change in spp. of each group in Top 500")
  p1 <- p1 + facet_grid(.~variable)
  p1 <- p1 + opts(
                  axis.title.x=theme_blank(),
                  axis.text.x=theme_blank(),
                  axis.ticks=theme_blank(),
                  axis.title.y=theme_text(angle=90,size=18),
                  axis.text.y=theme_text(size=14),
                  legend.text=theme_text(size=14)
                  )
  p1
  # 
  #   p2 <- ggplot(compare3, aes(y=value, x=variable, fill=variable))
  #   #p1 <- p2 + geom_bar(fill="lightblue", colour="darkgrey")
  #   p2 <- p2 + geom_bar(colour="grey")
  #   p2 <- p2 + scale_fill_brewer(palette="PuOr")
  #   p2 <- p2 + labs(y="% Change in spp. of each group in Top 500")
  #   p2 <- p2 + facet_grid(.~Group)
  #   p2 <- p2 + opts(axis.text.x=theme_text(angle=90, hjust=1)
  #                   )
  #   p2

# Mean changes in representation per group:
compare3a


###### UKOTs

setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/')
drv <- dbDriver("SQLite") 
con <- dbConnect(drv, "mapisco.db")

# Number of species in all UKOTs:
sum(list1$ukot,na.rm=T)

# Number of species per UKOT:
dbGetQuery(con, "SELECT country, 
                        count(DISTINCT spid) 
                 FROM country_spid 
                 WHERE country 
                 IN (SELECT country 
                     FROM ukots) 
                 GROUP BY country")

dbDisconnect(con)
setwd('/Users/MacSizedRoon/Documents/docs/MAPISCO/HANDOVER/000_DATA/000_OUTPUT')

all$ukot[is.na(all$ukot)] <- 0
ukot_list <- all[all$ukot==1,]
ukot_list <- calcP_SUM(ukot_list)

# Score for highest ranking species:
round(max(ukot_list$pscore),3)
# Score for 10th species:
round(ukot_list$pscore[10],3)
# First ten species:
temp <- subset(ukot_list, select=c('binomial','vernacular','pscore','Rank'))
temp[1:10,]
# First species of each group etc:
ddply(ukot_list, .(tgroup), summarise, 
      TaxGroupMaxRank=min(as.numeric(as.vector(Rank))), 
      TaxGroupMaxScore=round(max(pscore),3), 
      species=binomial[1])
# Score resolution for full UKOT list, no. of ranks and total no. of species:
round((length(levels(ukot_list$Rank))/nrow(ukot_list))*100,2); length(levels(ukot_list$Rank)); nrow(ukot_list)

# Top 500 statistics for UKOTs:
ukot_top <- topStats(ukot_list, 500)
ukot_top$bygroup

Table7 <- cbind(ddply(ukot_list, .(tgroup), summarise, 
                      TaxGroupMaxRank=min(as.numeric(as.vector(Rank))), 
                      TaxGroupMaxScore=round(max(pscore),3), 
                      species=binomial[1]), 
                topStats(ukot_list, 500)$bygroup)

  # Table A5-3 (top 500 for UKOTs):
TableA5_3 <- hd(ukot_list, 500)
write.csv(TableA5_3, "Table A5-2.csv", row.names=F)

### Extra CB data example: Palms

setwd('/Users/MacSizedRoon/Documents/docs/MAPISCO/HANDOVER/000_DATA/')
palms <- read.csv('000_INPUT/Mapisco palms June 2012.csv',header=T)
names(palms) <- c("rlid","Family","Genus",
                  "Species","Authority","X",
                  "rl_status","Harv","ES","Hab",
                  "rl_crit","rl_crit_ver","year_assessed",
                  "poptrend","source")
palms$binomial <- paste(palms$Genus, palms$Species)

setwd('~/Documents/docs/MAPISCO/HANDOVER/000_Data/')
drv <- dbDriver("SQLite") 
con <- dbConnect(drv, "mapisco.db")
spid <- dbGetQuery(con, "SELECT * FROM spp")
palms$spid <- spid$spid[match(palms$binomial, spid$binomial)]
dbDisconnect(con)
setwd('/Users/MacSizedRoon/Documents/docs/MAPISCO/HANDOVER/000_DATA/000_OUTPUT')
palms$Hab <- palms$Hab/max(palms$Hab)
palms$Harv <- palms$Harv/max(palms$Harv)
palms$ES <- palms$ES/max(palms$ES)

all$palms_Hab <- palms$Hab[match(all$spid,palms$spid)]
all$palms_Harv <- palms$Harv[match(all$spid,palms$spid)]
all$palms_ES <- palms$ES[match(all$spid,palms$spid)]

all_palms <- all

# The number of palm species ID'ed by the Palms SG:
length(unique(palms$binomial))
# The number of selected palm species already assessed on the RL:
sum(palms$binomial %in% spid$binomial)

# Priority calculated as SUM, with PALMS:

all_palms$Stat <- apply(cbind(all_palms$s_status, all_palms$s_srli),1,max_func)/2
all_palms$Hab <- apply(cbind(all_palms$s_aze, all_palms$s_iba, all_palms$palms_Hab),1,sum,na.rm=T)/3
all_palms$Harv <- apply(cbind(all_palms$s_aqc, 
                              all_palms$s_aqc_rl, 
                              all_palms$s_commf, 
                              all_palms$palms_Harv),1,sum,na.rm=T)/4
all_palms$GenD <- apply(cbind(all_palms$s_cwr, all_palms$s_bgci, all_palms$s_doman),1,sum,na.rm=T)/3
all_palms$ES <- apply(cbind(all_palms$s_carfor, all_palms$s_wat, all_palms$palms_ES),1,sum,na.rm=T)/3

temp <- cbind(1*all_palms$Stat, 
              1*all_palms$Hab, 
              1*all_palms$Harv, 
              1*all_palms$GenD, 
              1*all_palms$ES)

all_palms$pscore <- apply(temp, 1, sum, na.rm=T)/5

all_palms <- all_palms[order(all_palms$pscore, decreasing=T),]

all_palms$Rank <- as.factor(all_palms$pscore)
levels(all_palms$Rank) <- seq(length(levels(all_palms$Rank)),1,-1)

# Top 20 list with Palms vs w/o Palms:
hd(list1, 20)
hd(all_palms, 20)

# Mean CB score for Palm species for Hab Cons, BEFORE and AFTER including new data:
mean(list1[list1$binomial %in% palms$binomial,"Hab"])
round(mean(all_palms[all_palms$binomial %in% palms$binomial,"Hab"]),3)

# Mean CB score for Palm species for Harvesting, BEFORE and AFTER including new data:
mean(list1[list1$binomial %in% palms$binomial,"Harv"])
round(mean(all_palms[all_palms$binomial %in% palms$binomial,"Harv"]),3)

# Mean CB score for Palm species for ES, BEFORE and AFTER including new data:
round(mean(list1[list1$binomial %in% palms$binomial,"ES"]),3)
round(mean(all_palms[all_palms$binomial %in% palms$binomial,"ES"]),3)

# Mean Priority Score for these species BEFORE and AFTER including new data:
round(mean(list1[list1$binomial %in% palms$binomial,"pscore"]),3)
round(mean(all_palms[all_palms$binomial %in% palms$binomial,"pscore"]),3)

# Species number one (palm) and its score:
as.vector(all_palms$binomial[1]); round(all_palms$pscore[1],3)
# Previous rank of no. 1 species:
round(as.numeric(as.vector(list1$Rank[list1$binomial==as.vector(all_palms$binomial[1])])),3)
# Species number two (palm) and its score:
as.vector(all_palms$binomial[2]); round(all_palms$pscore[2],3)
# Previous rank of no. 2 species:
round(as.numeric(as.vector(list1$Rank[list1$binomial==as.vector(all_palms$binomial[2])])),3)

palms_topstats <- topStats(all_palms, 500)
palms_topstats$bygroup$PercOld <- topStats(list1, 500)$bygroup$Perc
palms_topstats$bygroup$PercDiff <- palms_topstats$bygroup$Perc-palms_topstats$bygroup$PercOld
palms_topstats$bygroup

# #
# # Priority calculated as AVERAGE, with PALMS:
# 
# list2 <- calcP_AV()
# 
# all_palms$pscore <- NULL
# all_palms$Rank <- NULL
# all_palms$Stat <- apply(cbind(all_palms$s_status, all_palms$s_srli),1,max_func)/2
# all_palms$Hab <- apply(cbind(all_palms$s_aze, all_palms$s_iba, all_palms$palms_Hab),1,mean,na.rm=T)
# all_palms$Harv <- apply(cbind(all_palms$s_aqc, 
#                               all_palms$s_aqc_rl, 
#                               all_palms$s_commf, 
#                               all_palms$palms_Harv),1,mean,na.rm=T)
# all_palms$GenD <- apply(cbind(all_palms$s_cwr, all_palms$s_bgci, all_palms$s_doman),1,mean,na.rm=T)
# all_palms$ES <- apply(cbind(all_palms$s_carfor, all_palms$s_wat, all_palms$palms_ES),1,mean,na.rm=T)
# 
# temp <- cbind(1*all_palms$Stat, 
#               1*all_palms$Hab, 
#               1*all_palms$Harv, 
#               1*all_palms$GenD, 
#               1*all_palms$ES)
# 
# all_palms$pscore <- apply(temp, 1, sum, na.rm=T)/5
# 
# all_palms <- all_palms[order(all_palms$pscore, decreasing=T),]
# 
# all_palms$Rank <- as.factor(all_palms$pscore)
# levels(all_palms$Rank) <- seq(length(levels(all_palms$Rank)),1,-1)
# 
# hd(list2, 20)
# hd(all_palms, 20)






