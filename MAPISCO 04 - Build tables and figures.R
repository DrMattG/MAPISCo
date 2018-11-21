### MAPISCO BUILD FIGURES & TABLES PHASE 1 REPORT

### Table 1:

temp <- as.factor(as.vector(all$fb_comm_overall[!all$fb_comm_overall==""]))
temp <- data.frame(table(temp))
names(temp) <- c("Category","No. spp.")
temp$perc <- round(temp[,2]/sum(temp[,2])*100,1)
temp <- temp[c(2,1,3,6,5,4),]
temp$score <- seq(6,1,-1)
names(temp)[3] <- "Percentage"
names(temp)[4] <- "Category score"
temp$Definition <- c("The species is very important to the capture fisheries (or aquaculture) of a country",
                     "The species is regularly taken in the capture fisheries or regularly found in aquaculture activities of a country",
                     "The species is of comparatively less importance in capture fisheries or aquaculture in a given country",
                     "The species is consumed locally only, mostly by the fishers themselves",
                     "", "")
Table1 <- temp
write.csv(Table1, "Table 1.csv", row.names=F)

### Table 2:

temp <- as.factor(as.vector(all$aculture_rl[!all$aculture_rl==""]))
temp <- data.frame(table(temp))
names(temp) <- c("Category","No. spp.")
temp$perc <- round(temp[,2]/sum(temp[,2])*100,1)
temp <- temp[c(1,3,2),]
temp$score <- c(3,2,1)
names(temp)[3] <- "Percentage"
names(temp)[4] <- "Category score"
Table2 <- temp

### Table 3:

temp <- as.factor(as.vector(all$status[!all$status==""]))
temp <- data.frame(table(temp))
names(temp) <- c("Category code","No. spp.")
temp$perc <- round(temp[,2]/sum(temp[,2])*100,1)
temp <- temp[c(1,4,3,11,10,2,7,8,9,6,5),]
temp$Category <- c("Critically Endangered",
                   "Extinct in the Wild",
                   "Endangered",
                   "Vulnerable",
                   "Near Threatened",
                   "Data Deficient",
                   "Lower Risk",
                   "Lower Risk",
                   "Lower Risk",
                   "Least Concern",
                   "Extinct")
temp$score <- c(9,8,7,6,5,4,3,3,3,2,1)
Table3 <- temp
write.csv(Table3, "Table 3.csv", row.names=F)

### Table 4:
temp <- as.factor(as.vector(all$srli_status[!all$srli_status==""]))
temp <- data.frame(table(temp))
names(temp) <- c("Category code","No. spp.")
temp$perc <- round(temp[,2]/sum(temp[,2])*100,1)
temp <- temp[c(1,3,7,6,2,4,5),]
temp$Category <- c("Critically Endangered",
                   "Endangered",
                   "Vulnerable",
                   "Near Threatened",
                   "Data Deficient",
                   "Least Concern",
                   "Not Evaluated")
temp$score <- c(9,7,6,5,4,2,1)
Table4 <- temp


