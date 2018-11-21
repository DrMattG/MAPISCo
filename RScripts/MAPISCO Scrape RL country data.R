### LOAD RL ID'S FROM RL_STATUS TABLE:
library(XML)
i1a <- 1
i1b <- 100

lookup <- rl_status$rlid[i1a:i1b]

sp_country <- as.data.frame(NULL)
for (i in 1:length(lookup)) {
  if(inherits(try ( { lookup_i <- htmlParse(paste("http://api.iucnredlist.org/details/",lookup[i],"/0",sep="")) }, silent=T),"try-error")) 
  {
    sp_country <- rbind(sp_country, data.frame(rlid=lookup[i],country="COULD NOT FIND ID"))
  } else {
    #lookup_i <- htmlParse(paste("http://api.iucnredlist.org/details/",lookup[i],"/0",sep=""))
    country_i <- xpathSApply(lookup_i, '//ul[@class="countries"]', xmlValue)
    if (length(country_i)>0) {
      country_i <- unlist(strsplit(country_i, "\n"))
      sp_country <- rbind(sp_country, data.frame(rlid=rep(lookup[i],length(country_i)),country=country_i))
    }    
  }
  print(paste(i, " - ", lookup[i]))
}
sp_country10 <- sp_country
write.csv(sp_country10, 'sp_country10.csv', row.names=F)

sp_country <- rbind(sp_country1, sp_country2, sp_country3, sp_country4, sp_country5, sp_country6, sp_country7, sp_country8, sp_country9, sp_country10)

temp <- levels(sp_country$country)
write.csv(temp, '000_INPUT/RL COUNTRY LIST.csv', row.names=F, na="")

sp_country$country[sp_country$country==temp[56]] <- "Cote d' Ivoire"
sp_country$country[sp_country$country==temp[178]] <- "Reunion"
sp_country$country[sp_country$country==temp[179]] <- "Saint Barthelemy"
sp_country$country[sp_country$country==temp[187]] <- "Sao Tome and Principe"
sp_country$country[sp_country$country==temp[241]] <- "Oland Islands"

sp_country$country <- as.factor(sp_country$country)
temp <- levels(sp_country$country)
write.csv(temp, '000_INPUT/RL COUNTRY LIST.csv', row.names=F, na="")

write.csv(sp_country, '000_INPUT/RL SPECIES BY COUNTRY.csv', row.names=F, na="")
