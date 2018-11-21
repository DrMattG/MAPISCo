library(RHTMLForms)
library(RCurl)
library(XML)
#forms <- getHTMLFormDescription("http://fishbase.org/search.php")
#str(forms)
#myfunc <- createFunction(forms$IBCI)

setwd('~/Documents/docs/MAPISCO/000_DATA')
countrycodes <- read.csv('FishBase_CountryCodes.csv', header=T)

base <- "http://fishbase.org/Country/CountryChecklist.php"

all_data <- as.data.frame(NULL)
for (i in 1:length(countrycodes[,1])) {
  print(as.character(countrycodes[i,"country"]))
  i_page <- postForm(base, 
                     c_code=as.vector(countrycodes[i,"code"]), 
                     vhabitat="commercial", 
                     trpp="999999")
  i_data <- readHTMLTable(htmlParse(i_page, asText=T))[[4]]
  
  if(!is.null(i_data)) {
    names(i_data) <- as.vector(unlist(i_data[1,]))
    i_data <- i_data[2:length(i_data[,1]),]
    i_data$Country <- as.character(countrycodes[i, "country"])
    all_data <- rbind(all_data, i_data)    
  }
}


posted <- postForm(base, c_code="608", vhabitat="commercial")
posted <- postForm(base, c_code="608", vhabitat="commercial", trpp="1000000")

extr_data <- readHTMLTable(htmlParse(posted, asText=T))