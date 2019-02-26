#---Muchen Zhu Individual Project Data Preparation---
setwd("~/Desktop/programming-class/individualproject/Toll_Transactions")
library("tidyr")
library("dplyr")

mydf <- read.csv("Toll_Transactions_wrangled.csv")
mydf <- separate(mydf, Location, c("lat","long"), sep = ",")
mydf <- separate(mydf, Date, c('Date','Time'), sep = " ")
mydf <- mydf[-2]

mydf$Date <- as.Date(mydf$Date,"%m/%d/%Y")
mydf$lat <- as.numeric(mydf$lat)
mydf$long <- as.numeric(mydf$long)
str(mydf)

mydf <- filter(mydf, mydf$Total_Transactions>=mydf$Transponder_Transactions)
mydf <- filter(mydf, mydf$Facility == 'BHT' | mydf$Facility =='FMT' | mydf$Facility =='FSK'
               | mydf$Facility =='HMB' | mydf$Facility =='JFK' | mydf$Facility =='BAY')

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
unique(mydf$Facility)
mydf1 <- filter(mydf,mydf$Facility == 'BHT')
mydf2 <- filter(mydf,mydf$Facility == 'FMT')
mydf3 <- filter(mydf,mydf$Facility == 'FSK')
mydf4 <- filter(mydf,mydf$Facility == 'HMB')
mydf5 <- filter(mydf,mydf$Facility == 'JFK')
mydf6 <- filter(mydf,mydf$Facility == 'BAY')

Mode(mydf1$lat)
Mode(mydf1$long)

mydf <- filter(mydf, mydf$lat == Mode(mydf1$lat) | mydf$lat == Mode(mydf2$lat) | mydf$lat == Mode(mydf3$lat)
               | mydf$lat == Mode(mydf4$lat) | mydf$lat == Mode(mydf5$lat) | mydf$lat == Mode(mydf6$lat))

mydf <- filter(mydf, mydf$long == Mode(mydf1$long) | mydf$long == Mode(mydf2$long) | mydf$long == Mode(mydf3$long)
               | mydf$long == Mode(mydf4$long) | mydf$long == Mode(mydf5$long) | mydf$long == Mode(mydf6$long))



