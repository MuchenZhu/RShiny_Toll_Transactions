#---Muchen Zhu Individual Project Data Preparation---
setwd("~/Desktop/GWU 2017 fall semester/programming-class/individualproject/Toll_Transactions") #Set my wording directory to the folder of shiny app
library("tidyr") # Load these two libraries for data wrangling
library("dplyr")
library("leaflet")

mydf <- read.csv("Toll_Transactions_wrangled.csv") # Read in the csv file wrangled by Trifecta

# Seperate the original columns into columns I need and check the data type:
mydf <- separate(mydf, Location, c("lat","long"), sep = ",") # Separate the lattitude and longitude in Location column into two columns named lat and long
mydf <- separate(mydf, Date, c('Date','Time'), sep = " ") # Separate the date and time in Date column into two columns
mydf <- mydf[-2] # Drop the Time column
mydf$Date <- as.Date(mydf$Date,"%m/%d/%Y") # Set the data type to Date for column Date
mydf$lat <- as.numeric(mydf$lat) # Set the data type to numeric for column lat
mydf$long <- as.numeric(mydf$long)  # Set the data type to numeric for column long
str(mydf) # Check if all columns are in the correct data type

# There are some typos and values that do not make sense, let's drop those rows:
mydf <- filter(mydf, mydf$Total_Transactions>=mydf$Transponder_Transactions) # Choose only the rows that the value in Total_Transactions is larger than the value in Transponder_Transactions  
mydf <- filter(mydf, mydf$Facility == 'BHT' | mydf$Facility =='FMT' | mydf$Facility =='FSK'
               | mydf$Facility =='HMB' | mydf$Facility =='JFK' | mydf$Facility =='BAY') #Choose only the rows with correct abbreviations of the facilities
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]} # Create a function that return the mode of a column

unique(mydf$Facility) # subset mydf into 6 subsets of each facility
mydf1 <- filter(mydf,mydf$Facility == 'BHT')
mydf2 <- filter(mydf,mydf$Facility == 'FMT')
mydf3 <- filter(mydf,mydf$Facility == 'FSK')
mydf4 <- filter(mydf,mydf$Facility == 'HMB')
mydf5 <- filter(mydf,mydf$Facility == 'JFK')
mydf6 <- filter(mydf,mydf$Facility == 'BAY')

# Keep the rows where the value of lat and long match the mode of lat and long of each facility
mydf <- filter(mydf, (mydf$Facility == 'BHT' & mydf$lat == Mode(mydf1$lat)) | (mydf$Facility == 'FMT' & mydf$lat == Mode(mydf2$lat)) | (mydf$Facility == 'FSK' & mydf$lat == Mode(mydf3$lat))
               | (mydf$Facility == 'HMB' & mydf$lat == Mode(mydf4$lat)) | (mydf$Facility == 'JFK' & mydf$lat == Mode(mydf5$lat)) | (mydf$Facility == 'BAY' & mydf$lat == Mode(mydf6$lat)))
mydf <- filter(mydf, (mydf$Facility == 'BHT' & mydf$long == Mode(mydf1$long)) | (mydf$Facility == 'FMT' & mydf$long == Mode(mydf2$long)) | (mydf$Facility == 'FSK' & mydf$long == Mode(mydf3$long))
                | (mydf$Facility == 'HMB' & mydf$long == Mode(mydf4$long)) | (mydf$Facility == 'JFK' & mydf$long == Mode(mydf5$long)) | (mydf$Facility == 'BAY' & mydf$long == Mode(mydf6$long)))

#--Shiny---
library(shiny) # Load the packages needed
library(leaflet)

attach(mydf) # attach the dataset

function(input, output) {
  
  output$facility <- renderPrint({ input$radio }) # Show the abbreviation of the chosen facility
  
  output$Descriptive_Data <- renderPrint({
    mydf <- filter(mydf, mydf$Facility == input$radio)
    summary(mydf, digits = 9) # Show the summary of the data according to the selected facility
  }) 
  
  output$plot1 <- renderPlot({
    mydf <- filter(mydf, mydf$Facility == input$radio)
    mydf <- separate(mydf, Date, c('Y','M','D')) # Separate the Date columns into 3 columns of year, month and day
    plot(mydf$Y,mydf$Total_Transactions, xlab = "Year", ylab = "Total Transactions") }) # Year as the x-axis and Total_Transactions as the Y-axis, plot a graph based on the selected facility
  
  output$plot2 <- renderPlot({
    mydf <- filter(mydf, mydf$Facility == input$radio)
    mydf <- separate(mydf, Date, c('Y','M','D'))
    plot(mydf$Y,mydf$Transponder_Transactions, xlab = "Year", ylab = "Transporder Transactions")}) # Same as the first graph but switching the y-axis to Transporder Transactions
  
  output$plot3 <- renderPlot({
    mydf <- filter(mydf, mydf$Facility == input$radio)
    mydf <- separate(mydf, Date, c('Y','M','D'))
    plot(mydf$Y,mydf$Percentage_of_Transponder_Transactions, xlab = "Year", ylab = "Percentage of Transporder Transactions")}) # Same as the first graph but switching the y-axis to Percentage of Transporder Transactions
  
  output$leaf <- renderLeaflet({
    ll <- mydf[,c(6,7)] # a dataset with only the lat and long values
    mydf <- filter(mydf, mydf$Facility == input$radio)
    leaflet() %>% # leaflet function
      addTiles() %>% # Add default OpenStreetMap map tiles
      setView(lng=mean(mydf$long),lat=mean(mydf$lat),zoom=16) %>% # set the starting view to the location of the toll facility
      addMarkers(data = ll) %>% # add markers of where the 6 facilities are
      addMarkers( # add popups including the name, abbreviation, lattitude and longitude for the 6 facilities
        lng = -76.58716, lat = 39.24061,
        popup = "Baltimore Harbor Tunnel-BHT, (-76.58716, 39.24061)") %>% 
      addMarkers( 
        lng = -76.56158, lat = 39.2661,
        popup = "Fort McHenry Tunnel-FMT, (-76.56158, 39.2661)")  %>% 
      addMarkers( 
        lng = -76.51066, lat = 39.23049,
        popup = "Francis Scott Key Memorial Bridge-FSK, (-76.51066, 39.23049)")  %>% 
      addMarkers( 
        lng = -76.07306, lat = 39.56781,
        popup = "Thomas J. Hatem Memorial Bridge-HMB, (-76.07306, 39.56781)")  %>% 
      addMarkers( 
        lng = -76.07285, lat = 39.58925,
        popup = "John F. Kennedy Memorial Highway-JFK, (-76.07285, 39.58925)")  %>% 
      addMarkers( 
        lng = -76.40672, lat = 39.01338,
        popup = "William Preston Lane Memorial Bridge-BAY, (-76.40672,39.01338)")
    }) 
}
