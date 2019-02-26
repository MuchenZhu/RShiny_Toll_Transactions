# The dataset is download from https://data.maryland.gov/Transportation/Toll-Transactions/hrir-ejvj
library(shiny)

fluidPage(
   titlePanel("Toll Transactions in MD"), # Add a title panel and show "Toll Transactions in MD" in the panel
   sidebarLayout( # Design the layout of the shiny app
     sidebarPanel( # # Add a side bar panel
       # add a radio button widget and call it Facility, the choices(buttons) are shown in the list
       radioButtons("radio", label = h3("Facility"), 
                    choices = list('Baltimore Harbor Tunnel' = "BHT", 'Fort McHenry Tunnel' = "FMT", 
                                  'Francis Scott Key Memorial Bridge' = "FSK",'Thomas J. Hatem Memorial Bridge'= 'HMB', 
                                  'John F. Kennedy Memorial Highway' = 'JFK', 'William Preston Lane Memorial Bridge' = 'BAY')),
       # below the buttons, add a row to display the selected facility
       fluidRow(column(12, verbatimTextOutput("facility")))),
     mainPanel( # add a main panel
       # the main panel contains 4 tabs, the first tab shows the summany of the data according to the selected facility
       # the second tab shows a plot of the total transactions by year
       # the third tab shows a plot of the transporder transactions by year
       # the fourth tab shows a map (with 6 markers of the 6 facilities and starting view set at the chosen facility)
       tabsetPanel(tabPanel("Summary",verbatimTextOutput("Descriptive_Data")),
                   tabPanel("Plot_total_transaction_by_year", plotOutput("plot1")),
                   tabPanel("Plot_transporder_transaction_by_year", plotOutput("plot2")),
                   tabPanel("Plot_percentage_transporder_transaction_by_year", plotOutput("plot3")),
                   tabPanel("leaflet_facility",leafletOutput("leaf"))))
   )
)