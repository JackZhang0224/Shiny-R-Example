#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



#load library
library(shiny)
library(dplyr)

# power data

#read in commodity and location name data
commodity_names <- read.table("commodity_names.txt", header=T)

#line <- rxImport(RxXdfData("Line_both_scored.xdf"),reportProgress = 0)
location_names_power <- read.table("location_names_power.txt", header=T,stringsAsFactors = FALSE)


# cancellation data

# ts_input_adj


shinyUI(navbarPage("2017 Project Visualization",
                   tabPanel("Line Prediction",
                            
                            # Application title
                            titlePanel("2017 Line Prediction"), 
                            
                            #The shiny app is made up of two columns: the first column houses all of the user interface,
                            #including commodity selection, location type.  
                            column(4, wellPanel( 
                              # A drop down menu to choose the commodity name of interest.  
                              # Reads in commodity names from commodity_names.txt.  Can choose which name to default to.  
                            
                              selectInput('commodity_name', 'Commodity Name', as.character(levels(commodity_names$commodity)), selected="SYN"),
                              
                              # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
                              dateRangeInput('years','Choose date range', start= "2017-01-01", end="2017-05-01", 
                                             min = "2017-01-01", max="2017-05-01" ),
                              
                              uiOutput("location"),
                              
                              
                              #A line break to make the interface clearer 
                              br(),
                              
                              # A row with one column: choose the location type
                              fluidRow(
                                column(7, radioButtons("locty", "Location Type",
                                                       c("Pump Station" = "pump_station",
                                                         "Single Segment" = "line_segment",
                                                         "All pump stations within a segment"="psline_segments",
                                                         "All segments"="aline_segments",
                                                         "Line" = "line"), selected="aline_segmpsents")
                                )
                              )
                            )  
                            ),
                            
                            
                            
                            # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
                            column(8, plotOutput('plot1'))
                   ),
                   
                   
 #############################################################################                  
                   
                   tabPanel("Cancellation Prediction",
                            
                            # Application title
                            titlePanel("2017 Cancellation Prediction"), 
                            
                            #The shiny app is made up of two columns: the first column houses all of the user interface,
                            #including only accumulated month selection.  
                            column(4, wellPanel( 
                              # A drop down menu to choose the commodity name of interest.  
                              # Reads in commodity names from commodity_names.txt.  Can choose which name to default to.  
  
                              
                              # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
                              dateRangeInput('yearsP','Choose date range', start= "2014-01-01", end="2017-11-01", 
                                             min = "2014-01-01", max="2017-11-01" ),
                              
                              uiOutput("locationP"),
                              
                              
                              #A line break to make the interface clearer 
                              br(),
                              
                              # A row with one column: choose the location type
                              fluidRow(
                                column(7, radioButtons("loctyP", "Report Type",
                                                       c("Cancellation Forecast" = "Cancellation",
                                                         "Seasonality Forecast Line 4" = "Seasonality_L4",
                                                         "Seasonality Forecast Line 67" = "Seasonality_L67"), selected="Cancellation")
                                )
                              )
                            )  
                            ),
                            
                            
                            
                            # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
                            column(8, plotOutput('plot2'),
                                   br(),
                                   # create a output table
                                   fluidRow( column(4, tableOutput('table2'))))
                            
                            
                    
                            
                   ),
                   
                 
                   
  ###########################################################################                 
                   
                   
                   tabPanel("More Information",   # Information about data collection.
                            br(),
                            br(),
                            "Please visit our ", 
                            a("GitHub Repository", href="http://enbze2elpde-ghe.eastus2.cloudapp.azure.com/Enbridge/2017-Power-Optimization"),
                            " for more project information.",
                            br(),
                            "If you have any questions or comments, please send an email to",
                            br(),
                            "Jack Zhang: " ,
                            a("jack.zhang@enbridge.com", href="mailto:jack.zhang@enbridge.com")
                   )
                   
)
)




