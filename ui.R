#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#





#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)
library(dplyr)


#read in disease and location name data
disease_names <- read.table("disease_names.txt", header=T)
disease_names <- droplevels(filter(disease_names, x!="P&I MORT"))
infrequent <- read.table("inf_dis.txt", header=T)




shinyUI(navbarPage("Power Optimization Visualization",
                   tabPanel("Line 1 Prediction",
                            
                            # Application title
                            titlePanel("2017 Line 1 Sample data Prediction"), 
                            
                            #The shiny app is made up of two columns: the first column houses all of the user interface,
                            #including commodity selection, location type, plot type, and plot options.  
                            column(4, wellPanel( 
                              # A drop down menu to choose the commodity name of interest.  
                              # Reads in commodity names from commodity_names.txt.  Can choose which name to default to.  
                              selectInput('commodity_name', 'Commodity Name', as.character(levels(commodity_name$x)), selected="SYN"),
                              
                              # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
                              # The checkbox is defined in the server.R file
                              dateRangeInput('years','Choose date range', start= "2017-01-01", end=Sys.Date(), 
                                             min = "2008-01-01", max=Sys.Date() ),
                              
                              uiOutput("location"),
                              
                              
                              #A line break to make the interface clearer 
                              br(),
                              
                              # A row with two columns: one to choose the location type, and one to choose a plot type.
                              fluidRow(
                                column(7, radioButtons("locty", "Location Type",
                                                       c("Pump Station" = "pump_station",
                                                         "Single Segment" = "line_segment",
                                                         "All pump stations within a segment"="line_segments",
                                                         "All segments"="line_segments",
                                                         "Line Total" = "line"), selected="line_segments")
                                ),
                                column(5, radioButtons("plotty", "Plot Type",
                                                       c("Daily data" = "day",
                                                         "Weekly data" = "week",
                                                         "Monthly data" = "month",
                                                         ), selected="week"))
                              ),
                              
                              # A row with some plot options. uiOutput("frees") creates a checkbox for
                              # whether the y-axis scale should be the same for all plots.
                              # This checkbox only appears for certain location type selections, and is defined in the server.R file. 
                              fluidRow(
                                h5(strong('Plot Options')),
                                checkboxInput('alert_line', 'Include alert thresholds (experimental)'),
                                uiOutput("frees")
                              ))
                            ),
                            
                            
                            
                            # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
                            column(8, plotOutput('plot1'))
                   ),
                   
                   
                 
                   
                   tabPanel("More Information",   # Information about data collection.
                            "Data are updated weekly on Thursday at 20:00 CT.",
                            br(),
                            br(),
                            "Please visit", 
                            a("this site", href="http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
                            "for more information on how the data were collected.  All data are provisional.",
                            br(),
                            br(),
                            a("See the code", href="https://github.com/NLMichaud/WeeklyCDCPlot"),
                            br(),
                            br(),
                            "Any questions or comments can be sent to",
                            br(),
                            "Aaron Kite-Powell: " ,
                            a("akitepowell@gmail.com", href="mailto:akitepowell@gmail.com"),
                            br(),
                            "Nick Michaud: ",
                            a("michaud@iastate.edu", href="mailto:michaud@iastate.edu"))
                   
)
)




