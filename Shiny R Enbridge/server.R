#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(grid)

## Sometimes as different options are chosen, Shiny tries to create a plot without first reading in new user input, 
## which can cause ggplot to throw an error.  For that reason, there's a lot of code like if(is.null(input$locty)) return()
## This code checks to see if the correct input has been read yet, and if not, it prevents ggplot from trying to plot anything. 


###########################################################




shinyServer(function(input, output, session) {
  
  # The first reactive element of the UI is a drop down menu which filters locations based on whether
  # the user has selected pump stations, line segments, pump stations in line segments, line
  # if "all segment" is selected, no choices are displayed
 
  
  
  
  ### create selection for power
   output$location <- renderUI({     
    if(is.null(input$locty))return()
    switch(input$locty,
           "pump_station" =     return(selectInput('location_name_power', 'Pump Station Name',sort(filter(location_names_power, type=="pump_station")$location))),
           "psline_segments" =  return(selectInput('location_name_power', 'Line Segment Name', sort(filter(location_names_power, type=="line_segment")$location))),
           "line_segment" =  return(selectInput('location_name_power', 'Line Segment Name', sort(filter(location_names_power, type=="line_segment")$location))),
           "line" =  return(selectInput('location_name_power', 'Line', sort(filter(location_names_power, type=="line")$location)))
  )
  })
  
  
   
   #### data selection for power visulization

  # We select data to plot based on which location type and location was chosen.  
  # The reactive function filters the data to return only rows from line1 data which correspond to either the pump_station,
  # line_segment, or line selected.  We also need to put in extra error check for the " pump_stations with a line segment" option to prevent ggplot error message 
  selectedData <- reactive({
    
    if(input$locty=="aline_segments") return(filter(line, commodity == input$commodity_name, line_segment %in% location_names_power$location[which(location_names_power$type=="line_segment")],date >= input$years[1], date<=input$years[2] ))
  
    if(is.null(input$location_name_power))return()
    
    
    if(input$locty=="pump_station") return(filter(line, commodity == input$commodity_name, pump_station == input$location_name_power,date >= input$years[1], date<=input$years[2] ))

    if(input$locty=="line_segment") return(filter(line, commodity == input$commodity_name, line_segment == input$location_name_power,date >= input$years[1], date<=input$years[2] ))
    
    if(input$locty=="line") return(filter(line, commodity == input$commodity_name, line == input$location_name_power,date >= input$years[1], date<=input$years[2] ))
    
    
      if(input$locty=="psline_segments"){
      if(!(input$location_name_power %in% location_names_power$line_segment)){return()}
      return(filter(line, commodity == input$commodity_name, line_segment == input$location_name_power,date >= input$years[1], date<=input$years[2] ))
        }
  })
  
  
  
  #### data selection for cancellation & Seasonality visulization
  
  
  selectedDataI <- reactive({
    
    if(input$loctyP=="Cancellation") return(filter(ts_input_adj,NOS_MONTH >= "2014-01-01",NOS_MONTH<=input$yearsP[2] ))
    
    if(input$loctyP=="Seasonality_L4") return(filter(Seasonal_input1,Date >= "2015-01-01", Date<=input$yearsP[2] ))
    
    if(input$loctyP=="Seasonality_L67") return(filter(Seasonal_input1,Date >= "2015-01-01", Date<=input$yearsP[2] ))
    
   })
  
  
  
  
  #######################plot for power##################
  
  # Plot data -  either a single plot for one location, or faceted plots for all locations of a single type  
  output$plot1 <- renderPlot({
    if(is.null(input$locty)||is.null(selectedData()))return()    
    scaletype = "fixed"
    
    
    # Create the main ggplot
    p <- ggplot(selectedData(), aes(x = ts_pi))+geom_line(aes(y = hhpu))+ geom_line(aes(y = hhpu_Pred),color = "red") +ggtitle(paste("Predicted HHPU VS Actual HHPU Report for Commodity ",input$commodity_name))
    
    if(input$locty=="pump_station"||input$locty=="line_segment"||input$locty=="line") return(p)
    
    
    if(input$locty=="psline_segments") 
      return(p+ facet_wrap(~ pump_station)+theme(panel.spacing = unit(1, "lines")))
    
    
    if(input$locty=="aline_segments") 
      return(p+ facet_wrap(~ line_segment)+theme(panel.spacing = unit(1, "lines")))
    
  })
  
  
  #######################plot for cancellation/seasonality##################
  
  # Plot data - for cancellation or seasonality
  
  output$plot2 <- renderPlot({
    
  if(is.null(input$loctyP)||is.null(selectedDataI()))return()    
  scaletype = "fixed"
    
    
  if(input$loctyP=="Cancellation"){
      
  n <- nrow(selectedDataI())
      
  ts_data_adj <- selectedDataI()$adj_delta_batches[1:n]
      
  ts_server_input <- ts(ts_data_adj,frequency = 12,start = c(2014,01))
      
  plot.forecast(forecast(auto.arima(ts_server_input),h=2))
  }
    
    
  if(input$loctyP=="Seasonality_L4"){
      
    n <- nrow(selectedDataI())
      
    ts_seasonality_adj <- selectedDataI()$Line4[1:n]
      
    ts_seasonality_input <- ts(ts_seasonality_adj,frequency = 12,start = c(2015,01))
      
    plot.forecast(forecast(auto.arima(ts_seasonality_input),h=2))
    
  
  }
    
  
  if(input$loctyP=="Seasonality_L67"){
    
    n <- nrow(selectedDataI())
    
    ts_seasonality_adj1 <- selectedDataI()$Line67[1:n]
    
    ts_seasonality_input1 <- ts(ts_seasonality_adj1,frequency = 12,start = c(2015,01))
    
    plot.forecast(forecast(auto.arima(ts_seasonality_input1),h=2))
  }
  
  

    
   })
  
  
  
  output$table2 <- renderTable({
    
    if(is.null(input$loctyP)||is.null(selectedDataI()))return()    
    scaletype = "fixed"
    
    
    if(input$loctyP=="Cancellation"){
      
      n <- nrow(selectedDataI())
      
      ts_data_adj <- selectedDataI()$adj_delta_batches[1:n]
      
      ts_server_input <- ts(ts_data_adj,frequency = 12,start = c(2014,01))
      
      t1 <- data.frame(forecast(auto.arima(ts_server_input), h=2))
      
      return(t1) 
    }
    
    
    if(input$loctyP=="Seasonality_L4"){
      
      n <- nrow(selectedDataI())
      
      ts_seasonality_adj <- selectedDataI()$Line4[1:n]
      
      ts_seasonality_input <- ts(ts_seasonality_adj,frequency = 12,start = c(2015,01))
      
      t2 <- data.frame(forecast(auto.arima(ts_seasonality_input), h=2))
      
      return(t2) 
    }
    
    
    if(input$loctyP=="Seasonality_L67"){
      
      n <- nrow(selectedDataI())
      
      ts_seasonality_adj1 <- selectedDataI()$Line67[1:n]
      
      ts_seasonality_input1 <- ts(ts_seasonality_adj1,frequency = 12,start = c(2015,01))
      
      t3 <- data.frame(forecast(auto.arima(ts_seasonality_input1), h=2))
      
      return(t3) 
    }
    
    
  }) 
  
  

  
  
  
  
  
  

  
})


