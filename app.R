#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Libraries
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(dslabs)
#library(plotly)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(zoo)
library(rsconnect)
#library(forecast)

# data
ClimateData <- read.csv("https://raw.githubusercontent.com/atkinsjeff/ALTA/main/data/ClimateNormals.csv")
temp <- read.csv("https://raw.githubusercontent.com/atkinsjeff/ALTA/main/data/ACGTemp.csv")
# temp <- read.csv("./data/ACGTemp.csv")
temp$Year <- as.integer(temp$Year)
temp$date <- as.Date(temp$date)

# this is taken from:  https://library.virginia.edu/data/articles/getting-started-with-shiny
ui <- fluidPage(
  
  #titlePanel("Costa Rica ACG Climate Dashboard"),
  navbarPage("Costa Rica ACG Project",
             
             # Site Descriptions
             tabPanel("ACG Sites",
                      sidebarLayout(
                        sidebarPanel(
                          # inputs
                          selectizeInput("siteInput", "ACG Site ID",
                                         choices = unique(temp$siteID),  
                                         selected="Maritza", multiple = FALSE)
                        ),
                        mainPanel(
                          p("This Climate Dashboard app, powered by R Shiny, includes modeled 
              air temperature and precipitaion data that has been clipped to the 
              country of Costa Rica. Plots from the ACG project have then been
              interesected with those data. Users can selected ACG sites via the 
              sidebar and view and manipulate the temperature and precipitation data.
              Select the tabs above for further information. ")
                        )    
                      )
             ),
             # Temperature Panel
             tabPanel("Temperature",
                      sidebarLayout(
                        sidebarPanel(
                          # inputs
                          selectizeInput("siteInput", "ACG Site ID",
                                         choices = unique(temp$siteID),  
                                         selected="Maritza", multiple = FALSE),
                          checkboxGroupInput("climInput", "Climate Variable",
                                             choices = c("Tmin",
                                                         "Tmax",
                                                         "Tavg"),
                                             selected = c("Tavg")),
                          radioButtons("timeAvg", label = h4("Time Averaging Period"),
                                       choices = list("Annual" = "annual", 
                                                      "None" = ""),
                                       selected = ""),
                          
                          sliderInput("yearInput", "Year", min = min(temp$Year), max = max(temp$Year),
                                      value= range(temp$Year), sep = ""),
                          
                          radioButtons("trend", label = h4("Trendline"),
                                       choices = list("Add Trendline" = "lm", "Remove Trendline" = ""),
                                       selected = "")
                        ),  
                        mainPanel(
                          plotOutput("tempplot"),
                          br(), br(),
                          plotOutput("climateStripes"),
                          br(), br(),
                          plotOutput("distplot")
                        )
                      )
             ),
             
             #Precipitation
             tabPanel("Precipitation",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectizeInput("siteInput", "ACG Site ID",
                                         choices = unique(temp$siteID),  
                                         selected="Maritza", multiple = FALSE),
                          # radioButtons("timeAvg", label = h4("Time Averaging Period"),
                          #               choices = list("Annual" = "annual", "None" = ""),
                          #               selected = ""),
                          sliderInput("yearInput", "Year", min = min(temp$Year), max = max(temp$Year),
                                      value= range(temp$Year), sep = ""),
                          radioButtons("trendPrecip", label = h4("Trendline"),
                                       choices = list("Add Trendline" = "lm", "Remove Trendline" = ""),
                                       selected = "")
                        ),
                        mainPanel(
                          plotOutput("precipplot"),
                          br(), br(),
                          plotOutput("precipdistplot")
                          
                        )
                      )
                      
             )
             
  )    
)



########## SERVER
server <- function(input, output) {
  
  
  # TEMPERATURE
  # this filters the data to what is needed based on the input() from the ui
  d <- reactive({
    # set filter dates
    min_date <- input$yearInput[1]
    max_date <- input$yearInput[2]
    
    # sorting and doing time avg
    temp %>%
      dplyr::filter(siteID == input$siteInput,
                    var %in% input$climInput,
                    Year >= min_date & Year <= max_date) %>%
      {
        if (input$timeAvg == "") {
          .
        } else if (input$timeAvg == "annual") {
          {.} %>%
            group_by(siteID, var, Year) %>%
            summarize(value = mean(value, na.rm = TRUE), .groups = 'drop')
        }
      }
  })
  
  
  # PRECIPITATION
  # this filters the data to what is needed based on the input() from the ui
  e <- reactive({
    # set filter dates
    min_date <- input$yearInput[1]
    max_date <- input$yearInput[2]
    
    # sorting and doing time avg
    temp %>%
      select(siteID, date, Year, Month, P) %>%
      dplyr::filter(siteID == input$siteInput,
                    Year >= min_date & Year <= max_date) %>%
      {
        if (input$timeAvg == "") {
          .
        } else if (input$timeAvg == "annual") {
          {.} %>%
            mutate(Year = as.factor(Year)) %>%
            group_by(siteID, Year) %>%
            summarize(P = sum(P, na.rm = TRUE), .groups = 'drop') %>%
            data.frame()
        }
      }
  })
  
  f <- reactive({
    # set filter dates
    min_date <- input$yearInput[1]
    max_date <- input$yearInput[2]
    
    # sorting and doing time avg
    ClimateData %>%
      dplyr::filter(siteID == input$siteInput,
                    Year >= min_date & Year <= max_date)
    
    
  })
  ## PLOTTING
  
  
  # TEMPERATURE
  output$tempplot <- renderPlot({
    
    # filter on trendline
    if (input$timeAvg == "annual"){
      
      if (input$trend == 'lm'){
        ggplot(d(), aes(x = Year, y = value, col = var)) +
          geom_line(alpha = 0.5)+
          theme_bw()+
          xlab("Year")+
          ylab("Temp")+
          stat_smooth(method = lm, se = FALSE)+
          ggtitle("Climate Variables Over Time")
        
      } else {
        ggplot(d(), aes(x = Year, y = value, col = var)) +
          geom_line()+
          theme_bw()+
          xlab("Year")+
          ylab("Temp")+
          #geom_smooth(method = "loess", color = var)+
          ggtitle("Climate Variables Over Time")
      }  
    } else {
      if (input$trend == 'lm'){
        ggplot(d(), aes(x = date, y = value, col = var)) +
          geom_line(alpha = 0.5)+
          theme_bw()+
          xlab("Date")+
          ylab("Temp")+
          stat_smooth(method = lm, se = FALSE)+
          ggtitle("Climate Variables Over Time")
        
      } else {
        ggplot(d(), aes(x = date, y = value, col = var)) +
          geom_line()+
          theme_bw()+
          xlab("Date")+
          ylab("Temp")+
          #geom_smooth(method = "loess", color = var)+
          ggtitle("Climate Variables Over Time")
        
      }
    }
  })
  
  output$climateStripes <- renderPlot({
    
    # color palette
    col_strip <- RColorBrewer::brewer.pal(11, "RdBu")
    
    #climate strip
    ggplot(f(), aes(x = Year, y = 1, fill = TAnom))+
      geom_tile(show.legend = FALSE)+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_gradientn(colors = rev(col_strip))+
      theme_void()
    
    
    
    
  })
  
  output$distplot <- renderPlot({
    
    # ggplot(f(), aes(x = as.factor(Year), y = value))+
    #   geom_boxplot(color = "black", fill = "darkgreen")+
    #   theme_classic()+
    #   xlab("")+
    #   ylab("Mean of Temp Variable")
    ggplot(f(), aes(x = Year, y = TAnom, fill = TempSign))+
      geom_col(position = "identity", colour = "black", size = 1) +
      scale_fill_manual(values = c("dodgerblue", "red"), guide = "none")+
      theme_minimal()+
      ylab("Temperature Anomaly [Degree C]")+
      xlab("Year")
    
    
  })
  
  # PRECIPITATION
  output$precipplot <- renderPlot({
    
    # Trying to include trendline, but it doesn't want to
    if (input$trendPrecip == 'lm'){
      ggplot(f(), aes(x = Year, y = P)) +
        geom_col(color = "black", fill = "darkblue")+
        theme_bw()+
        xlab("Year")+
        ylab("P [mm]")+
        stat_smooth(method = lm, se = FALSE)+
        ggtitle("Change")
      
    } else {
      ggplot(f(), aes(x = Year, y = P)) +
        geom_col(color = "black", fill = "darkblue")+
        theme_bw()+
        xlab("Year")+
        ylab("P [mm]")+
        #geom_smooth(method = "loess", color = var)+
        ggtitle("Annual Precipitation")
    }  
  })
  
  
  output$precipdistplot <- renderPlot({
    
    # ggplot2 boxplot of Precipitation Data
    ggplot(e(), aes(x = as.factor(Month), y = P))+
      geom_boxplot(fill = "darkorange")+
      theme_classic()+
      xlab("Month of Year")+
      ylab("P [mm]")+
      ggtitle("Monthly Precipitation Means Over Selected Time Period")
    
  })
  
  
}

shinyApp(ui=ui, server=server)



