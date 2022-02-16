
###################
##  MILESTONE 4  ##
###################

library(tidyverse)
library(readr)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(shiny)

# CCIHE2018-PublicData found in WEEK 4
# Most_Recent_Cohorts_All_Data_Elements.RData found in WEEK 3
Data <- read_excel(path = "C:/Users/4eliz/Downloads/UCLA 2019 - 2020/2020-2021/2021 08 Spring/STATS 20/Team Project/CCIHE2018-PublicData (1).xlsx", sheet = "Data")
load("C:\\Users\\4eliz\\Downloads\\Most_Recent_Cohorts_All_Data_Elements.RData")

miniBigData <- Most_Recent_Cohorts_All_Data_Elements[c(1,4,22,23)]
names(miniBigData)[2] <- "NAME"
combinedData <- inner_join(Data, miniBigData, by = c("UNITID", "NAME")) # merge initial data with longitude/latitude details
combinedData <- combinedData[which(combinedData$SATV25 >= 400),]        # get rid of columns with 0 for SAT Verbal Scores

lat <- combinedData$LATITUDE
lon <- combinedData$LONGITUDE
Name <- combinedData$NAME

save(combinedData, file = "combinedData.Rda")  


ui <- fluidPage(
    style = "background-color: Wheat;",
    h1("Comparing SAT Verbal Scores Across Regions", style = "color: FireBrick; font-weight: bold; font-style: oblique;"),
    br(),
    h3("Heat map of US SAT verbal scores"),
    br(),
    
    #############
    ##   map   ##
    #############
    
    leafletOutput("mymap"),
    p(),
    #actionButton("recalc", "Recenter"),
    br(),
    titlePanel("College Data"),
    
    
    ###################
    ##   histogram   ##
    ###################
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 0,
                        max = 50,
                        value = 30),
            width = 4,
            selectInput(inputId = "reg1",
                        label = "First Region:",
                        choices = c("New England" = 1, "Mid East" = 2, "Great Lakes" = 3, "Plains" = 4, "Southeast" = 5, "Southwest" = 6, "Rocky Mountains" = 7, "Far West" = 8, "Outlying Areas" = 9)),
            
            # Input: Selector for choosing measures ----
            selectInput(inputId = "reg2",
                        label = "Second Region:",
                        choices = c("New England" = 1, "Mid East" = 2, "Great Lakes" = 3, "Plains" = 4, "Southeast" = 5, "Southwest" = 6, "Rocky Mountains" = 7, "Far West" = 8, "Outlying Areas" = 9))),
        
        # Show a plot of the generated distribution
        mainPanel(
            verticalLayout(plotOutput("distPlot"),
                           plotOutput("distPlot2")),
            width = 7
        )
    ),
    
    br()
)


server <- function(input, output) {
    
    #############
    ##   map   ##
    #############
    
    pal <- colorNumeric(
        palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
        domain = 400:800)
    
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(Data) %>%
            setView(lng = -99, lat = 38, zoom = 3.5)  %>% #setting the view over ~ center of North America
            addTiles() %>%
            # addCircles(data = combinedData, lat = ~ LATITUDE, lng = ~ LONGITUDE, weight = 1, radius = ~sqrt(SATV25)*500, popup =  ~as.character(SATV25), label = ~as.character(paste0("SAT Verbal: ", sep = " ", SATV25)), color = ~pal(SATV25), fillOpacity = 0.5) %>%
            addCircleMarkers(data = combinedData, lat =  ~LATITUDE, lng =~LONGITUDE, 
                             radius = 9, 
                             color = ~pal(SATV25),
                             popup =  ~as.character(SATV25),
                             label = ~as.character(paste0(NAME, " - ", "SAT Verbal: ", sep = " ", SATV25)),
                             stroke = FALSE, fillOpacity = 0.8) %>%
            
            addLegend(position = "bottomleft", pal=pal, values=combinedData$SATV25,opacity=1, title = "Verbal Score Avg.")
        
    })
    
    ###################
    ##   histogram   ##
    ###################
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- Data$SATV25
        region_names <- c("New England", "Mid East", "Great Lakes", "Plains", "Southeast", "Southwest", "Rocky Mountains", "Far West", "Outlying Areas")
        
        first_region    <- Data$SATV25[which(Data$SATV25 >= 400 & Data$OBEREG == input$reg1)]
        bins <- seq(min(first_region), max(first_region), length.out = input$bins + 1)
        
        
        # Setting the background color.
        par(bg = "Tan")
        # draw the histogram with the specified number of bins
        hist(first_region, breaks = bins, col = 'dodgerblue', border = 'white', main = paste("Student SAT Verbal Scores for", region_names[as.numeric(input$reg1)] ,"Universities"), xlab="SAT Verbal score", ylab = "Frequency")
        box()# Needed to force as.numeric on the input here for some reason. seems fully functional.
    })
    
    output$distPlot2 <- renderPlot({
        region_names <- c("New England", "Mid East", "Great Lakes", "Plains", "Southeast", "Southwest", "Rocky Mountains", "Far West", "Outlying Areas")
        
        second_region <- Data$SATV25[which(Data$SATV25 >= 400 & Data$OBEREG == input$reg2)]
        # generate bins based on input$bins from ui.R
        bins <- seq(min(second_region), max(second_region), length.out = input$bins + 1)
        # bins <- seq(min(se), max(se), length.out = 30 + 1)
        
        # Setting the background color.
        par(bg = "Tan")
        # draw the histogram with the specified number of bins
        hist(second_region, breaks = bins, col = 'firebrick', border = 'white', main = paste("Student SAT Verbal Scores for", region_names[as.numeric(input$reg2)] ,"Universities"), xlab="SAT Verbal score", ylab = "Frequency")
        box()
        
    })
}


shinyApp(ui = ui, server = server)