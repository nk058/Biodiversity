library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(DT)
library(plotly)
library(viridis)
library(hrbrthemes)


data<-read.csv("poland_data.csv",header = T)
setDT(data)
data$eventDate<-as.Date(data$eventDate,format ="%m/%d/%Y")



## Sidebar content
sidebar<-dashboardSidebar(

sidebarMenu(
    menuItem("Global Map", tabName = "map", icon = icon("thumbtack")),
    menuItem("Species View", tabName = "SpeciesView",icon = icon("dashboard")),
    menuItem("Species Observed - Trend", tabName = "charts",icon = icon("dashboard"))
  )
)

## Body Content
body<-dashboardBody(
  
  tabItems(
    tabItem(tabName="map",
            # parks map section
            leafletOutput("BioMap",width = "100%",height = 1000) 
            
    ),
    
    tabItem(tabName = "SpeciesView",
      # species data section
      fluidRow( column(width = 1),
        column(2, uiOutput("SelectVernacularName2"),offset = 1),
                column(2, uiOutput("SelectScientificName"),offset = 1),
      #tabName = "table", DTOutput("DataTable",width = "100%",height = "100%")
      column(10,leafletOutput("BioMap2",width = "120%",height = 1000))
      )
    ),
    
    tabItem(tabName = "charts",
    fluidRow( column(2, uiOutput("SelectVernacularName")),
              column(12, plotlyOutput("trend"))
              
    ))
   
  )## tab items end
)## body ends



### UI Part
ui <-fluidPage(
  dashboardPage(
    dashboardHeader(title = "Biodiversity"),
  sidebar ,
  body
  
))


###Server Part
server <- function(input, output,session) { 
         
  ## user input for species 
  
  ## get category list
  vernacularNameList<-data[,unique(vernacularName),]
  ScientificNameList<-data[,unique(scientificName),]
  
  
    
  # parks map
    output$BioMap <- renderLeaflet({
      
      dt<-data[,.(longitudeDecimal,latitudeDecimal,vernacularName,individualCount  )]
      
      
      getColor <- function(dt) {
        sapply(dt$individualCount, function(individualCount) {
          if(individualCount <= 10) {
            "green"
          } else if(individualCount <= 50) {
            "orange"
          } else {
            "red"
          } })
      }
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(dt)
      )
      
      
    
      m <- leaflet(dt) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addAwesomeMarkers(lng=~longitudeDecimal, lat=~latitudeDecimal, popup = ~as.character(vernacularName ),icon=icons, label = ~(individualCount),clusterOptions = markerClusterOptions()) %>%
        #addProviderTiles(providers$Stamen.TonerLabels, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() 
      m  # Print the map
      
    
    
  })
    
    #### Get the data according to selection
    v <- reactiveValues(dt = NULL)
    
    
    observeEvent(input$SelectScientificName, {
      print(input$SelectScientificName)
      v$dt <-data[scientificName %in% input$SelectScientificName,.(longitudeDecimal,latitudeDecimal,vernacularName,individualCount,scientificName)]
    })
    
    
    observeEvent(input$selectedVernacularName2, {
      print(input$selectedVernacularName2)
      v$dt <- data[vernacularName %in% input$selectedVernacularName2,.(longitudeDecimal,latitudeDecimal,vernacularName,individualCount,scientificName)]
    })  
    
    bio_select <- reactive({
      if (is.null(v$dt)) return()
      v$dt
      #print(v$data)
      
    })
    
    ### Get user Species name input
    
    output$SelectVernacularName2 <- renderUI({
      selectInput("selectedVernacularName2","Select a vernacularName:", choices=vernacularNameList,selected = vernacularNameList)
    })
    
    ### Get Scientific name 
    
    output$SelectScientificName <- renderUI({
      selectInput("SelectScientificName","Select a Scientific Name:", choices=ScientificNameList)})
    
    #observeEvent(input$SelectScientificName, {
    #  updateSelectInput(session, 'selectedVernacularName2', "Select vernacularName:", choices = unique(data[scientificName ==input$SelectScientificName,]$vernacularName),selected = NULL)
    #    })
    

    
  ###----Map2
    output$BioMap2 <- renderLeaflet({
    
      dt<-bio_select()
      print(dt)
      
      getColor <- function(dt) {
        sapply(dt$individualCount, function(individualCount) {
          if(individualCount <= 4) {
            "green"
          } else if(individualCount <= 5) {
            "orange"
          } else {
            "red"
          } })
      }
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(dt)
      )
      
      
      
      m <- leaflet(dt) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addAwesomeMarkers(lng=~longitudeDecimal, lat=~latitudeDecimal, popup = ~as.character(paste("Vernacular Name:",vernacularName,"Scientific Name :",scientificName)),icon=icons, label = ~(individualCount),clusterOptions = markerClusterOptions()) %>%
        #addProviderTiles(providers$Stamen.TonerLabels, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() 
      m  # Print the map
      
  
    })
    
    
    
    output$SelectVernacularName <- renderUI({
      selectInput("selectedVernacularName","Select a vernacularName:", choices=vernacularNameList )
    })
    
    
    
    output$trend <- renderPlotly({
      
      dd_cat<-data[vernacularName==input$selectedVernacularName,]
      ###--Plot as per user input
      g<-ggplot(dd_cat,aes(x=eventDate, y=individualCount, fill=id))+geom_bar(stat="identity")+scale_fill_viridis(discrete = T) +
      labs( x ="",y="", title = paste0("Total number of incidents","Species : ", unique(data$vernacularName)))+ theme_modern_rc() 
      
      (ggplotly(g))  
      
    })  
    
    
    # DT table
    output$DataTable <-renderDT(dt,
                                filter = "top",
                                options = list(
                                  pageLength = 20
                                ))
  
  
}

shinyApp(ui, server)






