
# Load Packages & Data ----------------------------------------------------

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(shinyWidgets)

getwd()
# setwd("\\Users\\awithin\\Documents\\Shiny_deploy")
load("ShinyPracticeData.Rdata")
load("PracticeDataLocationInfo.Rdata")
stationInfo$MonitoringLocationIdentifier <- gsub("-ORDEQ", "", stationInfo$MonitoringLocationIdentifier)

stations <- stationInfo[,c(4:7)]
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)
mapData <- merge(dta_sub, stations, by.x="lasar_id", by.y="MonitoringLocationIdentifier", all.x=TRUE, all.y=FALSE)%>%
  filter_at(vars(contains("grade")), all_vars(. != "E" & . != "C"))


# UI ----------------------------------------------------------------------


ui = shinyUI(navbarPage(
  "A Shiny App for Dissolved Oxygen",
  

# Main Tab 1 -------------------------------------------------------------------
tabPanel("Clickable Map",
         tabsetPanel(
           tabPanel("Table",
              sidebarLayout(
                  sidebarPanel(
                     leafletOutput("map"),
                     textOutput("temp"),
                     checkboxGroupButtons(
                       inputId = "checkboxtablesorter",
                       label = "Select table columns to display:",
                       choices = names(mapData),
                       selected = c("lasar_id", "datetime", "temp", "ph", "do", "cond",
                                    "data_source", "MonitoringLocationName")
                     )
                          ),
                  mainPanel(DT::dataTableOutput("table"))
                            )
           ),
           tabPanel("Plot of total number of samples per year",
                    plotlyOutput("nplot"),
                    plotlyOutput("avgdoplot")),
           tabPanel("Other overview visualizations",
                    plotlyOutput("summaryboxplot"),
                    plotlyOutput("summaryboxplot2"),
                    plotlyOutput("summaryboxplot3"))
         )), 


# Main Tab 2 -------------------------------------------------------------------
  tabPanel(
    "Search by Station",
    fluidPage(
    tabsetPanel(
      tabPanel("Plots",
      sidebarPanel(
        selectInput(
          inputId = "station_selection",
          label = "Select station:",
          choices = levels(mapData$lasar_id)
        ),
        selectInput(
          inputId = "x",
          label = "x-axis",
          choices = names(mapData),
          selected = "datetime"
        ),
        selectInput(
          inputId = "y",
          label = "y-axis",
          choices = names(mapData),
          selected = "do"
        ),
        selectInput(
          inputId = "y2",
          label = "2nd y-axis",
          choices = names(mapData),
          selected = "temp"
        ),
        selectInput(
          inputId = "y3",
          label = "3rd y-axis",
          choices = names(mapData),
          selected = "cond"
        )
        # sliderInput(
        #   inputId = "datefilter",
        #    label = "Select years:",
        #   min = min(mapData$datetime),
        #   max = max(mapData$datetime),
        #   value = c(min(mapData$datetime), max(mapData$datetime)),
        #   timeFormat = "%Y"
        # # ),
        # sliderInput(
        #   inputId = "datefilter",
        #   label = "Select years:",
        #   min = 2008,
        #   max = 2013,
        #   value = c(2008, 2013),
        #   timeFormat = "%Y"
        # ),
        # sliderTextInput(
        #   inputId = "monthfilter",
        #   label = "Select months:",
        #   choices = month.abb,
        #   selected = c("Jan", "Dec")
        # ),
        # sliderInput(
        #   inputId = "monthfilter",
        #   label = "Select months:",
        #   min = 01,
        #   max = 12,
        #   value = c(1,12),
        #   timeFormat = "%m"
        # )
      ),
      mainPanel(
          plotlyOutput("subsetscatter"),
          plotlyOutput("subsetscatter2"),
          plotlyOutput("subsetscatter3")
        )),
        tabPanel("Data Selected From Graph, in a Table", DT::dataTableOutput("graph_to_table"))
      )
    )
      
    ),

# Main Tab 3 -------------------------------------------------------------------

    tabPanel("Search by DO pass/fail",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                 selectizeInput(
                   inputId = "station_pf",
                   label = "Select station(s):",
                   choices = levels(mapData$lasar_id),
                   multiple = TRUE
                 ),
                 radioButtons(
                   inputId = "DOPassFailRadio",
                   label = "Select:",
                   choices = c("Pass", "Fail", "All"),
                   selected = "All"
                 )
               ),
                 mainPanel(
                   tabsetPanel(
                   tabPanel("Map"),
                   tabPanel("Table")
                 )
                   )
         ))
  
  )
)
)

# Server ------------------------------------------------------------------

server = function(input, output, session) {
  

# Main Tab 1 Items --------------------------------------------------------

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite Map") %>%
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
      addProviderTiles("CartoDB", group = "Carto") %>%
      addMarkers(data = stations,
                 label = ~paste0("Station ID: ", MonitoringLocationIdentifier, "  Site Name: ", MonitoringLocationName),
                 labelOptions = labelOptions(textOnly = FALSE),
                 layerId = stations$MonitoringLocationIdentifier) %>%
      addLayersControl(baseGroups = c("Esri Satellite Map", "Open Topo Map", "Carto"))
  })
  
  output$temp <- renderPrint({
    req(input$map_marker_click$id)
    cat("The station you have chosen is: ", input$map_marker_click$id)
  })
  
  output$table <- DT::renderDataTable({
    shiny::validate(need(!is.null(input$map_marker_click$id), "Click on a station to view data"))
    maptabledata <- mapData %>% 
      select(input$checkboxtablesorter) %>% 
      filter(lasar_id == input$map_marker_click$id)
    DT::datatable(
      data = maptabledata,
      options = list(pageLength = 10),
      rownames = FALSE,
      filter = 'bottom'
    )
  })
  
  # output$dateplot <- renderPlot()
  output$nplot <- renderPlotly({
    mapData %>% 
      group_by(month = floor_date(datetime, "month")) %>% 
      mutate(n_samples = n()) %>% 
      mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct"))) %>% 
      plot_ly(x = ~month, y = ~n_samples) %>% 
      add_markers(color = ~season, colors = "Set3", size = ~n_samples)
  })
  
  output$avgdoplot <- renderPlotly({
    mapData %>%
      group_by(lasar_id, month = floor_date(datetime, "month")) %>% 
      mutate(min = min(do)) %>% 
      plot_ly(x = ~month, y = ~min, color = ~lasar_id,
              text = ~paste('Site:', MonitoringLocationName))
  })
  
  output$summaryboxplot <- renderPlotly({
    boxplotData <- mapData %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData, x = ~lasar_id, y = ~do, color = ~season, type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot2 <- renderPlotly({
    boxplotData <- mapData %>% 
      mutate(month = floor_date(datetime, "month")) %>%
      mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData, x = ~season, y = ~do, color = ~lasar_id, type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  
  output$summaryboxplot3 <- renderPlotly({
    boxplotData <- mapData %>% 
      mutate(year = factor(floor_date(datetime, "year")))
      # mutate(season = factor(month.abb[month(datetime)], levels=c("May", "Jun", "Jul", "Aug", "Oct")))
    plot_ly(boxplotData, x = ~year, y = ~do, color = ~lasar_id, type = "box",
            text = ~paste('Site: ', MonitoringLocationName)) %>%
      layout(boxmode = "group")
  })
  

# Main Tab 2 Items --------------------------------------------------------

  
  stations_subset <- reactive({
    req(input$station_selection)
    filter(mapData, lasar_id %in% input$station_selection)
  })
  
  # output$subsetscatter <- renderPlot({
  #   ggplot(data = stations_subset(),
  #          aes_string(x = input$x, y = input$y)) +
  #     geom_point()
  # })
 
  output$subsetscatter <- renderPlotly({
    plot_ly(data = stations_subset(),
            x = ~get(input$x), y = ~get(input$y),
            type = "scatter")
  }) 
 
  output$subsetscatter2 <- renderPlotly({
    plot_ly(data = stations_subset(),
           x = ~get(input$x), y = ~get(input$y2),
           type = "scatter")
  }) 
  
  output$subsetscatter3 <- renderPlotly({
    plot_ly(data = stations_subset(),
            x = ~get(input$x), y = ~get(input$y3),
            type = "scatter")
  })
  

  output$graph_to_table <- DT::renderDT({
    d <- event_data("plotly_selected", source = "A")
    if(is.null(d) == T) return(NULL)
    # else stations_subset() %>% filter(input$x > min(d$x) )
    else stations_subset() %>% filter(between(input$x, min(d$x), max(d$x)))
    
    
    
    
    
    # shiny::validate(need(!is.null(d), "Lasso graph to view data"))
    # # stations_subset() %>% filter(between(input$x, min(numeric(d$x)), max(numeric(d$x))))
    # stations_subset() %>% filter(input$x > min(d$x) & input$x < max(d$x))
    # print(str(d))
    # print(str(stations_subset()))
    # print(str(input$x))
  })
  
  # output$subsetscatter2 <- renderPlot({
  #   ggplot(data = stations_subset(),
  #          aes_string(x = ~x, y = ~y2)) +
  #     geom_point()
  # })
  # 
  # output$subsetscatter3 <- renderPlot({
  #   ggplot(data = stations_subset(),
  #          aes_string(x = input$x, y = input$y3)) +
  #     geom_point()
  # })
  
  # output$subsetscatter3 <- renderPlotly({
  #   subplot(
  #     plot_ly(stations_subset(), x = ~input$x, y = ~input$y, name = "default"),
  #     plot_ly(stations_subset(), x = ~input$x, y = ~input$y2) %>% 
  #       add_markers(alpha = 0.2, name = "alpha"),
  #     plot_ly(stations_subset(), x = ~input$x, y = ~input$y3) %>% 
  #       add_markers(symbol = I(1), name = "hollow")
  # )
  # })
  
  
  
}

shinyApp(ui = ui, server = server)
