server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  ######### Panel 1 ######### 
  
  # Filtering by player input ----
  selectGeneralData <- reactive({
    if (input$audience == "General flow per point") {
      if (input$player == "All") {
        if (input$type == "All") {
          return(VisitsOOH)
        }
        else {
          return(VisitsOOH %>% filter(type.y == input$type))
        }
      }
      else {
        if (input$type == "All") {
          return(VisitsOOH %>% filter(player == input$player))
        }
        else {
          return(VisitsOOH %>% filter(player == input$player & type.y == input$type))
        }
      }
    }
    else {
      if (input$player == "All") {
        if (input$type == "All") {
          return(VisitsOOHGeneral %>% filter(store == input$audience))
        }
        else {
          return(VisitsOOHGeneral %>% filter(store == input$audience & type.y == input$type))
        }
      }
      else {
        if (input$type == "All") {
          return(VisitsOOHGeneral %>% filter(store == input$audience & player == input$player))
        }
        else {
          return(VisitsOOHGeneral %>% filter(store == input$audience & player == input$player & type.y == input$type))
        }
      }
    }
  })
  
  output$competitor <- renderText({
    if (input$audience == "Flow of your competitors per point") {
      paste("Competitors: The Shiny Store, The Great Store and The Awesome Store.")
    }
    else {
      paste("")
    }
  })
  
  # General Map plot ----
  output$oohGeneralPlot <- renderLeaflet({
    data <- selectGeneralData()
    data %>% 
      leaflet(options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 5, maxZoom = 19)) %>%  
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lat = -23.550453, lng = -46.633910, zoom = 10) %>% 
      addCircleMarkers(radius = 8, weight = 5, fillColor = "#3E4965", color = "#676681", layerId = ~id,
                       stroke = T, fillOpacity = 2, opacity = 0.7,
                       clusterOptions = markerClusterOptions(
                         iconCreateFunction = JS("function (cluster) {    
                            var childCount = cluster.getChildCount();  
                            if (childCount < 100) {  
                              c = '#ABB9DB;'
                            } else if (childCount < 1000) {  
                              c = '#ABB9DB;'  
                            } else { 
                              c = '#ABB9DB;'  
                            }    
                            return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
                        
                          }"
                         ))) 
  })
  
  observe({
    event <- input$audience
    if (event == "Flow of your customers per point") {
      leafletProxy("oohGeneralPlot") %>%  addMarkers(data = Stores, icon = Storeicons, lat = ~lat, lng = ~lng,
                                                     label = ~paste(store, " | ", address), labelOptions =  labelOptions(textsize = "17px"))
    }
  })
  
  # Show a popup at the given location ---
  showPopupGeneral <- function(idPoint, lat, lng) {
    quantiles <- quantile(selectGeneralData()$total_uu, probs = c(0, 0.25, 0.5, 0.75, 1), names = F)
    data <- selectGeneralData() %>% filter(id == idPoint)
    if (data$total_uu <= quantiles[1]) {
      content <- as.character(tagList(
        tags$h5(img(src = "01.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type.y, data$id), align = "center")
      ))
      leafletProxy("oohGeneralPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500)                                                 )
    }  
    else if (data$total_uu <= quantiles[2]) {
      content <- as.character(tagList(
        tags$h5(img(src = "02.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type.y, data$id), align = "center")
      ))
      leafletProxy("oohGeneralPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else if (data$total_uu <= quantiles[3]) {
      content <- as.character(tagList(
        tags$h5(img(src = "03.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type.y, data$id), align = "center")
      ))
      leafletProxy("oohGeneralPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else if (data$total_uu <= quantiles[4]) {
      content <- as.character(tagList(
        tags$h5(img(src = "04.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type.y, data$id), align = "center")
      ))
      leafletProxy("oohGeneralPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else {
      content <- as.character(tagList(
        tags$h5(img(src = "05.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type.y, data$id), align = "center")
      ))
      leafletProxy("oohGeneralPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }                                                 
  }
  
  # When map is clicked, show a popup with point info ---
  observe({
    leafletProxy("oohGeneralPlot") %>% clearPopups()
    event <- input$oohGeneralPlot_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPopupGeneral(event$id, event$lat, event$lng)
    })
  })
  
  # Filtering by point input ----
  DataClickGeneralGender <- reactive({
    if(is.null(input$oohGeneralPlot_marker_click$id)) {
      gender <- c("Feminine", "Masculine")
      percentage <- c(VisitsOOH[VisitsOOH$id == "5a0101113227c99def6264bb",]$feminine, VisitsOOH[VisitsOOH$id == "5a0101113227c99def6264bb",]$masculine)
      return(data.frame(gender, percentage))
    }
    else {
      gender <- c("Feminine", "Masculine")
      percentage <- c(VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$feminine, VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$masculine)
      return(data.frame(gender, percentage))
    }
  })
  
  # General Gender plot ----
  output$genderPlot <- renderHighchart({
    data <- DataClickGeneralGender()
    # hciconarray(c("Feminino", "Masculino"), data$percentage, icons = c("male", "female"))
    data %>% hchart('pie', hcaes(x = 'gender', y = 'percentage')) %>% 
      hc_yAxis(title = list(text = "Percentage"),
               opposite = FALSE,
               labels = list(format = "{value}%")) %>% 
      hc_title(text = "Gender",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = '{point.y:.2f}% ') %>% 
      hc_colors(c("#61729C", "#ABB9DB")) %>% 
      hc_legend(align = "right", verticalAlign = "middle", layout = "vertical")
    
  })
  
  # Filtering by point input ----
  DataClickGeneralIncome <- reactive({
    if(is.null(input$oohGeneralPlot_marker_click$id)) {
      income <- c("< 1", "1 - 2", "2 - 5", "5 - 10", "10 - 20", "20 +")
      probability <- c(VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$meio + VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$um,
                       VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$dois,
                       VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$tres + VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$cinco,
                       VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$dez,
                       VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$quinze + VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$vinte,
                       VisitsOOH[VisitsOOH$id == "5a01010f3227c99def6264b5",]$mais_vinte)
      barplotData <- data.frame(income, probability)
      return(barplotData)
    }
    else {
      income <- c("< 1", "1 - 2", "2 - 5", "5 - 10", "10 - 20", "20 +")
      probability <- c(VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$meio + VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$um,
                       VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$dois,
                       VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$tres + VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$cinco,
                       VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$dez,
                       VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$quinze + VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$vinte,
                       VisitsOOH[VisitsOOH$id == input$oohGeneralPlot_marker_click$id,]$mais_vinte)
      barplotData <- data.frame(income, probability)
      return(barplotData)
    }
  })
  
  # General Income plot ----
  output$incomePlot <- renderHighchart({
    data <- DataClickGeneralIncome() 
    hchart(data, "column", hcaes(x = income, y = probability)) %>% 
      hc_title(text = "Income",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>%
      hc_yAxis(title = list(text = "% Percentage"),
               opposite = FALSE,
               labels = list(format = "{value}%")) %>% 
      hc_xAxis(title = list(text = "Minimum wages")) %>% 
      hc_tooltip(pointFormat = '{point.y:.2f}% ') %>% 
      hc_colors("#61729C")
    
  })
  
  ######### Panel 2 ######### 
  
  DataClickPlanning <- reactive({
    data <- PlanningData %>% 
      filter(address == input$store) %>% 
      arrange(desc(total_uu)) %>% 
      head(50)
    if (input$playerPlanning == "All") {
      if (input$typePlanning == "All") {
        return(data)
      }
      else {
        return(data %>% filter(type == input$typePlanning))
      }
    }
    else {
      if (input$typePlanning == "All") {
        return(data %>% filter(player == input$playerPlanning))
      }
      else {
        return(data %>% filter(player == input$playerPlanning & type == input$typePlanning))
      }
    }
  })
  # Map to level up flow ----
  output$oohPlanningPlot <- renderLeaflet({
    data <- DataClickPlanning()
    if (nrow(data) == 0) {
      leaflet(options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 11, maxZoom = 18)) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addMarkers(data = Stores[Stores$address == input$store,], icon = Storeicons, lat = ~lat, lng = ~lng,
                   label = ~paste(store, " | ", address), labelOptions =  labelOptions(textsize = "17px"))
    }
    else {
      data %>% 
        leaflet(options = leafletOptions(zoomControl = TRUE,
                                         minZoom = 11, maxZoom = 18)) %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(lat = Stores[Stores$address == input$store, ]$lat, lng =	Stores[Stores$address == input$store, ]$lng, zoom = 13) %>% 
        addCircleMarkers(lat = ~latPoint , lng = ~lngPoint, radius = 8, weight = 5, fillColor = "#3E4965", color = "#676681", layerId = ~id,
                         stroke = T, fillOpacity = 1, opacity = 0.7) %>% 
        addMarkers(data = Stores[Stores$address == input$store,], icon = Storeicons, lat = ~lat, lng = ~lng,
                   label = ~paste(store, " | ", address), labelOptions =  labelOptions(textsize = "17px"))
    }
  })
  
  # Show a popup at the given location ---
  showPopupPlanning <- function(idPoint, lat, lng) {
    quantiles <- quantile(DataClickPlanning()$total_uu, probs = c(0, 0.25, 0.5, 0.75, 1), names = F)
    data <- DataClickPlanning() %>% filter(id == idPoint)
    if (data$total_uu <= quantiles[1]) {
      content <- as.character(tagList(
        tags$h5(img(src = "01.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type, data$id), align = "center")
      ))
      leafletProxy("oohPlanningPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500)                                                 )
    }  
    else if (data$total_uu <= quantiles[2]) {
      content <- as.character(tagList(
        tags$h5(img(src = "02.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type, data$id), align = "center")
      ))
      leafletProxy("oohPlanningPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else if (data$total_uu <= quantiles[3]) {
      content <- as.character(tagList(
        tags$h5(img(src = "03.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type, data$id), align = "center")
      ))
      leafletProxy("oohPlanningPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else if (data$total_uu <= quantiles[4]) {
      content <- as.character(tagList(
        tags$h5(img(src = "04.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type, data$id), align = "center")
      ))
      leafletProxy("oohPlanningPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
    else {
      content <- as.character(tagList(
        tags$h5(img(src = "05.png",
                    height = "22", weigth = "25"), align = "center"),
        tags$h6(img(src = "walking.png",
                    height = "26", weigth = "25"), " FLOW INTENSITY", align = "center", color = "grey"),
        tags$h5(tags$strong(HTML(sprintf("%s",
                                         data$player
        ))), align = "center"),
        tags$h5(sprintf("%s | ID: %s", data$type, data$id), align = "center")
      ))
      leafletProxy("oohPlanningPlot") %>% addPopups(lng, lat, content, layerId = idPoint, options = popupOptions(closeButton = F, minWidth = 200, maxWidth = 500))
    }
  }
  
  # When map is clicked, show a popup with point info ---
  observe({
    leafletProxy("oohPlanningPlot") %>% clearPopups()
    event <- input$oohPlanningPlot_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPopupPlanning(event$id, event$lat, event$lng)
    })
  })
  
  
  # Download Planning data ----
  output$Planning <- downloadHandler(
    filename = function() {
      paste("Planning ", input$store, ".csv", sep = "")
    },
    content = function(file) {
      write.csv((DataClickPlanning() %>% rename("ID DO PONTO" = id, "PLAYER" = player, "TOTAL DE PASSANTES" = total_uu) %>% 
                   select(`ID DO PONTO`, PLAYER, `TOTAL DE PASSANTES`))
                , file, row.names = F)
    }
  )
  
  Sys.sleep(5)
  hide("loading-content", TRUE, "fade", 5)
  
}