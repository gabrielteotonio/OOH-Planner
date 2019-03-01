header <- dashboardHeaderPlus(fixed = T,
                              title = div(img(src="ooh-planner.png", height = 38, weight = 29), ""),
                              titleWidth = 420,
                              enable_rightsidebar = F,
                              rightSidebarIcon = "bullhorn"
                              
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$style(
      ".main-sidebar {float:top; margin-top:20px; padding-left:0px; padding-right:0px}"
    ),
    menuItem("Flow of Passers", tabName = "visualization", icon = icon("street-view")),
    menuItem("Planning", tabName = "planning", icon = icon("map")), 
    menuItem("Performance", tabName = "performance", icon = icon("bar-chart-o"), badgeLabel = "Coming soon", badgeColor = "light-blue")
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(includeHTML(("google-analytics.html"))),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  tabItems(
    
    # First item ---
    tabItem(tabName = "visualization",
            fluidRow(
              column(width = 3,
                     div(h3("Flow of Passers"), h5("Data from 01 to 11 January 2019"))
              )
            ),
            fluidRow(
              column(width = 3,
                     wellPanel(
                       h4("Filter"),
                       selectInput("player",
                                   "Player",
                                   choices = c("All", 
                                               "Sun advertising",
                                               "Star advertising",
                                               "Rocket advertising"), selected = "All"),
                       selectInput("type",
                                   "Placement points",
                                   choices = c("All", 
                                               "Bus stop",
                                               "Digital clock",
                                               "Indoor"), selected = "All"),
                       selectInput("audience",
                                   "Audience",
                                   choices = c("General flow per point",
                                               "Flow of your customers per point",
                                               "Flow of your competitors per point"), selected = "General flow per point"),
                       textOutput("competitor")
                     ),
                     wellPanel(
                       highchartOutput("genderPlot", height = "200px") %>% withSpinner(color="#ABB9DB"),
                       highchartOutput("incomePlot", height = "311px") %>% withSpinner(color="#ABB9DB")
                     )
              ),
              column(width = 9,
                     leafletOutput("oohGeneralPlot", height = 882) %>% withSpinner(color="#ABB9DB")
              )
            )
    ),
    
    # Second item ---
    tabItem(tabName = "planning",
            fluidRow(
              column(width = 3,
                     div(h3("Planning"), h5("Data from 01 to 11 January 2019"))
              )
            ),
            fluidRow(
              column(width = 3,
                     h4("Here you have access to which OOH points perform best to influence the flow of 
                         visitors to one of your stores."),
                     wellPanel(
                       h4("Filter"),
                       selectInput("playerPlanning",
                                   "Player",
                                   choices = c("All", "Sun advertising", "Star advertising", "Rocket advertising"), selected = "All"),
                       selectInput("typePlanning", "Placement points", choices = c("All",
                                                                                   "Bus stop",
                                                                                   "Digital clock",
                                                                                   "Indoor"), selected = "All"),
                       selectInput("store",
                                   "Which of your stores do you want to influence the flow?",
                                   choices = unique(VisitsOOHStore[VisitsOOHStore$store == "The Best Store",]$address), selected = "AV DAS NACOES UNIDAS"),
                       hr(),
                       h4("In the map to the side, are represented the points where the majority of the visitors of the selected store pass."),
                       br(),
                       h4("Click the button below to download information about each of these points."),
                       br(), br(),
                       downloadButton("Planning", "DOWNLOAD"),
                       br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                     )
                     
              ),
              column(width = 9,
                     leafletOutput("oohPlanningPlot", height = 872) %>% withSpinner(color="#ABB9DB")
              )
            )
    ),
    
    # Third item ---
    tabItem(tabName = "performance",
            div(img(src = "InLocoLogo.png", height = 410, width = 400), style="text-align: center;"),
            h1("Location is everything!", align = "center")
    )
  )
)

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  div(id = "loading-content", "",
      img(src = "PinoLoad.gif", height = "48px", width = "48px")),
  tags$head(tags$link(rel="shortcut icon", href="InLocoLogo.png")),
  
  dashboardPagePlus(title = "Intelligence | OOH Planner", header, sidebar, body))