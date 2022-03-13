# List of libraries used in this project
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.providers)
library(tidyverse)
library(stringr)
library(shinyWidgets)
library(gridExtra)
library(scales)
library(DT)

# reading data files into a dataframe
temp = list.files(pattern="*.tsv")
allData <- lapply(temp,read.delim)
ctaloc <- do.call(rbind, allData)
stationname <- unique(ctaloc[["stationname"]])

cta_2021 <- ctaloc %>% filter(year(date) == "2021") %>% select("stationname", "date", "rides")
cta_rides <- ctaloc %>% select("stationname", "date", "rides")

# UI part of the dashboard
ui<- dashboardPage(
  
  dashboardHeader(title="CS 424 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("About Page", tabName = "dashboard"),
      menuItem("Main Page", tabName = "widgets",selected = TRUE),
      menuItem("Two Dates", tabName = "dates")
    )
  ),
  # Body of the UI which contains all the UI input and output elements
  dashboardBody(
    tabItems(tabItem(tabName = "dashboard",
                     h2("About Page"),
                     p("The data is collected from this "), a("website", aref = "https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),p("it contains the stationname and rides for different dates"),
                     "and we have also used the location of the stations which is available at this ", a("location", aref = "https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme"),
             p("Chicago Transit Authority published this data and it contains 
                       how many people have taken trains or buses at every stop. 
                       Data is collected from 2001 to 2021 it contains around 2 million rows and 148 stations"),
             p(strong("Packages Used: "), "shiny, shinydashboard, lubridate, ggplot2, leaflet, leaflet.providers, tidyverse, stringr, shinyWidgets, gridExtra, scales, DT"),
             p(strong("Created Using: "), "R, R studio, Shiny"),
             p("This app is written by Abhijeet Chintakunta and Karan Jogi as part of course project for CS 424: Visualization and visua")),
             
             tabItem(tabName="widgets",
                     fluidRow(
                       column(5,
                              fluidRow({leafletOutput("map", height = "1300")})),
                       column(4,
                              conditionalPanel(
                                condition = "input.gt == 'Table'",
                                dataTableOutput("datatable", height = "761")
                              ),
                              conditionalPanel(
                                condition = "input.gt == 'Graph'",
                                plotOutput("initialbar", height = "850"),
                                radioButtons("graphtype","GraphType",c("Ascending","Descending","Alphabetical"),selected="Alphabetical",inline=TRUE)
                              ),
                              
                              radioButtons("gt","Graph or Table",c("Graph","Table"),selected="Graph",inline = TRUE),
                              actionButton("previousday","Previous day"),
                              actionButton("nextday","Next day"),
                              dateInput("date", "Date:", value = "2021-05-23",min = "2001-01-01", max="2021-12-31" ),
                              br(),
                              selectInput(inputId="stationname",label="Select the Station",stationname,selected="UIC-Halsted"),
                              strong(paste("List of Lines this station serves: ")),
                              verbatimTextOutput("lines")
                       ),
                       column(3,
                              plotOutput("stationplot"),
                              plotOutput("stationplot_all", height = "600"),
                              tabsetPanel(type = "pills",
                                          tabPanel("Daily", dataTableOutput("station_t_daily")),
                                          tabPanel("Monthly", dataTableOutput("station_t_monthly")),
                                          tabPanel("Weekdays", dataTableOutput("station_t_wday"))
                              )
                       )
                     )),
             tabItem(tabName="dates",
                     fluidRow(
                       column(5,
                              fluidRow(leafletOutput("twodatesmap",height="1300"))
                              ),
                       column(7,
                              plotOutput("twodates", height = "800"),
                              dateRangeInput("dateRange", "TwoDates", separator="and", start="2021-08-23",
                                             end="2021-08-24", min = "2001-01-01", max="2021-12-31"),
                              dataTableOutput("tdates"))
                     )
                       )
  )))

# server function which acts as backend for this application
server <- function(input,output,session){
  cta <- reactive({
    subset(ctaloc,ctaloc$date==input$date)
  })
  # bar graph with stationname and rides 
  output$initialbar <- renderPlot({
    ggplot(cta(),aes(x=stationname,y=rides))+
      geom_bar(stat="identity",fill="steelblue")+
      coord_flip()+ labs(x="stationname")
  })
  
  # datatable with stationname and rides
  output$datatable <- DT::renderDataTable({select(cta(),stationname,rides)},rownames=FALSE,options=list(pageLength=15),colnames = c("stationname","Entries"))
  
  # Next Day button event observer
  observeEvent(input$nextday,{
    date <- input$date 
    date <- date + days(1)
    updateDateInput(session, "date",value =date)
    })
  
  
  # Previous Day button event observer
  observeEvent(input$previousday,{
    date <- input$date 
    date <- date - days(1)
    updateDateInput(session, "date",value =date)
  })
  
  #TwoDates processing Code
  observeEvent(input$dateRange,{
    ctadates <- subset(ctaloc,date==format(input$dateRange[1]) | date==format(input$dateRange[2]))
    output$twodates <- renderPlot({
      # ggplot(ctadates,aes(x=stationname,fill=date,y=rides))+geom_col()+
      #   facet_grid(~rides)
      ggplot(ctadates,aes(x=stationname,fill=date,y=rides))+
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)+coord_flip()
    })
    output$tdates <- DT::renderDataTable({select(ctadates,stationname,date,rides)},rownames=FALSE,options=list(pageLength=7),colnames = c("stationname","date","Entries"))
    ctadates<- ctadates %>%  mutate(rides_diff = abs(rides - lag(rides)))
    ctared1 <- subset(ctadates,ctadates$RED=="TRUE")
    ctablue1 <- subset(ctadates,ctadates$BLUE=="TRUE")
    ctagreen1 <- subset(ctadates,ctadates$G=="TRUE")
    ctabrown1 <- subset(ctadates,ctadates$BRN=="TRUE")
    ctayellow1 <- subset(ctadates,ctadates$Y=="TRUE")
    ctapink1 <- subset(ctadates,ctadates$Pnk=="TRUE")
    ctaorange1 <- subset(ctadates,ctadates$O=="TRUE")
    ctapurple1 <- subset(ctadates,ctadates$P=="TRUE" & ctadates$Pexp=="TRUE")
    output$twodatesmap <- renderLeaflet({
      leaflet(options=leafletOptions(attributionControl=FALSE)) %>%
        setView(lng = -87.649707, lat = 41.875474, zoom = 11.5) %>% 
        addTiles() %>%
        addCircles(ctared1$Longitude,ctared1$Latitude,radius=25,color="red",group="Red",
                   label=paste("<strong>",ctared1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctared1$rides_diff) %>% 
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,direction = "bottom")) %>%
        addCircles(ctablue1$Longitude,ctablue1$Latitude,radius=25,color="blue",group="Blue",
                   label=paste("<strong>",ctablue1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctablue1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctagreen1$Longitude,ctagreen1$Latitude,radius=25,color="green",group="Green",
                   label=paste("<strong>",ctagreen1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctagreen1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctabrown1$Longitude,ctabrown1$Latitude,radius=25,color="brown",group="Brown",
                   label=paste("<strong>",ctabrown1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctabrown1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctayellow1$Longitude,ctayellow1$Latitude,radius=25,color="yellow",group="Yellow",
                   label=paste("<strong>",ctayellow1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctayellow1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctapink1$Longitude,ctapink1$Latitude,radius=25,color="pink",group="Pink",
                   label=paste("<strong>",ctapink1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctapink1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctaorange1$Longitude,ctaorange1$Latitude,radius=25,color="orange",group="Orange",
                   label=paste("<strong>",ctaorange1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctaorange1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addCircles(ctapurple1$Longitude,ctapurple1$Latitude,radius=25,color="purple",group="Purple",
                   label=paste("<strong>",ctapurple1$stationname,"</strong>","<br><strong>Difference:</strong>",
                               ctapurple1$rides_diff) %>%
                     lapply(htmltools::HTML),
                   labelOptions = labelOptions(noHide = T,
                                               direction = "bottom")) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "B/w") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Railway") %>%
        addLayersControl(
          baseGroups = c("B/w", "Toner", "Railway"),
          overlayGroups = c("Red","Blue","Green","Brown","Yellow","Pink","Orange","Purple"),
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE))
    })
    })
  
  # to change the graph to ascending and descending order depending on the selection of radioInput    
  observeEvent(input$graphtype,
    if(input$graphtype=="Ascending"){
      output$initialbar <- renderPlot({
        ggplot(cta(),aes(x=reorder(stationname,+rides),y=rides))+
          geom_bar(stat="identity",fill="steelblue")+
          coord_flip()
      })
    }
    else if(input$graphtype=="Descending"){
      output$initialbar <- renderPlot({
        ggplot(cta(),aes(x=reorder(stationname,-rides),y=rides))+
          geom_bar(stat="identity",fill="steelblue")+
          coord_flip()+ labs(x="stationname")
      })
    }
    else if(input$graphtype=="Alphabetical"){
      output$initialbar <- renderPlot({
        ggplot(cta(),aes(x=stationname,y=rides))+
          geom_bar(stat="identity",fill="steelblue")+
          coord_flip()+ labs(x="stationname")
      })
    })
  
  ctastation_year <- reactive({cta_rides %>% filter(stationname == input$stationname, year(date) == year(input$date))})
  ctastationname_2021 <- reactive({subset(cta_2021,cta_2021$stationname==input$stationname)})
  
  # Daily, Monthly, Yearly graphs of a particular station
  output$stationplot_all<- renderPlot({
    
    
    p1 <- ggplot(ctastationname_2021(),aes(x=date(date),y=rides))+geom_bar(stat="identity",fill="steelblue")+
      labs(x = "Date", y ="Entries")+
      scale_y_continuous(labels = comma)+
      scale_x_date(date_breaks = "1 month",date_labels="%b")
    
    p2 <- ggplot(ctastation_year(),aes(x=month(date, label = TRUE),y=rides))+geom_bar(stat="identity",fill="steelblue")+
      labs(x = "Month", y ="Entries")+
      scale_y_continuous(labels = comma)
    
    p3 <- ggplot(ctastation_year(),aes(x=wday(date, label = TRUE),y=rides))+geom_bar(stat="identity",fill="steelblue")+
      labs(x = "Weekday", y ="Entries")+
      scale_y_continuous(labels = comma)
    
    grid.arrange(p1, p2, p3)
  })
  
  # tables of daily, monthly, weekday
  output$station_t_daily <- DT::renderDataTable({ctastationname_2021() %>% select("date", "rides") %>% arrange(date)},rownames=FALSE,options=list(pageLength=5),colnames = c("Date","Entries"), )
  output$station_t_monthly <- DT::renderDataTable({ctastation_year() %>% group_by("month" = month(date, label = TRUE)) %>% summarise("rides" = sum(rides)) %>%  select("month", "rides")},rownames=FALSE,options=list(pageLength=5),colnames = c("Month","Entries"))
  output$station_t_wday <- DT::renderDataTable({ctastation_year() %>% group_by("wday" = wday(date, label = TRUE)) %>% summarise("rides" = sum(rides)) %>%  select("wday", "rides")},rownames=FALSE,options=list(pageLength=5),colnames = c("Weekday","Entries"))
  
  # for a change in stationname in select input we are changing the bargraph
  observeEvent(input$stationname,
               output$stationplot<- renderPlot({
                 ctastationname <- reactive({subset(ctaloc,ctaloc$stationname==input$stationname)})
                 ctaline <- ctastationname()[1,]
                 ctalines <- vector()
                 if(ctaline$BLUE=="TRUE"){
                   ctalines<- append(ctalines,"Blue")
                 }
                 if(ctaline$RED=="TRUE"){
                   ctalines<- append(ctalines,"Red")
                 }
                 if(ctaline$G=="TRUE"){
                   ctalines<- append(ctalines,"Green")
                 }
                 if(ctaline$BRN=="TRUE"){
                   ctalines<- append(ctalines,"Brown")
                 }
                 if(ctaline$P=="TRUE" || ctaline$Pexp=="TRUE"){
                   ctalines<- append(ctalines,"Brown")
                 }
                 if(ctaline$Y=="TRUE"){
                   ctalines<- append(ctalines,"Yellow")
                 }
                 if(ctaline$Pnk=="TRUE"){
                   ctalines<- append(ctalines,"Pink")
                 }
                 if(ctaline$O=="TRUE"){
                   ctalines<- append(ctalines,"Orange")
                 }
                 output$lines <- renderText({paste(ctalines)})
                 ggplot(ctastationname(),aes(x=year(date),y=rides))+geom_bar(stat="identity",fill="steelblue")+
                   labs(x = "Year", y ="Entries")+
                   scale_y_continuous(labels = comma)+scale_x_continuous(breaks=c(2001:2021))
               }))
  
  # change in bar graph when clicked on leaflet map
  observeEvent(input$map_shape_click,
          output$stationplot<- renderPlot({
            list <- input$map_shape_click
            ctastationname1 <- reactive({filter(ctaloc,ctaloc$Latitude==list$lat & ctaloc$Longitude==list$lng)})
            stationname <- unique(ctastationname1()[["stationname"]])
            updateSelectInput(session, "stationname",selected = stationname)
            ggplot(ctastationname1(),aes(x=year(date),y=rides))+geom_bar(stat="identity",fill="steelblue")+
              labs(x = "Year", y ="Entries")+
              scale_y_continuous(labels = comma)+scale_x_continuous(breaks=c(2001:2021))
          }))
  
  # color coding the leaflet map
  ctared <- reactive({subset(cta(),cta()$RED=="TRUE")})
  ctablue <- reactive({subset(cta(),cta()$BLUE=="TRUE")})
  ctagreen <- reactive({subset(cta(),cta()$G=="TRUE")})
  ctabrown <- reactive({subset(cta(),cta()$BRN=="TRUE")})
  ctayellow <- reactive({subset(cta(),cta()$Y=="TRUE")})
  ctapink <- reactive({subset(cta(),cta()$Pnk=="TRUE")})
  ctaorange <- reactive({subset(cta(),cta()$O=="TRUE")})
  ctapurple <- reactive({subset(cta(),cta()$P=="TRUE" & cta()$Pexp=="TRUE")})
  
  # leaflet map output code
  output$map <- renderLeaflet({
    leaflet(options=leafletOptions(attributionControl=FALSE)) %>%
      setView(lng = -87.649707, lat = 41.875474, zoom = 11.5) %>% 
      addTiles() %>%
      addCircles(ctared()$Longitude,ctared()$Latitude,radius=25,color="red",group="Red",
                 label=paste("<strong>",ctared()$stationname,"</strong>","<br><strong>Rides:</strong>",
                                                      ctared()$rides) %>% 
                                         lapply(htmltools::HTML),
                                         labelOptions = labelOptions(noHide = T,direction = "bottom")) %>%
      addCircles(ctablue()$Longitude,ctablue()$Latitude,radius=25,color="blue",group="Blue",
                 label=paste("<strong>",ctablue()$stationname,"</strong>","<br><strong>Rides:</strong>",
                              ctablue()$rides) %>%
                 lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctagreen()$Longitude,ctagreen()$Latitude,radius=25,color="green",group="Green",
                 label=paste("<strong>",ctagreen()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctagreen()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctabrown()$Longitude,ctabrown()$Latitude,radius=25,color="brown",group="Brown",
                 label=paste("<strong>",ctabrown()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctabrown()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctayellow()$Longitude,ctayellow()$Latitude,radius=25,color="yellow",group="Yellow",
                 label=paste("<strong>",ctayellow()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctayellow()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctapink()$Longitude,ctapink()$Latitude,radius=25,color="pink",group="Pink",
                 label=paste("<strong>",ctapink()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctapink()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctaorange()$Longitude,ctaorange()$Latitude,radius=25,color="orange",group="Orange",
                 label=paste("<strong>",ctaorange()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctaorange()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addCircles(ctapurple()$Longitude,ctapurple()$Latitude,radius=25,color="purple",group="Purple",
                 label=paste("<strong>",ctapurple()$stationname,"</strong>","<br><strong>Rides:</strong>",
                             ctapurple()$rides) %>%
                   lapply(htmltools::HTML),
                 labelOptions = labelOptions(noHide = T,
                                             direction = "bottom")) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "B/w") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Railway") %>%
      addLayersControl(
        baseGroups = c("B/w", "Toner", "Railway"),
        overlayGroups = c("Red","Blue","Green","Brown","Yellow","Pink","Orange","Purple"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE))
  })
}

shinyApp(ui,server)