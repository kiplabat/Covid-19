library(dplyr)
library(reshape2)
library(ggplot2)
library(ggiraph)
library(backports)
library(zeallot)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(data.table)
library(dplyr)
library(ggplot2)
library(rvest)
library(httr)
library(htmltools)
library(feather)
library(tidyverse)
library(RCurl)
library(RSelenium)
library(stringr)
library(qdapRegex)
library(htm2txt)
library(httr)
library(tidyr)
library(DT)
library(leaflet.esri)
library(googledrive)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(shinyWidgets)

# setwd("E:/Google Drive/Companies/Covid-19")

con_deat_reco <- function(x){
  conf <- read.csv(url(x))
  
  conf <- conf[,-c(1,3,4)]
  
  library(data.table)
  
  conf$Country.Region <- gsub("Congo (Brazzaville)","Congo", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Congo (Kinshasa)","Democratic Republic of the Congo", conf$Country.Region,  fixed = T)
  conf$Country.Region <- gsub("US", "United States", conf$Country.Region, fixed = T)
  #conf$Country.Region <- gsub("Burma", "Myanmar", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Cabo Verde", "Cape Verde", conf$Country.Region, fixed = T)
  #conf$Country.Region <- gsub("Cote d'Ivoire", "Ivory Coast", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Czechia", "Czech Republic", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Diamond Princess", "Japan", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Eswatini", "Swaziland", conf$Country.Region, fixed = T)
  #conf$Country.Region <- gsub("Guinea-Bissau", "Guinea Bissau", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Holy See", "Vatican", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Syria", "Syrian Arab Republic", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Libya", "Libyan Arab Jamahiriya", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Tanzania", "United Republic of Tanzania", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Korea, South", "Korea, Republic of", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Korea, North", "Korea, Democratic People's Republic of", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("MS Zaandam", "United States", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("North Macedonia", "Macedonia", conf$Country.Region, fixed = T)
  #conf$Country.Region <- gsub("Serbia", "Republic of Serbia", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Taiwan*", "Taiwan", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Moldova", "Republic of Moldova", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Iran", "Iran (Islamic Republic of)", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Turkmenia", "Turkmenistan", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Laos", "Lao People's Democratic Republic", conf$Country.Region, fixed = T)
  conf$Country.Region <- gsub("Vietnam", "Viet Nam", conf$Country.Region, fixed = T)
  
  conf <- setDT(conf)[, lapply(.SD, sum), by = Country.Region]
  
  wor <- (as.data.frame(colnames(conf)))
  wor$World <- 0
  wor[1,2] <- c("World")
  
  m <- 2
  while (m <= length(conf)) {
    wor[m,2] <- sum(conf[,m, with=F]);
    m = m+1;
  }
  
  wor <- t(wor)
  colnames(wor) <- wor[1,]
  conf <- rbind(wor, conf)
  conf <- conf[2:nrow(conf),]
  colnames(conf) <- gsub("X", "", colnames(conf))
  conf <- t(conf)
  colnames(conf) <- conf[1,]
  conf <- conf[2:nrow(conf),]
  conf <- cbind(rownames(conf), conf)
  
  colnames(conf) <- c("X", colnames(conf[,-1]))
  rownames(conf) <- NULL
  
  conf <- as.data.frame(as.matrix(conf))
  
  n <- 2
  while (n <= ncol(conf)) {
    conf[,n] <- as.numeric(conf[,n]);
    n = n+1;
  }
  
  names(conf) <- gsub(x = names(conf), pattern = "\\.", replacement = " ")
  conf <- mutate(conf, X=as.Date(X, format = "%m.%d.%y"))
  
  
  #Find daily figures ---> copy the dataframe then update the copy
  conf2 <- conf
  # #While through the columns and rows to update each datapoint to correspond to daily observs.
  j <- 2
  while (j <= ncol(conf)) {
    k <- 2
    while (k <= nrow(conf)) {
      
      conf2[k,j] <- conf[k,j] - conf[k-1,j]
      k = k+1
    }
    j = j+1
  }
  conf <- conf2
  conf2 <- conf
  #Find weekly figures starting from the latest day backwards
  #get remainder value so as to divide by 7 days in a week
  l <- 2
  while (l <= ncol(conf)) {
    
    p <- nrow(conf)%%7
    conf2[p,l] <- sum(conf[1:p,l])
    q <- nrow(conf)%%7+7
    y <- c(p)
    while (q <= nrow(conf)) {
      x <- p+1
      conf2[q,l] <- sum(conf[x:q,l])
      p = p+7
      q = q+7
      y <- c(y,p)
    }
    l = l+1
  }
  conf2 <- conf2[y,]
  
  conf <- conf2
  
  return(conf)
}

get_conf <- function(){
  day <- gsub("\\. *","", Sys.Date())
  if(file.exists(paste("conf", day, ".csv", sep = ""))==TRUE){
    df <- fread(paste("conf", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }else{
    #df <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"))
    df <- con_deat_reco("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv")
    colnames(df) <- c("X", colnames(df[,-1]))
    
    do.call(file.remove, list(list.files(pattern = "\\.*csv", full.names = TRUE)))
    
    write.csv(df, paste("conf", day, ".csv", sep = ""), row.names = F)
    
    df <- fread(paste("conf", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }
  return(df)
}

get_deat <- function(){
  day <- gsub("\\. *","", Sys.Date())
  if(file.exists(paste("deat", day, ".csv", sep = ""))==TRUE){
    df <- fread(paste("deat", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }else{
    df <- con_deat_reco("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv")
    colnames(df) <- c("X", colnames(df[,-1]))
    write.csv(df, paste("deat", day, ".csv", sep = ""), row.names = F)
    
    df <- fread(paste("deat", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }
  return(df)
}

get_reco <- function(){
  day <- gsub("\\. *","", Sys.Date())
  if(file.exists(paste("reco", day, ".csv", sep = ""))==TRUE){
    df <- fread(paste("reco", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }else{
    df <- con_deat_reco("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recovered_global.csv")
    colnames(df) <- c("X", colnames(df[,-1]))
    write.csv(df, paste("reco", day, ".csv", sep = ""), row.names = F)
    
    df <- fread(paste("reco", day, ".csv", sep = ""))
    colnames(df) <- c("X", colnames(df[,-1]))
    #df[,1] <- as.Date(df[,1])
    df[,2:ncol(df)] <- lapply(df[,2:ncol(df)], as.numeric)
  }
  return(df)
}


conf <- get_conf()
deat <- get_deat()
reco <- get_reco()



map_dat <- function(df){
  conf2 <- data.frame(t(df))
  colnames(conf2) <- conf2[1,]
  conf2 <- conf2[-c(1,2),]
  conf2[, 1:ncol(conf2)] <- lapply(conf2[, 1:ncol(conf2)], as.numeric)
  conf2$Countries <- row.names(conf2)
  conf2$Countries <- gsub("\\."," ", conf2$Countries)
  #conf2 <- data.frame(cbind(conf2[["Countries"]], conf2[,1:(ncol(conf2)-1)]))
  #colnames(conf2) <- c("X", colnames(conf2[,-1]))
  rownames(conf2) <- NULL
  return(conf2)
}


manip_shp <- function(df){
  world_sp <- readOGR( 
    dsn= getwd(), 
    layer="TM_WORLD_BORDERS_SIMPL-0.3",
    verbose=FALSE
  )
  
  # world_sp <- gSimplify(world_sp, tol = 5)
  # world_sp@data$POP2005[ which(world_sp@data$POP2005 == 0)] = NA
  # world_sp@data$POP2005 <- as.numeric(as.character(world_sp@data$POP2005))
  world_sp@data <- left_join(world_sp@data, select(read.csv("population"), c("Country", "Pop")), by = c("NAME" = "Country"))
  world_sp@data <- left_join(world_sp@data, map_dat(df), by = c("NAME" = "Countries"))
  #world_sp@data$datew$datew <- as.numeric(world_sp@data$datew$datew)
  last_week <- conf[nrow(conf), 1][[1]]
  
  world_sp@data$perc <- as.numeric((100)*(world_sp@data[[as.character(last_week[1])]])/(world_sp@data$Pop))
  
  return(world_sp)
}

world_spdf <- manip_shp(conf)
world_spdg <- manip_shp(deat)
world_spdh <- manip_shp(reco)


library(shiny)

ui <- navbarPage("Latest Covid-19", id="nav",
           
           tabPanel("Confirmed Cases Per Capita Map",
                    div(class="outer",
                        tags$head(
                          # tags$link(rel="shortcut icon", href="Covid-19_icon.png"),
                          # includeHTML("<title>COVID-19 Stats</title>"),
                          # includeHTML('<meta name="Keywords" content = "World, Coronavirus, Covid-19, Statistics">'),
                          # includeHTML('<meta name="viewport" content="width=device-width, initial-scale=1.0">'),
                          includeCSS("styles.css"),
                        
                        ),
                    
                    leafletOutput("map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 70, right = 550, bottom = "auto", height = "auto", 
                                  
                                  sliderTextInput(inputId = "decade", 
                                                  label = "Week Ended:",
                                                  animate = F,
                                                  choices = c(as.list(as.character(conf$X))),
                                                  selected = conf$X[105],
                                                  width = "100%",
                                                  )
                                  ),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                  width = 500, height = "auto",
                                  
                                  h4(textOutput("confTable")),
                                  DT::dataTableOutput("confInfo"),
                                  # display static world stats only (graph)
                                  # output list of countries with numbers and percentages that can be sorted
                                  
                                  
                                 
                                  selectInput(
                                    'confirmed', 'Explore Country Confirmed Cases to Date', 
                                    choices = colnames(conf[,-1]),
                                    multiple = F,
                                    width = "100%"),
                                  
                                  
                                  h4(textOutput("countrystats")),
                                  
                                  
                                  plotOutput(
                                    'confplot',
                                    width = "100%", height = 230),
                                  
                                  ),
                    
                    
                    )
                    ),
           tabPanel("Deaths Per Capita",
                    div(class="outer",
                        tags$head(
                          # tags$link(rel="shortcut icon", href="Covid-19_icon.png"),
                          # includeHTML("<title>COVID-19 Stats</title>"),
                          # includeHTML('<meta name="Keywords" content = "World, Coronavirus, Covid-19, Statistics">'),
                          # includeHTML('<meta name="viewport" content="width=device-width, initial-scale=1.0">'),
                          includeCSS("styles.css"),
                          
                        ),
                        
                        leafletOutput("mapdeat", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 70, right = 550, bottom = "auto", height = "auto", 
                                      
                                      sliderTextInput(inputId = "deatt", 
                                                      label = "Week Ended:",
                                                      animate = F,
                                                      choices = c(as.list(as.character(conf$X))),
                                                      selected = deat$X[105],
                                                      width = "100%",
                                      )
                        ),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 500, height = "auto",
                                      
                                      h4(textOutput("deathsInfo")),
                                      DT::dataTableOutput("deathsTable"),
                                      # display static world stats only (graph)
                                      # output list of countries with numbers and percentages that can be sorted
                                      
                                      
                                      
                                      selectInput(
                                        'deaths', 'Explore Country Death Cases to Date', 
                                        choices = colnames(deat[,-1]), 
                                        selected = "Denmark",
                                        multiple = F,
                                        width = "100%"),
                                      
                                      
                                      h4(textOutput("countrydeaths")),
                                      
                                      
                                      plotOutput(
                                        'deathsplot',
                                        width = "100%", height = 230),
                                      
                        ),
                        
                        
                    )
                    ),
           
           tabPanel("Recoveries Per Capita",
                    div(class="outer",
                        tags$head(
                          # tags$link(rel="shortcut icon", href="Covid-19_icon.png"),
                          # includeHTML("<title>COVID-19 Stats</title>"),
                          # includeHTML('<meta name="Keywords" content = "World, Coronavirus, Covid-19, Statistics">'),
                          # includeHTML('<meta name="viewport" content="width=device-width, initial-scale=1.0">'),
                          includeCSS("styles.css"),
                          
                        ),
                        
                        leafletOutput("mapreco", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 70, right = 550, bottom = "auto", height = "auto", 
                                      
                                      sliderTextInput(inputId = "recos", 
                                                      label = "Week Ended:",
                                                      animate = F,
                                                      choices = c(as.list(as.character(conf$X))),
                                                      selected = reco$X[105],
                                                      width = "100%",
                                      )
                        ),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 500, height = "auto",
                                      
                                      h4(textOutput("recoveriesInfo")),
                                      DT::dataTableOutput("recoveriesTable"),
                                      # display static world stats only (graph)
                                      # output list of countries with numbers and percentages that can be sorted
                                      
                                      
                                      
                                      selectInput(
                                        'recoveries', 'Explore Country Recoveries to Date', 
                                        choices = colnames(reco[,-1]), 
                                        selected = "Denmark",
                                        multiple = F,
                                        width = "100%"),
                                      
                                      
                                      h4(textOutput("countryrecoveries")),
                                      
                                      
                                      plotOutput(
                                        'recoveriesplot',
                                        width = "100%", height = 230),
                                      
                        ),
                        
                        
                    )
           ),
)


server <- function(input, output) {
  # IP <- reactive({input$getIP})
  # 
  # observe({
  #   cat(capture.output(str(IP), split=TRUE))
  #   })
  
  #----------------Tab1-----Confirmed----------------------
  
  mapDat <- reactive({
    world_spdf@data$perc <- as.numeric((100)*(world_spdf@data[[as.character(input$decade)]])/(world_spdf@data$Pop))
    world_spdf
  })
  
  # Map with margins bounded
  output$map <- renderLeaflet({
    mybins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, Inf)
    # last_week <- conf[nrow(conf), 1][[1]]
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin( palette="Blues", domain = world_spdf@data$perc, na.color = "transparent", bins=mybins)
    
    leaflet(world_spdf) %>%
      setView(lng = 80, lat = 35, zoom = 2) %>%
      addLegend( pal = mypalette, values = ~world_spdf@data$perc,
                 opacity = 0.95,
                 title = "% Population Infected", position = "bottomleft")
  })
  
  #Use observer to dynamically clear and update map
  observe({
    mybins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, Inf)
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin( palette="Blues", domain = mapDat()@data$perc, na.color = "transparent", bins=mybins)
    
    mytext <- paste(
      "Country: ", mapDat()@data$NAME,"<br/>", 
      "Confirmed Infections: ", paste(mapDat()@data[[as.character(input$decade)]]), "<br/>", 
      "% Population Infected: ", paste(round(mapDat()@data$perc, 1), "%", sep = ""), "<br/>",
      "Week Ended: ", input$decade, "<br/>", 
      "Population: ", mapDat()@data$Pop, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leafletProxy("map", data = mapDat()) %>%
      # clearShapes() %>%
      addTiles()  %>%
      # setView(lng = 0, lat = 0, zoom = 1) %>%
      addPolygons( 
        fillColor = ~mypalette(mapDat()@data$perc), 
        stroke=TRUE, 
        fillOpacity = 1, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      )
    })
  
  output$confTable <- renderText({paste("Highest Infections: Week Ended", input$decade)})
  
  output$confInfo <- DT::renderDataTable({
    df <- select(mapDat()@data, c("NAME", "perc", input$decade))
    df <- df[order(df$perc, decreasing = T),]
    df$perc <- round(df$perc, 1)
    row.names(df) <- df$NAME
    df <- df[,-1]
    colnames(df) <- c("% Population Infected", "People Infected")
    DT::datatable(df,
                  extensions = c('Scroller'),
                  options = list(
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 150,
                    scroller = T))
  })
  
  output$countrystats = renderText({
    paste(input$confirmed, "Confirmed Weekly Cases")
  })
  
  output$confplot = renderPlot({
    plot.data <- select(conf, c("X", as.character(input$confirmed)))
    #plot.data <- merge(plot.data,reco[,c("X",input$country)],by.x = "X", by.y = "X")
    colnames(plot.data) <- c("X", "confirmed")
    #
    # plot.data <- reshape2::melt(plot.data, id.vars = 'X')
    #
    ggplot(plot.data) +
      geom_line(mapping = aes(x = X, y = confirmed), color = "blue") +
      labs (x = "Dates", y = "Total Weekly Cases")
  })
  
  
  #--------------------------Tab2---deaths---------------------------
  
  mapDatd <- reactive({
    world_spdg@data$perc <- as.numeric((100)*(world_spdg@data[[as.character(input$deatt)]])/(world_spdg@data$Pop))
    world_spdg
  })
  
  # Map with margins bounded
  output$mapdeat <- renderLeaflet({
    mybins <- c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.015, 0.02, 0.03, Inf)
    # last_week <- conf[nrow(conf), 1][[1]]
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin( palette="Reds", domain = world_spdg@data$perc, na.color = "transparent", bins=mybins)
    
    leaflet(world_spdg) %>%
      setView(lng = 80, lat = 35, zoom = 2) %>%
      addLegend( pal = mypalette, values = ~world_spdg@data$perc,
                 opacity = 0.95,
                 title = "% Population Deaths", position = "bottomleft")
  })
  
  #Use observer to dynamically clear and update map
  observe({
    mybins <- c(0, 0.002, 0.004, 0.006, 0.008, 0.01, 0.015, 0.02, 0.03, Inf)
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin( palette="Reds", domain = mapDatd()@data$perc, na.color = "transparent", bins=mybins)
    
    mytext <- paste(
      "Country: ", mapDatd()@data$NAME,"<br/>", 
      "Deaths: ", paste(mapDatd()@data[[as.character(input$deatt)]]), "<br/>", 
      "% Deaths: ", paste(round(mapDatd()@data$perc, 3), "%", sep = ""), "<br/>",
      "Week Ended: ", input$deatt, "<br/>", 
      "Population: ", mapDatd()@data$Pop, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leafletProxy("mapdeat", data = mapDatd()) %>%
      # clearShapes() %>%
      addTiles()  %>%
      # setView(lng = 0, lat = 0, zoom = 1) %>%
      addPolygons( 
        fillColor = ~mypalette(mapDatd()@data$perc), 
        stroke=TRUE, 
        fillOpacity = 1, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      )
  })
  
  output$deathsInfo <- renderText({paste("Highest Deaths: Week Ended", input$deatt)})
  
  output$deathsTable <- DT::renderDataTable({
    df <- select(mapDatd()@data, c("NAME", "perc", input$deatt))
    df <- df[order(df$perc, decreasing = T),]
    df$perc <- round(df$perc, 3)
    row.names(df) <- df$NAME
    df <- df[,-1]
    colnames(df) <- c("% Population Deaths", "Deaths")
    DT::datatable(df,
                  extensions = c('Scroller'),
                  options = list(
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 150,
                    scroller = T))
  })
  
  output$countrydeaths = renderText({
    paste(input$deaths, "Weekly Deaths")
  })
  
  output$deathsplot = renderPlot({
    plot.data <- select(deat, c("X", as.character(input$deaths)))
    #plot.data <- merge(plot.data,reco[,c("X",input$country)],by.x = "X", by.y = "X")
    colnames(plot.data) <- c("X", "deaths")
    #
    # plot.data <- reshape2::melt(plot.data, id.vars = 'X')
    #
    ggplot(plot.data) +
      geom_line(mapping = aes(x = X, y = deaths), color = "red") +
      labs (x = "Dates", y = "Total Weekly Deaths")
  })
  
  #--------------------Tab3-------Recoveries------------
  mapDatr <- reactive({
    world_spdh@data$perc <- as.numeric((100)*(world_spdh@data[[as.character(input$recos)]])/(world_spdh@data$Pop))
    world_spdh
  })
  
  # Map with margins bounded
  output$mapreco <- renderLeaflet({
    mybins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, Inf)
    # last_week <- conf[nrow(conf), 1][[1]]
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin( palette="Greens", domain = world_spdh@data$perc, na.color = "transparent", bins=mybins)
    
    leaflet(world_spdh) %>%
      setView(lng = 80, lat = 35, zoom = 2) %>%
      addLegend( pal = mypalette, values = ~world_spdh@data$perc,
                 opacity = 0.95,
                 title = "% Population Recovery", position = "bottomleft")
  })
  
  #Use observer to dynamically clear and update map
  observe({
    mybins <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, Inf)
    # mybins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
    mypalette <- colorBin(palette="Greens", domain = mapDatr()@data$perc, na.color = "transparent", bins=mybins)
    
    mytext <- paste(
      "Country: ", mapDatr()@data$NAME,"<br/>", 
      "Recoveries: ", paste(mapDatr()@data[[as.character(input$recos)]]), "<br/>", 
      "% Population Recovered: ", paste(round(mapDatr()@data$perc, 1), "%", sep = ""), "<br/>",
      "Week Ended: ", input$recos, "<br/>", 
      "Population: ", mapDatr()@data$Pop, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    leafletProxy("mapreco", data = mapDatr()) %>%
      # clearShapes() %>%
      addTiles()  %>%
      # setView(lng = 0, lat = 0, zoom = 1) %>%
      addPolygons( 
        fillColor = ~mypalette(mapDatr()@data$perc), 
        stroke=TRUE, 
        fillOpacity = 1, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      )
  })
  
  output$recoveriesInfo <- renderText({paste("Highest Recoveries: Week Ended", input$recos)})
  
  output$recoveriesTable <- DT::renderDataTable({
    df <- select(mapDatr()@data, c("NAME", "perc", input$recos))
    df <- df[order(df$perc, decreasing = T),]
    df$perc <- round(df$perc, 3)
    row.names(df) <- df$NAME
    df <- df[,-1]
    colnames(df) <- c("% Population Recovered", "People Recovered")
    DT::datatable(df,
                  extensions = c('Scroller'),
                  options = list(
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 150,
                    scroller = T))
  })
  
  output$countryrecoveries = renderText({
    paste(input$recoveries, "Weekly Recoveries")
  })
  
  output$recoveriesplot = renderPlot({
    plot.data <- select(conf, c("X", as.character(input$recoveries)))
    #plot.data <- merge(plot.data,reco[,c("X",input$country)],by.x = "X", by.y = "X")
    colnames(plot.data) <- c("X", "recoveries")
    #
    # plot.data <- reshape2::melt(plot.data, id.vars = 'X')
    #
    ggplot(plot.data) +
      geom_line(mapping = aes(x = X, y = recoveries), color = "green") +
      labs (x = "Dates", y = "Total Weekly Recoveries")
  })
  
  

  
  
}

shinyApp(ui = ui, server = server)

#-----------------PART OF OLD UI---------------------------------

# 
# 
# h1(strong(textOutput('CountryName')), align ="center"),
# selectInput(
#   'country', 'Country:', 
#   choices = colnames(conf[,-1]), 
#   selected = "World",
#   multiple = F),
# # tableOutput("table2"),
# checkboxGroupInput("cnt",
#                    "Select graphs to show: ",
#                    choices = c('Confirmed', 
#                                'Deaths'),
#                    selected = c('Confirmed', 'Deaths'),
#                    inline = TRUE
# ),
# tags$hr(),
# 
# plotOutput(
#   'lineplot',
#   width = "100%"), 
# 







#     sliderTextInput(inputId = "decade", 
#                     label = "Week:",
#                     animate = T,
#                     choices = c(as.list(as.character(conf$X))))
#   ),
#   fluidRow(id = "fluidRow2",
#            # Display for maps and a timeline plot
#            column(6,
#                   leafletOutput("map"),
#            ),
#            column(6,
#                   fluidRow(id = "fluidRow2",
#                            plotOutput(
#                              'lineplot',
#                              width = "100%"),
#                   ),
#            ),
#   ),
#   fluidRow(
#     column(12, align = "center",
#            fluidRow(
#              tags$em("Build and deploy your data driven web applications with ",
#                      HTML
#                      ('<a href = "https://shiny.rstudio.com/tutorial/" target ="_blank">R Shiny and R Studio</a>'),
#                      " in minutes.", align ="center"
#              ),
#              
#            ),
#            tags$hr(),
#            fluidRow(
#              h1(strong(textOutput('CountryName')), align ="center"),
#              selectInput(
#                'country', 'Country:', 
#                choices = colnames(conf[,-1]), 
#                selected = "World",
#                multiple = F),
#              # tableOutput("table2"),
#              checkboxGroupInput("cnt",
#                                 "Select graphs to show: ",
#                                 choices = c('Confirmed', 
#                                             'Deaths'),
#                                 selected = c('Confirmed', 'Deaths'),
#                                 inline = TRUE
#              ),
#              tags$hr(),
#              
#              
#              
#              tags$hr(),
#              
#              h2("Country stats"),
#              strong(textOutput('Cfat')),
#              strong(textOutput('Cconf')),
#              strong(textOutput('Cconfs')),
#              strong(textOutput('Cdeat')),
#              strong(textOutput('Cdeats')),
#              strong(textOutput('Creco')),
#              strong(textOutput('Crecos')),
#              
#              
#              h2("Worldwide stats"),
#              strong(textOutput('Wfat')),
#              strong(textOutput('Wconf')),
#              strong(textOutput('Wconfs')),
#              strong(textOutput('Wdeat')),
#              strong(textOutput('Wdeats')),
#              strong(textOutput('Wreco')),
#              strong(textOutput('Wrecos')),
#              
#            ),
#            tags$hr(),
#            fluidRow(
#              tags$em("Tarus Kiplabat | R Shiny apps | R version 3.6.3 (2020-02-29) | dplyr(1.0.1), reshape2(1.4.4), ggplot2(3.3.2), ggiraph(0.7.8),shiny(1.50), zeallot (0.1.0) | Data:Johns Hopkins University"),
#              tags$p("Follow me on ", HTML('
#              <a href="https://www.linkedin.com/in/labatt" target="_blank">LinkedIn</a>
#              '))
#            )
#     )
#   )
#   
# )

#backports::import(NULL)






#------------------PART OF OLD SERVER----------------------------

# output$Wfat <- renderText({paste0("Worldwide fatality rate: ", round((sum(deat$World)*100)/(sum(conf$World)), digits = 2),"%")})
# output$Wconf <- renderText({paste("Last 7-day cases: ", format(conf[nrow(conf),"World"], big.mark = ","))})
# output$Wconfs <- renderText({paste("Total cases: ", format(sum(conf$World), big.mark = ","))})
# output$Wdeat <- renderText({paste("Last 7-day deaths: ", format(deat[nrow(deat),"World"], big.mark = ","))})
# output$Wdeats <- renderText({paste("Total deaths: ", format(sum(deat$World), big.mark = ","))})
# output$Wreco <- renderText({paste("Last 7-day recoveries: ", format(reco[nrow(reco),"World"], big.mark = ","))})
# output$Wrecos <- renderText({paste("Total recoveries: ", format(sum(reco$World), big.mark = ","))})
# 
# output$Cfat <- renderText({paste0("Country fatality rate: ", round((sum(deat[,input$country])*100)/(sum(conf[,input$country])), digits = 2), "%")})
# output$Cconf <- renderText({paste("Last 7-day cases: ", format(conf[nrow(conf),input$country], big.mark = ","))})
# output$Cconfs <- renderText({paste("Total cases: ", format(sum(conf[,input$country]), big.mark = ","))})
# output$Cdeat <- renderText({paste("Last 7-day deaths: ", format(deat[nrow(deat),input$country], big.mark = ","))})
# output$Cdeats <- renderText({paste("Total deaths: ", format(sum(deat[,input$country]), big.mark = ","))})
# output$Creco <- renderText({paste("Last 7-day recoveries: ", format(reco[nrow(reco),input$country], big.mark = ","))})
# output$Crecos <- renderText({paste("Total recoveries: ", format(sum(reco[,input$country]), big.mark = ","))})
# output$CountryName <- renderText({paste0(input$country, " Coronavirus Statistics")})
# 
# # output$table2 = renderTable({
# #   x <- as.data.frame(conf[nrow(conf),input$country]-conf[nrow(conf)-1,input$country])
# #   colnames(x) <- c("date")
# # })
# 
# 
# 
# output$lineplot = renderPlot({
#   plot.data <- merge(conf[,c("X",input$country)],deat[,c("X",input$country)],by.x = "X", by.y = "X")
#   #plot.data <- merge(plot.data,reco[,c("X",input$country)],by.x = "X", by.y = "X")
#   colnames(plot.data) <- c("X","Confirmed", "Deaths")
#   
#   plot.data <- reshape2::melt(plot.data, id.vars = 'X')
#   
#   plot.data <- plot.data[plot.data$variable %in% input$cnt, ]
#   ggplot(plot.data) +
#     geom_line(mapping = aes(x = X, y = value, colour = variable)) + 
#     labs (x = "Dates", y = "Total Weekly Cases") + 
#     scale_colour_discrete(name = "KEY: ") +
#     theme(legend.position="top")
# })

