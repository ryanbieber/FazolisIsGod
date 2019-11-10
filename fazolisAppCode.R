library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  textInput(inputId = "map",
            label = "Put in the address you want to see how far it is away from Fazolis and Taco Bell",
            value = "", width = "100%", placeholder = "1600 Pennsylvania Avenue NW Washington, DC 20500"),
  actionButton("goButton", "Pleast tell me!"),
  plotOutput("faz"),
  textOutput("dist")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  library(ggmap)
  library(ggplot2)
  library(dplyr)
  library(geosphere)
  library(ggrepel)
  register_google(key = "API KEY")
  origin <- data.frame(lon = -92.50606, lat = 44.08087)
  originm <- as.matrix(origin)
  observeEvent(
    eventExpr = input[["goButton"]],
    handlerExpr = {
      print("PRESSED") #simulation code can go here
      latlong <- geocode(input$map)
      df <- rbind(origin, latlong)
      
      locs <- c("The center of the universe", "You are HERE")
      df <- cbind(df, locs)
      
      distToOrigin <- distm(originm, as.matrix(latlong), fun = distVincentyEllipsoid)
      distToOriginMiles <- distToOrigin*.000621
      
      zoom <- 3
      ifelse(distToOriginMiles <= 60, zoom <- 9,
             ifelse(distToOriginMiles <= 140 & distToOriginMiles > 60, zoom <- 8,
                    ifelse(distToOriginMiles <= 210 & distToOriginMiles > 140, zoom <- 7,
                           ifelse(distToOriginMiles <= 350 & distToOriginMiles > 210, zoom <- 6,
                                  ifelse(distToOriginMiles <= 600 & distToOriginMiles > 350, zoom <- 5,
                                         ifelse(distToOriginMiles <= 1000 & distToOriginMiles > 600, zoom <- 4,
                                                ifelse(distToOriginMiles > 1000 , zoom <- 3, zoom <- 3)))))))
      
      maploc = get_map(location = c(lon = latlong$lon, lat = latlong$lat), maptype = "terrain", source = "google", zoom = zoom)
      map = ggmap(maploc, extent = "device")
      output$faz <- renderPlot({
        map +
          geom_point(aes(x = origin$lon, y = origin$lat), color = 'red', alpha = 1, data = df) + 
          geom_point(aes(x = latlong$lon, y = latlong$lat), color = 'blue' , alpha = 1) + 
          geom_segment(x = origin$lon, y = origin$lat, xend = latlong$lon, yend = latlong$lat) +
          geom_label_repel(
            aes(x = lon, y = lat, label = locs),
            data=df,
            family = 'Times', 
            size = 3, 
            box.padding = 0.1, point.padding = 0.1,
            segment.color = 'grey50')
      })
      output$dist <- renderText(paste("You are", round( distToOriginMiles, 2), "Miles from Fazoils", sep = " "))
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

