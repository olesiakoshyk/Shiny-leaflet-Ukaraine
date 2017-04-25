library(shiny)
library(leaflet)
library(readxl)
library(shinydashboard)
library(graphics)
library(googleVis)



UkrStat <- read_excel("D:/My downloads/Downloads/R Studio/UkrStat.xlsx")

pal <- colorNumeric(
  palette = "#FFA500",
  domain = UkrStat$Pop)

contentPopup <- as.character(tagList(
  tags$b(UkrStat$Region), ":", br(), as.numeric(UkrStat$Pop)))


ui <- navbarPage(
  "Population of Ukraine", id="nav",
  tabPanel("Map", div(class = "outer",
                      tags$head(
                        includeCSS("D:/My downloads/Downloads/R Studio/style.css")),
                      leafletOutput("CountryMap", width = "100%", height = "100%"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    selectInput("region", label = h4("Region"), choices = c("", UkrStat$Region), selected = "", width = "90%"),
                                    htmlOutput("pieChart")
                      ))),
  tabPanel("About",
           box(title = "About this app", includeHTML("D:/My downloads/Downloads/R Studio/include.html"), width = "100%"),
           box(width  = "100%", height = 600,
               leafletOutput("MiniMap")
           ))
)




server <- function(input, output, session){
  output$CountryMap <- renderLeaflet({
    leaflet() %>% addTiles() %>% addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lng = 31.165580, lat = 48.379433, zoom = 6) %>%
      addCircles(lng = as.numeric(UkrStat$Longtitude), lat = as.numeric(UkrStat$Latitude), weight = 1, radius = sqrt(UkrStat$Pop)*30, popup = contentPopup, color = "#FFA500", fillOpacity = UkrStat$Opacity) %>%
      addLegend("bottomleft", pal = pal, values = UkrStat$Pop, title = "Population in Regions") %>%
      
      #Easy buttons code
      
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #Action on selectInput
  
  observeEvent(input$region, {
    
    if(input$region != "")
    {
      leafletProxy("CountryMap") %>% clearShapes() %>% clearPopups()
      index = which(UkrStat$Region == input$region)
      leafletProxy("CountryMap") %>% addCircles(lng = as.numeric(UkrStat$Longtitude[index]), lat = as.numeric(UkrStat$Latitude[index]), weight = 1, radius = sqrt(UkrStat$Pop[index])*30, color = "#FFA500", fillOpacity=0.8) %>%
        addPopups(lng = as.numeric(UkrStat$Longtitude[index]), lat = as.numeric(UkrStat$Latitude[index]), popup = paste(contentPopup[index]))
    } else {
      leafletProxy("CountryMap") %>% clearMarkers() %>% clearPopups() %>% addCircles(lng = as.numeric(UkrStat$Longtitude), lat = as.numeric(UkrStat$Latitude), weight = 1, radius = sqrt(UkrStat$Pop)*30, popup = contentPopup, color = "#FFA500", fillOpacity = UkrStat$Opacity)
    }
  })
  
  output$pieChart <- renderGvis({
    pie1 <- gvisPieChart(UkrStat, labelvar = "Region", numvar = "Pop", options=list(title = "Population in Slices",width=300, height=350, colors="['#FFA500']", legend='none'))
    
  })
  output$MiniMap <- renderLeaflet({
    leaflet() %>% addTiles() %>% addProviderTiles("Esri.WorldStreetMap") %>%
      setView(lng = 31.165580, lat = 48.379433, zoom = 6) %>% addCircles(lng = as.numeric(UkrStat$Longtitude), lat = as.numeric(UkrStat$Latitude), weight = 1, radius = sqrt(UkrStat$Pop)*30, popup = paste(UkrStat$Region, ", ", UkrStat$Pop), color = "#FFA500", fillOpacity = UkrStat$Opacity)
  })
}








# Run the application 
shinyApp(ui = ui, server = server)

