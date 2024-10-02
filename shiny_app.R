#setup ----
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(DT)
library(sf) 
library(leaflet)

#first read in the data and supporting shape files

#setwd("C:/Users/keith/OneDrive/Documents/MSC/00_RBootCamp")
data <- read.csv("datafinal.csv") 
stadt_zuri <- st_read("stzh.adm_statistische_quartiere_b_p.shp")  #point map
stadt_zuri_outline <- st_read("stzh.adm_statistische_quartiere_map.shp") #polygon map
datajoined <- data %>% left_join(stadt_zuri_outline, by = c("QuarLang" = "qname"))


#In addition, create an aggregated summary of the data for performance reasons
datasummarised <- data %>% 
  group_by(QuarLang, KreisLang) %>% 
  summarise(Apartments = sum(AnzWhg))

datajoinedchlor <- datasummarised %>% left_join(stadt_zuri_outline, by = c("QuarLang" = "qname"))




# ui----
ui <- fluidPage( titlePanel("Stadt Zürich Building Analysis"),
  sidebarLayout(
    
    sidebarPanel( width = 2,
                  h3("How to use this app"),
                  helpText("This dashboard gives an overview of building projects in the city of Zürich from 2009 up to 2021. 
                           The Kreis filter acts on all visual elements, with the year range selector only applied to the table and the bar chart."),
                  checkboxGroupInput("selectedkreis", h4("Please select a Kreis"), choices 
                  =list("Kreis 1","Kreis 2", "Kreis 3", "Kreis 4", "Kreis 5", "Kreis 6", "Kreis 7", "Kreis 8", "Kreis 9", "Kreis 10", "Kreis 11"), 
                  selected = list("Kreis 1","Kreis 2", "Kreis 3", "Kreis 4", "Kreis 5", "Kreis 6", "Kreis 7", "Kreis 8", "Kreis 9", "Kreis 10", "Kreis 11")),
                  
      sliderInput("selectedyear", h4("Select a Year Range"),
                  min = 2009, max = 2021, value = c(2009,2021), sep = "", ticks = FALSE)
    ), #seperate each UI element with a comment
    
    mainPanel( width = 8,
      
      
      # map (ui) ----
   fluidRow(
   column(5, leafletOutput(outputId = "bau_map")),
   column(7, plotOutput(outputId = "bau_bar"))  
   ),
      # datatable ui ----
      DTOutput(outputId = "bau_datatable")
      
    )
  )
)

#server ----
server <- function(input, output, session) {
  
  #barchart ----
  
 output$bau_bar <- renderPlot({
   #the selected kreis 
   chosenkreis = input$selectedkreis
   chosenyear = input$selectedyear
   chosenquartier = input$bau_map_shape_click
   
   selectedkreis <- datajoined %>%
     filter(KreisLang %in% chosenkreis, Jahr >= chosenyear[1], Jahr <= chosenyear[2])
   
   ggplot(data = selectedkreis, mapping = aes(x = Jahr, y = AnzWhg, fill = Eigentumsart))+
     geom_bar(stat='identity')+
     theme_bw()+
     scale_fill_brewer()+
     ggtitle("Yearly Development by Property Type") +
     xlab("Year") + ylab("# Apartments")
 })
  
 
  # map ----
  
  output$bau_map <- renderLeaflet({
    
    chosenkreis = input$selectedkreis
    chosenyear = input$selectedyear
    
    selectedkreis <- datajoinedchlor %>%
      filter(KreisLang %in% chosenkreis)
  

    bins <- c(0, 1000, 2000, 5000, 8000, 10000, 16000, Inf)
    pal <- colorBin("YlOrRd", domain = selectedkreis$Apartments, bins = bins)
    
    
    leaflet(st_transform(selectedkreis$geometry, "+init=epsg:4326")) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = pal(selectedkreis$Apartments),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = selectedkreis$QuarLang,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))   %>%
      setView(8.5317761, 47.373902, zoom = 11.8) 

#first version of the map with ggplot2       
#    ggplot() +
#      geom_sf(data = selectedkreis$geometry.y, fill = NA, aes(size = selectedkreis$AnzWhg, color = selectedkreis$KreisLang), shape = 19) +
#      geom_sf(data = stadt_zuri_outline, fill = NA) +
#      theme_bw()
    
  })
  
  
  # datatable (server) ----
  output$bau_datatable <- renderDT({
    chosenkreis = input$selectedkreis
    chosenyear = input$selectedyear
    
    datajoinedreduced <- datajoined %>%
      select(Jahr, KreisLang, QuarLang, ProjStatus, Eigentumsart, AnzWhg)
    
    selectedkreis <- datajoinedreduced %>%
      filter(KreisLang %in% chosenkreis, Jahr >= chosenyear[1], Jahr <= chosenyear[2])
    DT::datatable(data = selectedkreis,
                  rownames = FALSE, 
                  filter = "top",
                  class ='hover cell-border stripe',
                  extensions = 'Buttons',
                  colnames = c("Year", "Kreis Number", "Quartier", "Status", "Property Type", "# Apartments at Year End"),
                  options = list(dom='Bfrtip',
                                 buttons=c('copy', 'csv', 'excel', 'print', 'pdf')))
  })
}
# run app ----
shinyApp(ui, server)
