library(shiny)
library(dplyr)
library(leaflet)
library(rworldmap)
library(tidyverse)
library(here)

# 1.read data--------------------
raw_data <- read.csv(here::here("raw data.csv"), sep = ";",
                     check.names = FALSE, stringsAsFactors = FALSE)

# 2.data cleaning----------------
clean_data <- raw_data %>%
  mutate(Location = trimws(Location)) %>%
  mutate(across(`1990`:`2050`, ~ as.numeric(gsub("[^0-9.]", "", .))))

ui <- fluidPage(
  titlePanel("Neoplasm Deaths by Country (1990-2050)"),
  tabsetPanel(
    tabPanel("Line Graph",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country_line", "Select countries:", 
                             choices = sort(unique(clean_data$Location)), 
                             selected = "Afghanistan",
                             multiple = TRUE),
                 helpText("Multiple selection possible")
               ),
               mainPanel(
                 plotOutput("deathPlot", height = "600px")
               )
             )
    ),
    tabPanel("Heatmap",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country_heat", "Select countries:", 
                             choices = sort(unique(clean_data$Location)), 
                             selected = "Afghanistan",
                             multiple = TRUE),
                 helpText("Multiple selection possible")
               ),
               mainPanel(
                 plotOutput("heatmap", height = "600px")
               )
             )
    ),
    tabPanel("World Map",
             leafletOutput("map", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  # Data in long format
  long_data <- reactive({
    clean_data %>%
      pivot_longer(cols = -Location, names_to = "Year", values_to = "Deaths") %>%
      mutate(Year = as.numeric(Year), Deaths = as.numeric(Deaths))
  })
  
  # Filtered data for line graph (multiple countries)
  filtered_line <- reactive({
    req(input$country_line)
    long_data() %>% filter(Location %in% input$country_line)
  })
  
  # Filtered data for heatmap (multiple countries)
  filtered_heat <- reactive({
    req(input$country_heat)
    long_data() %>% filter(Location %in% input$country_heat)
  })
  
  # World map data - change 2020 to 2050 percent
  map_data <- reactive({
    change_df <- clean_data %>%
      filter(!is.na(`2020`), !is.na(`2050`), `2020` != 0) %>%
      mutate(change = ((`2050` - `2020`) / `2020`) * 100) %>%
      select(Location, change)
    
    joinCountryData2Map(change_df,
                        joinCode = "NAME",
                        nameJoinColumn = "Location",
                        verbose = FALSE)
  })
  
  # Render line graph (multiple countries, color by Location)
  output$deathPlot <- renderPlot({
    data <- filtered_line()
    ggplot(data, aes(x = Year, y = Deaths, color = Location)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_vline(xintercept = 2020, linetype = "dashed", color = "darkred") +
      scale_x_continuous(breaks = unique(data$Year)) +
      labs(title = paste("Neoplasm Deaths Over Time:", paste(input$country_line, collapse = ", ")),
           x = "Year", y = "Number of Deaths", color = "Country") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # Render heatmap (multiple countries)
  output$heatmap <- renderPlot({
    data <- filtered_heat()
    ggplot(data, aes(x = factor(Year), y = Location, fill = Deaths)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
      labs(title = paste("Heatmap of Neoplasm Deaths:", paste(input$country_heat, collapse = ", ")),
           x = "Year", y = "Country", fill = "Deaths") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # Render world map
  output$map <- renderLeaflet({
    mapdata <- map_data()
    pal <- colorNumeric("YlOrRd", domain = mapdata@data$change, na.color = "transparent")
    
    leaflet(mapdata) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(change),
        weight = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME, ": ", round(change, 1), "% change"),
        labelOptions = labelOptions(direction = "auto", textsize = "14px")
      ) %>%
      addLegend(pal = pal,
                values = ~change,
                title = "% Change in Deaths (2020 → 2050)",
                labFormat = labelFormat(suffix = "%", digits = 1),
                opacity = 0.8,
                position = "bottomright")
  })
  
}

shinyApp(ui, server)





