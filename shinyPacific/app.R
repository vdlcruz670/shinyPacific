#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(rsdmx)
library(tidyverse)
library(sf)

# creating the map
pac_geo_data <- st_read('/Users/victoriadelacruz/Desktop/R/Projects/shinyPacific/data/Pacific islands region EEZs .geojson')
pac_geo_data_proj <- st_transform(pac_geo_data, crs = 3832)

pac_geo_data_proj$label_size <- ifelse(pac_geo_data_proj$country %in% c("American Samoa", "Samoa", "Wallis et Futuna"), 2, 3)

# merging sdg 2 data
## loading in from pdh api
sdg2 <- readSDMX(providerId = "PDH", resource = "data", flowRef = "DF_SDG_02")
sdg2_data <- as.data.frame(sdg2)

## creating lookup list for pac map and geo_pict
pict_country_lookup <- data.frame(
  GEO_PICT = c("NR", "MH", "WS", "GU", "CK", "SB", "KI", "FJ", "PG", "TO", "MP", "NC", "VU", "TV", "NU", "PF", "PW", "FM"),
  country = c("Nauru", "Marshall Islands", "Wallis et Futuna", "Guam", "Cook Islands", "Solomon Islands", "Kiribati", "Fiji", "Papua New Guinea", "Tonga", "Northern Mariana Islands", "New Caledonia", "Vanuatu", "Tuvalu", "Niue", "Polynesie Francaise", "Palau", "Micronesia")
)

## merging data
sdg2_data_merged <- merge(sdg2_data, pict_country_lookup, by = "GEO_PICT")

sdg2_data_merged$obsTime <- as.numeric(sdg2_data_merged$obsTime) # converting years to numeric

ui <- fluidPage(
  titlePanel("Pacific Islands SDG 2 Indicators"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select Indicator:", choices = unique(sdg2_data_merged$INDICATOR)),
      sliderInput("year", "Select Year:", 
                  min = min(sdg2_data_merged$obsTime), max = max(sdg2_data_merged$obsTime), 
                  value = min(sdg2_data_merged$obsTime)),
      selectInput("age", "Age Group:", choices = unique(sdg2_data_merged$AGE)),
      selectInput("income", "Income Level:", choices = unique(sdg2_data_merged$INCOME)),
      selectInput("urbanization", "Urbanization:", choices = unique(sdg2_data_merged$URBANIZATION)),
      selectInput("education", "Education Level:", choices = unique(sdg2_data_merged$EDUCATION)),
      selectInput("occupation", "Occupation:", choices = unique(sdg2_data_merged$OCCUPATION)),
      selectInput("composite_breakdown", "Composite Breakdown:", choices = unique(sdg2_data_merged$COMPOSITE_BREAKDOWN)),
      selectInput("disability", "Disability Status:", choices = unique(sdg2_data_merged$DISABILITY))
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

server <- function(input, output) {
  # Filter data based on inputs
  filtered_data <- reactive({
    sdg2_data_merged %>%
      filter(
        INDICATOR == input$indicator,
        obsTime == input$year,
        AGE == input$age,
        INCOME == input$income,
        URBANIZATION == input$urbanization,
        EDUCATION == input$education,
        OCCUPATION == input$occupation,
        COMPOSITE_BREAKDOWN == input$composite_breakdown,
        DISABILITY == input$disability
      )
  })
  
  filtered_data_with_geometry <- reactive({
    filtered_data() %>%
      inner_join(pac_geo_data_proj, by = "country")
  })
  
  # Render the map plot
  output$mapPlot <- renderPlot({
    ggplot(pac_geo_data_proj) +
      geom_sf(data = filtered_data_with_geometry(), aes(geometry = geometry, fill = obsValue)) +
      geom_sf_label(data = filtered_data_with_geometry(), aes(geometry = geometry, label = country)) +
      scale_fill_viridis_c(option = "plasma") +
      theme_minimal() +
      labs(title = paste("SDG 2 -", input$indicator, "in", input$year))
  })
}

# Defining server function

# Create shiny app function
shinyApp(ui, server)