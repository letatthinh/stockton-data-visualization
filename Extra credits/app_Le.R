# Load required libraries
library(shiny)         # Create interactive dashboard
library(bslib)         # Create interactive dashboard
library(readxl)        # Read excel files
library(leaflet)       # Plot maps
library(plotly)

# Application: bicycles-and-perdestrian-counts
# ------------------------------------------------------------------------------
df <- read_excel('Datasets.xlsx', sheet = 'Bicycle_Pedestrian_Counts')

df <- df %>%
  rename(
    "AMPM" = `AM/PM`,
    "LocationName" = `Location Name`
  )

# Add a new column for count level to use as radius
df$Count_radius <- cut(
  df$Count,
  breaks = seq(0, 1600, length.out = 17),  # Create 15 intervals
  labels = 1:16,                           # Assign levels 1 to 15
  include.lowest = TRUE                    # Include the lowest boundary
)

create_label <- function(label) {
  return(paste0("<b>", label, ":</b>"))
}

# For a single-page dashboards with an optional sidebar.
# https://rstudio.github.io/bslib/reference/index.html#dashboard-layouts
ui <- shinyUI(page_sidebar(
  # Application title
  title = "Count of Bicycles and Pedestrians",

  # Sidebar
  sidebar = sidebar(
    tags$p(paste(
      "Welcome! This dashboard showcases the count of bicycles and pedestrians",
      "across various places and years, highlighting trends and patterns"
    )),
    sliderInput(
      inputId = "year",
      label = HTML(create_label("Year range")),
      min = min(df$Year),
      max = max(df$Year),
      sep = "",
      value = c(min(df$Year), max(df$Year))
    ),
    div(
      selectizeInput(
        inputId = "ampm",
        label = HTML(create_label("AM/PM")),
        choices = unique(df$AMPM),
        options = list(placeholder = "Please select")
      ),
      selectInput(
        inputId = "mode",
        label = HTML(create_label("Mode")),
        choices = unique(df$Mode)
      ),
      class = "d-flex column-gap-3"
    ),
    width = 320
  ),

  # Main content
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Top 10 locations with highest count", class = "bg-primary"),
      card_body(plotlyOutput("topLocations"))
    ),
    card(
      card_header("Distribution of bikes or pedestrians by locations",
                  class = "bg-primary"
      ),
      card_body(leafletOutput("map"), class = "p-0")
    )
  )
))

# ------------------------------------------------------------------------------

# Define server logic
server <- shinyServer(function(input, output, session) {
  filtered_df <- reactive({
    df %>%
      filter(
        Year >= input$year[1],
        Year <= input$year[2],
        AMPM == input$ampm,
        Mode == input$mode
      )
  })
  
  grouped_df <- reactive({
    filtered_df() %>%
      group_by(LocationName, Longitude, Latitude) %>%
      summarise(TotalCount = sum(Count))
  })
  
  # Price analysis by listing type
  output$topLocations <- renderPlotly({
    top_locations_df <- grouped_df() %>%
      arrange(desc(TotalCount)) %>%
      head(n = 10)
    
    # Define colors
    hex_A <- "#E31A1C" # Color for the highest count bar
    hex_B <- "#007BC2" # Color for all other bars
    
    # Identify the highest count and assign colors
    top_locations_df$Color <- ifelse(
      top_locations_df$TotalCount == max(top_locations_df$TotalCount), 
      hex_A, 
      hex_B)
      
    
    # Create bar chart using Plotly
    plot_ly(
      top_locations_df,
      x = ~LocationName ,
      y = ~TotalCount,
      type = "bar",
      marker = list(color = ~Color)  # Apply color dynamically
    ) %>%
      layout(
        xaxis = list(title = "Place"),
        yaxis = list(title = "Count")
      )
  })
  
  # Map
  output$map <- renderLeaflet({
    qpal <- colorQuantile("YlOrRd", grouped_df()$TotalCount, n = 4)
    
    labels <- paste0(
      create_label("Location"), " ",
      grouped_df()$LocationName, "<br>",
      create_label("Total count"), " ",
      grouped_df()$TotalCount
    )
    
    leaflet(grouped_df()) %>%
      # Add Satellite tiles and assign it to group "Map"
      addTiles(group = "Map") %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 10,
        color = ~qpal(TotalCount),
        fillOpacity = 1,
        # Output labels as HTML
        label = lapply(labels, HTML),
        labelOptions = labelOptions(textsize = "15px")
      )
  })
})

# Run the app
shinyApp(ui, server)
