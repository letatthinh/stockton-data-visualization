create_map_by_group <- function(data_frame) {
  data_frame %>%
    plot_geo(locationmode = 'USA-states') %>%
    add_markers(
      x = ~Vehicle_Longitude,
      y = ~Vehicle_Latitude,
      color = I("blue"),
      alpha = 0.5
    ) %>%
    add_text(
      x = -78, 
      y = 47, 
      text = ~unique(Model_Year), 
      color = I("black")
    ) %>%
    layout(geo = list(
        scope = 'usa', # Only show USA map
        showland = T,
        landcolor = toRGB("gray90"),
        showcountries = T,
        subunitcolor = toRGB("white")
      )
    )
}

# Group data by model year
electric_vehicles_by_model_year_df <- electric_vehicle_df %>% 
  group_by(Model_Year)

electric_vehicles_by_model_year_df <- electric_vehicles_by_model_year_df %>%
  # Create map for each group of model years
  do(mafig = create_map_by_group(.))

electric_vehicles_by_model_year_df %>% subplot(nrows = 3)