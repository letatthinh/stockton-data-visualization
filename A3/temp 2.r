# # Plotting
# electric_vehicles_by_state_df %>%
#   # Plot a map focus on USA states
#   # https://plotly.com/r/reference/choropleth/#choropleth-locationmode
#   plot_geo(locationmode = 'USA-states') %>%
#   add_trace(
#     z = ~Count,
#     locations = ~State_Code,
#     color     = ~Count,
#     text      = ~Hover_Text,
#     colorscale = list(
#       c(0, '#c7e9c0'),  # Light yellow at the low end
#       c(1, '#005a32')       # Dark green at the high end
#     )
#   ) %>%
#   layout(
#     geo = list(
#       # Show USA instead of the world map
#       scope      = "usa",
#       # This projection is ideal when you want to map data across the states,
#       # and you want to keep the relative area of the states accurate.
#       projection = list(type = "albers usa"),
#       # Hide the lakes
#       showlakes  = FALSE
#     )
#   ) %>%
#   # Colorbar title
#   colorbar(title = "Units") %>%
#   layout(title = list(
#       # Make the title bold using HTML
#       text = "<b>Distribution of Electric Vehicles Across U.S. States<b>",
#       font = list(
#         size = 16
#       ),
#       x = 0.55,  # Center the title horizontally
#       xanchor = "center",  # Set the anchor point to the center
#       y = 0.95  # Adjust the vertical position as needed
#     )
#   )