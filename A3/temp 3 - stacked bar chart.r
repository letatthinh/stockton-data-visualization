renderPlot({
  average_electric_range_by_manufacturer <- ev_df %>%
    filter(
      Electric_Range > 0, 
      input$Year_Range[1] <= Model_Year,
      Model_Year <= input$Year_Range[2]
    ) %>%
    group_by(Manufacturer, EV_Type) %>%
    summarise(Average_Electric_Range = round(mean(Electric_Range), digits = 2))
  
  ggplot(
    average_electric_range_by_manufacturer,
    aes(x = Manufacturer, y = Average_Electric_Range, fill = EV_Type)
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(label = Average_Electric_Range),
    hjust = 0.5,
    position = position_stack(vjust = 0.5),
    fontface = "bold",
    size = 4,
    color = cyan_100
  ) +
  scale_fill_manual(
    values = c("BEV" = cyan_950, "PHEV" = cyan_500),
    labels = c(
      "BEV" = "Battery",
      "PHEV" = "Plug-in Hybrid"
    )
  ) +
  labs(
    fill = "Classification"
  ) +
  theme_classic() +
  theme(
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 11),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )
})