library(dplyr)   # Work with data
library(ggplot2) # Create charts
library(shiny)   # Create interactive dashboards
library(bslib)   # modern UI toolkit based on Bootstrap

df <- read.csv("impact-of-work-on-mental-health.csv")

df$Work_Location <- factor(
  df$Work_Location,
  levels = c("Onsite", "Hybrid", "Remote")
)

df$Stress_Level <- factor(
  df$Stress_Level,
  levels = c("Low", "Medium", "High")
)

df$Mental_Health_Condition <- factor(
  df$Mental_Health_Condition,
  levels = c("None", "Anxiety", "Burnout", "Depression")
)

df$Productivity_Change <- factor(
  df$Productivity_Change,
  levels = c("No Change", "Decrease", "Increase")
)

df$Physical_Activity <- factor(
  df$Physical_Activity,
  levels = c("None", "Weekly", "Daily")
)

df$Sleep_Quality <- factor(
  df$Sleep_Quality,
  levels = c("Poor", "Average", "Good")
)

unique_industries <- unique(df$Industry)
unique_genders <- unique(df$Gender)
unique_regions <- unique(df$Region)

# Colors
b_50 <- "#eff6ff"
b_100 <- "#dbeafe"
b_200 <- "#bfdbfe"
b_300 <- "#93c5fd"
b_400 <- "#60a5fa"
b_500 <- "#3b82f6"
b_600 <- "#2563eb"
b_700 <- "#1d4ed8"
b_800 <- "#1e40af"
b_900 <- "#1e3a8a"
r_400 <- "#f87171"
gr_400 <- "#4ade80"
y_500 <- "#eab308"

# For a single-page dashboards with an optional sidebar.
# https://rstudio.github.io/bslib/reference/index.html#dashboard-layouts
ui <- shinyUI(page_sidebar(
  title = "Impact of work and physical activities on mental health",
  # https://rstudio.github.io/bslib/reference/layout_columns.html
  sidebar = sidebar(
    selectInput(
      inputId = "industryOption",
      label = "Select industry:",
      choices = unique_industries,
      selected = "Finance",
    ),
    selectInput(
      inputId = "genderOptions",
      label = "Select genders:",
      choices = unique_genders,
      selected = c("Female"),
    ),
    selectInput(
      inputId = "regionOption",
      label = "Select region:",
      choices = unique_regions,
      selected = "North America",
    ),
    class = "bg-black"
  ),
  # https://rstudio.github.io/bslib/reference/layout_columns.html
  layout_columns(
    col_widths = c(4, 4, 4, 6, 6),
    row_heights = c(1, 1),
    plotOutput("graph_1"),
    plotOutput("graph_2"),
    plotOutput("graph_3"),
    plotOutput("graph_4"),
    plotOutput("graph_5"),
  )
))

server <- shinyServer(function(input, output) {
  filtered_df <- reactive({
    df %>%
      filter(
        Industry == input$industryOption,
        Gender %in% input$genderOptions,
        Region == input$regionOption
      )
  })
  
  # Graph 1 -------------------------------------------------------------
  
  average_social_isolation_rating_by_age <- reactive({
    filtered_df() %>%
      group_by(Age) %>%
      summarise(Average_social_isolation_rating = round(
        mean(Social_Isolation_Rating),
        digits = 2
      ))
  })
  
  output$graph_1 <- renderPlot({
    ggplot(
      average_social_isolation_rating_by_age(),
      aes(x = Age, y = Average_social_isolation_rating)
    ) +
      geom_line(color = b_400, size = 0.75, alpha = 0.7) +
      geom_point(color = b_400, size = 5, alpha = 0.7) +
      geom_label(
        aes(label = Average_social_isolation_rating),
        size = 4
      ) +
      labs(
        title = "Average Social Isolation by Age", 
        x = "Age",
        y = "Average social isolation rating"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          margin = margin(b = 10)
        ),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Graph 2 -------------------------------------------------------------
  
  productivity_change_by_work_location <- reactive({
    filtered_df() %>%
      count(Productivity_Change, Work_Location)
  })
  
  output$graph_2 <- renderPlot({
    ggplot(
      productivity_change_by_work_location(),
      aes(
        x = Work_Location,
        y = Productivity_Change,
        fill = n
      )
    ) +
      geom_tile() +
      coord_fixed() +
      scale_fill_gradient2(low = b_100, mid = b_400, high = b_900) +
      labs(
        title = "Productivity Change by Work Location", 
        x = "Work location",
        y = "Productivity change"
      ) +
      theme_minimal() +
      theme(
        # Hide the legend
        legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          margin = margin(b = 10)
        ),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Graph 3 -------------------------------------------------------------
  
  # https://stackoverflow.com/questions/57381627/add-text-labels-at-the-top-of-density-plots-to-label-the-different-groups
  density_peaks <- reactive({
    filtered_df() %>%
      group_by(Stress_Level) %>%
      summarize(
        x_peak = density(Years_of_Experience)$x[
          which.max(density(Years_of_Experience)$y)
        ],
        y_peak = max(density(Years_of_Experience)$y)
      )
  })
  
  output$graph_3 <- renderPlot({
    ggplot(filtered_df(), aes(x = Years_of_Experience, fill = Stress_Level)) +
      geom_density(alpha = 0.7) + # Alpha for transparency
      # Color palette for stress levels
      scale_fill_manual(values=c(gr_400, b_400, r_400)) +
      # Adding stress level labels at the peak of each density curve
      geom_text(
        data = density_peaks(), 
        aes(x = x_peak, y = y_peak, label = Stress_Level),
        vjust = -0.7, hjust = 0.5, fontface = "bold"
      ) +
      labs(
        title = "Density of Stress by Years of Experience", 
        x = "Years of experience", 
        y = "Density"
      ) +
      theme_classic() +
      theme(
        # Hide the legend
        legend.position = "none",
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          margin = margin(b = 10)
        ),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Graph 4 -------------------------------------------------------------
  
  job_with_highest_working_hours <- reactive({
    df_by_Job_Role <- filtered_df() %>%
      group_by(Job_Role) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    df_by_Job_Role$Job_Role[1]
  })
  
  output$graph_4 <- renderPlot({
    ggplot(
      filtered_df(), 
      aes(
        x = Job_Role, 
        y = Hours_Worked_Per_Week,
        fill = ifelse(
          Job_Role == job_with_highest_working_hours(), 
          "Highlighted", 
          "Normal"
        )
      )
    ) +
      geom_boxplot(size = 0.75, alpha = 0.7) +
      scale_fill_manual(
        values = c("Highlighted" = b_400, "Normal" = "white")
      ) +
      labs(
        title = "Working Hours Per Week by Job Role", 
        x = "Job role", 
        y = "Working hours per week"
      ) +
      theme_classic() +
      theme(
        # Hide the legend
        legend.position = "none",
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          margin = margin(b = 10)
        ),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Graph 5 -------------------------------------------------------------
  
  output$graph_5 <- renderPlot({
    ggplot(
      filtered_df(), 
      aes(x = Sleep_Quality, fill = Mental_Health_Condition)
    ) +
      # "fill" scales the bars to 100%, creating proportions
      geom_bar(position = "fill", alpha = 0.7) +
      # Separate plots by Physical_Activity levels
      facet_wrap(
        ~Physical_Activity,
        labeller = as_labeller(
            c(None = "None", 
              Weekly = "Weekly physical activity", 
              Daily = "Daily physical activity")
          )
      ) +
      # Color palette for Sleep_Quality
      scale_fill_manual(values=c(b_100, b_300, b_500, b_900)) +
      labs(
        title = "Sleep Quality by Mental Health Condition and Physical Activity",
        x = "Sleep quality", 
        y = "Proportion",
        fill = "Mental health condition"
      ) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(
          size = 16, 
          face = "bold", 
          margin = margin(b = 10)
        ),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
})

# Run the application
shinyApp(ui = ui, server = server)
