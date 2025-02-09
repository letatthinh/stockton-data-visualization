# Assignment 1: Exploring Crime Data in Los Angeles ----

# In this assignment, you'll be exploring a data set containing information
# about crimes in Los Angeles. The goal is to analyze and summarize key aspects
# of the data to better understand crime patterns.

# Start with a few simple ideas:

#1) What are the unique crimes happening in LA?
#2) Which type of crime is occurring the most?
#3) Which neighborhoods are experiencing the highest crime rates?
#4) Create 3 visuals from the data. Keep in mind the best practices we learned from
#Tufte and Berinato. Make sure all your graphs are customized.

# Deliverables:
# An R Script file.
# Screenshots of your graphs with a brief description of what you created. In your document 
# attach your visual with the caption explain what you accomplished with each chart.  

# Note: Each graph must have at least 4 modifications (more is better :) ) from the default settings to 
# receive full credit. You can include elements such as color, labels, axis names, titles, captions, or even remove
# unnecessary elements to enhance clarity. Be creative and ensure your graphs are informative
# and appealing.

# Helpful Reading Resources:

#Data Visualization: A Practical Introduction by Kieran Healy https://socviz.co/
#Chapter 3: Make a plot and
#Chapter 4: Show the right numbers



# Section 1: Read in required packages and libraries needed ----

# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)



# Section 2: Load data, review data, and format ----
crime_tbl <- read.csv("crime_data.csv")

# Check for for NAs in entire dataset
any(is.na(crime_tbl))

# Remove Nas from data
crime_tbl <- na.omit(crime_tbl)

# sanity check
is.na(crime_tbl)

# see column names
names(crime_tbl)

# rename columns for ease
crime_tbl <- crime_tbl %>%
  rename(
    "Report_Number"          = DR_NO,              
    "Date_of_Occurrence"     = DATE.OCC,       
    "Time_of_Occurrence"     = TIME.OCC,     
    "Crime_Code_Description" = Crm.Cd.Desc,
    "Crime_Code"             = Crm.Cd,  
    "Premise_Description"    = Premis.Desc,
    "Premise_Code"           = Premis.Cd,
    "Victim_Race"            = Vict.Descent,
    "Victim_Sex"             = Vict.Sex,
    "Victim_Age"             = Vict.Age,    
    "Neighborhood"           = AREA.NAME,
    "Neighborhood_Code"      = AREA,  
  )

# Convert date column "Date_of_Occurrence" to just date since we already have a time column 
crime_tbl$Date_of_Occurrence <- as.Date(crime_tbl$Date_of_Occurrence, format = "%m/%d/%Y")

# Sanity reformatted column
head(crime_tbl)



# Section 3: Wrangling crime data ----
# Data Manipulation

# Aggregate data by month
crime_by_month <- crime_tbl %>%
  mutate(Month = format(Date_of_Occurrence, "%Y-%m")) %>%
  group_by(Month) %>% # Group crimes by month
  summarise(Total_Crimes = n()) # n() counts the number of rows in each group, how many times each type of crime occurs.

# Convert Month back to Date to prepare date for plotting
crime_by_month$Month <- as.Date(paste0(crime_by_month$Month, "-01"))

# Create Line Chart showing total crimes per month ----
ggplot(crime_by_month, aes(x = Month, y = Total_Crimes)) +
  
  # Canvas
  geom_line(color = "blue", linewidth = 1) +  # Line plot for trend
  #geom_point(color = "red", size = 2) +  # Add points for emphasis
  theme_minimal() +
  labs(title = "Crime Is Declining in Los Angeles",
       caption = "Source: Data.gov",
       x = "",
       y = "Total Crimes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1) What are the unique crimes happening in LA?
unique_crime_types <- unique(crime_tbl$Crime_Code_Description)
number_of_unique_crime_types <- length(unique_crime_types)
number_of_unique_crime_types

# 2) Which type of crime is occurring the most?
# Get the occurrences of each crime type
crime_type_occurrences <- crime_tbl %>%
  group_by(Crime_Code_Description) %>%
  summarise(Occurrences = n())

# Find the max occurrence
most_occurring_crime <- crime_type_occurrences[
  which.max(crime_type_occurrences$Occurrences), 
]
most_occurring_crime

# 3) Which neighborhoods area experiencing the highest crime rates?
total_number_of_crimes <- nrow(crime_tbl)

number_of_crimes_by_area <- crime_tbl %>%
  group_by(Neighborhood) %>%
  summarise(Number_Of_Crimes = n())

number_of_crimes_by_area$Rate <- number_of_crimes_by_area$Number_Of_Crimes / 
  total_number_of_crimes * 100

highest_crime_rate_area <- number_of_crimes_by_area[
  which.max(number_of_crimes_by_area$Rate),
]
highest_crime_rate_area

# 4) Graphs
# 1. [For questions 1 & 2] Use a horizontal bar chart to display the number of 
# occurrences by each unique crime type.

# Set variables
lowest_number_of_crimes <- 0
highest_number_of_crimes <- 1150
number_interval <- 50
annotation_label <- paste(
  "The total number of unique crime types: ", 
  number_of_unique_crime_types
)
annotation_x_position <- highest_number_of_crimes
annotation_y_position <- number_of_unique_crime_types / 2

# Chart
ggplot(
  crime_type_occurrences,
  aes(x = Occurrences, y = Crime_Code_Description)
) +
  geom_col(
    aes(fill = ifelse(Occurrences == max(Occurrences), "#ff8081", "#71717a"))
  ) +
  scale_fill_identity() +
  geom_text(
    aes(label = Occurrences),
    hjust = -0.3,
    vjust = 0.35,
    fontface = "bold",
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = seq(
      lowest_number_of_crimes,
      highest_number_of_crimes,
      number_interval),
    expand = expansion(add = 10)
  ) +
  labs(
    title = "Number of Crimes by Type in Los Angeles",
    y = "Crime type description"
  ) +
  annotate(
    "text",
    x = annotation_x_position,
    y = annotation_y_position,
    label = annotation_label,
    vjust = 0.5,
    angle = -90,
    fontface = "italic"
  ) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_text(size = 10),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )


# 2. [For questions 3] Use a bar chart to show the number of crimes by area.

# Short description: This horizontal bar chart shows the number of crimes and 
# crime rates by area in Los Angeles. Central is a dangerous area having the 
# highest crime rate at about 6.86%. Besides, the colors range from light to 
# dark orange, where darker shades mean higher crime rates, and lighter shades 
# mean lower crime rates.

# Chart
ggplot(
  number_of_crimes_by_area,
  aes(
    x = reorder(Neighborhood, Number_Of_Crimes),
    y = Number_Of_Crimes,
    fill = Number_Of_Crimes
  )
) +
  geom_col() +
  geom_text(
    aes(label = Number_Of_Crimes),
    hjust = 0.5,
    vjust = 1.35,
    fontface = "bold",
    size = 3,
    color = "white"
  ) +
  geom_text(
    aes(label = paste0("(", round(Rate, digits = 2), "%)")),
    hjust = 0.5,
    vjust = 3.3,
    fontface = "bold",
    size = 3,
    color = "white"
  ) +
  scale_y_continuous(expand = expansion(add = 10)) +
  scale_fill_gradient(low = "#f5e55b", high = "#ea580c") +
  labs(
    title = "Number of Crimes and Crime Rate by Area in Los Angeles",
    x = "Area name",
    y= "Number of crimes",
    fill = "Number of crimes"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11)
  )

# Modifications
# - Used reorder() to sort the data in descending order by the number of crimes.
# - Used geom_col() to display the data as bars.
# - Used geom_text() to show the number of crimes and their percentage 
#   (rounded to two decimals) inside each bar. The texts are set to bold with 
#   a font size of 4 and white color.
# - Used scale_y_continuous() to adjust the padding between the x-axis and 
#   the bars.
# - Used scale_fill_gradient() to fill the columns, with higher occurrences 
#   being represented by orange, and lower occurrences being represented by 
#   yellow.
# - Used labs() to add a title to the chart, label the x and y axes and rename 
#   the lengend.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to change the font size of the titles and texts on the x and y 
#   axes.


# 3. Find the premise where the most crimes occurred, then analyze the 
# relationship between the age of the victims and the time of occurrence.

# Short description: The chart shows the relationship between the victims' ages 
# and the times when crimes happened on the street, the most common location in 
# Los Angeles. For both genders, 'F' and 'M,' the victims tend to be younger as 
# it gets later in the day. In contrast, for gender 'X,' the victims are older
# as the time progresses.

# Get the premise with the highest number of crimes 
top_premise <- crime_tbl %>%
  group_by(Premise_Description) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes)) %>%
  slice(1)
# result: STREET

# Select specific columns and filter data
cleaned_data_by_most_frequent_premise <- crime_tbl %>%
  select(Time_of_Occurrence, Premise_Description, Victim_Sex, Victim_Age) %>%
  filter(
    Victim_Age > 0, 
    Victim_Sex != "",
    Time_of_Occurrence >= 0,
    Premise_Description %in% top_premise$Premise_Description
  )

# Set variables
new_born <- 0
old_age <- 99
age_interval <- 9
early_morning_time <- 0
midnight_time <- 2400
time_interval <- 400
annotate_fill_bg <- "#FB67FF"

ggplot(
  cleaned_data_by_most_frequent_premise,
  aes(x = Time_of_Occurrence,
      y = Victim_Age,
      color = Victim_Sex)
) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 2) +
  scale_x_continuous(breaks = seq(
    early_morning_time,
    midnight_time,
    time_interval)
  ) +
  scale_y_continuous(breaks = seq(new_born, old_age, age_interval)) +
  scale_colour_manual(values = c(
    "F" = "#f472b6",
    "M" = "#7291FF",
    "X" = "#61E369")
  ) +
  facet_grid(cols = vars(Victim_Sex)) +
  labs(
    title = paste(
      "Victims' Sex by Age and Time of Crime Occurrence on the Street",
      "in Los Angeles"),
    x = "Time of occurrence",
    y = "Victim's age",
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(
      color = "#e2e8f0",
      size = 0.5,
      linetype = 1
    ),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Modifications
# - Used geom_point() to display points and set the size of the points to 3.
# - Used geom_smooth() to show the linear regression line, set the color of 
# the line to black, and set the line width to 2.5.
# - Used scale_x_continuous() and scale_y_continuous() to adjust the scale range 
# and interval on the x-axis and y-axis, respectively.
# - Used scale_colour_manual() to apply a unique color to each victim's sex.
# - Used facet_grid() to view different scatterplots based on victims' sex.
# - Used labs() to add a title to the chart and label the axes.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to change the grid color and add a border to the chart panel.
