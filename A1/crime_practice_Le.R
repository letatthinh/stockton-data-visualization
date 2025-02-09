# Assignment 1: Exploring Crime Data in Los Angeles ----

# In this assignment, you'll be exploring a data set containing information
# about crimes in Los Angeles. The goal is to analyze and summarize key aspects
# of the data to better understand crime patterns.

# Start with a few simple ideas:

# 1) What are the unique crimes happening in LA?
# 2) Which type of crime is occurring the most?
# 3) Which neighborhoods are experiencing the highest crime rates?
# 4) Create 3 visuals from the data. Keep in mind the best practices we learned from
# Tufte and Berinato. Make sure all your graphs are customized.

# Deliverables:
# An R Script file.
# Screenshots of your graphs with a brief description of what you created. In your document
# attach your visual with the caption explain what you accomplished with each chart.

# Note: Each graph must have at least 4 modifications (more is better :) ) from the default settings to
# receive full credit. You can include elements such as color, labels, axis names, titles, captions, or even remove
# unnecessary elements to enhance clarity. Be creative and ensure your graphs are informative
# and appealing.

# Helpful Reading Resources:

# Data Visualization: A Practical Introduction by Kieran Healy https://socviz.co/
# Chapter 3: Make a plot and
# Chapter 4: Show the right numbers



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
  geom_line(color = "blue", linewidth = 1) + # Line plot for trend
  # geom_point(color = "red", size = 2) +  # Add points for emphasis
  theme_minimal() +
  labs(
    title = "Crime Is Declining in Los Angeles",
    caption = "Source: Data.gov",
    x = "",
    y = "Total Crimes"
  ) +
  # [Tip]: rotate text on x-axis
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
  summarise(Count = n())

number_of_crimes_by_area$Rate <- number_of_crimes_by_area$Count /
  total_number_of_crimes * 100

highest_crime_rate_area <- number_of_crimes_by_area[
  which.max(number_of_crimes_by_area$Rate),
]
highest_crime_rate_area

# 4) Graphs
# 1. Visualize top 5 crimes at night from 2020 to 2024 in Los Angeles
crimes_at_night <- crime_tbl %>%
  filter(Time_of_Occurrence >= 1800 & Time_of_Occurrence <= 2400)

top_5_crimes_at_night <- crimes_at_night %>%
  group_by(Crime_Code_Description) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice(1:5)

ggplot(
  top_5_crimes_at_night,
  aes(x = Count, y = reorder(Crime_Code_Description, Count))
) +
  geom_bar(
    stat = "identity", width = 0.8,
    aes(fill = ifelse(Count == max(Count), "black", "#ebebeb"))
  ) +
  geom_text( # size in pt
    aes(label = Count, color = ifelse(Count == max(Count), "#ebebeb", "black")),
    hjust = 1.4,
    vjust = 0.35,
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  # [Tip]: Place the x title on the top
  scale_x_continuous(expand = expansion(add = 3), position = "top") +
  labs(
    title = "Top 5 Crimes occur the most at Night (6 PM to 12 AM) in Los Angeles",
    x = "Occurrences"
  ) +
  annotate(
    "text",
    x = 186, y = 1, label = "Dangerous crime!",
    hjust = 0, vjust = 0.35,
    fontface = "italic", size = 3.5
  ) +
  theme_classic() +
  theme( # size in mm
    axis.title.x = element_text(size = 11, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, vjust = 0.4),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 13, face = "bold", margin = margin(b = 10))
  )


# 2. Crime Rate by Area in Los Angeles
# Chart
ggplot(
  number_of_crimes_by_area,
  aes(
    x = reorder(Neighborhood, Count),
    y = Count,
    fill = Count
  )
) +
  geom_bar(stat = "identity", width = 0.85) +
  geom_text(
    aes(label = round(Rate, digits = 2)),
    hjust = 0.5,
    vjust = 2,
    fontface = "bold",
    size = 3,
    color = "black"
  ) +
  scale_y_continuous(expand = expansion(add = 10)) +
  scale_fill_gradient(low = "#FFCD19", high = "#FA681A") +
  labs(
    title = "Crime Rate by Area in Los Angeles",
    fill = "Number of crimes"
  ) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )


# 3. Victims' Sex by Age and Time of Crime Occurrence on the Street in Los
# Angeles

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

ggplot(
  cleaned_data_by_most_frequent_premise,
  aes(
    x = Time_of_Occurrence,
    y = Victim_Age,
    color = Victim_Sex
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1.25) +
  scale_x_continuous(breaks = seq(
    early_morning_time,
    midnight_time,
    time_interval
  )) +
  scale_y_continuous(breaks = seq(new_born, old_age, age_interval)) +
  scale_colour_manual(values = c(
    "F" = "#FF92DB",
    "M" = "#91B8FF",
    "X" = "#52CC85"
  )) +
  facet_grid(cols = vars(Victim_Sex)) +
  labs(
    title = paste(
      "Victims' Sex by Age and Time of Crime Occurrence on the Street",
      "in Los Angeles"
    ),
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
