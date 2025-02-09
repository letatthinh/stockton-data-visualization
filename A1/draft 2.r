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
# 1. [For questions 1 & 2] Use a bar chart to display the number of occurrences 
# by each unique crime type.

# Short description: This bar chart shows the number of occurrences for each of 
# the 96 unique crime types in Los Angeles, with 'Vehicle - Stolen' being the 
# most frequent with 1,101 occurrences. The color gradient transitions from red 
# to yellow, indicating from high to low in the number of occurrences.

# Set variables
lowest_number_of_crimes <- 0
highest_number_of_crimes <- 1200
number_interval <- 100
annotation_label <- paste(
  "The total number of unique crime types: ", 
  number_of_unique_crime_types
)
annotation_x_position <- number_of_unique_crime_types / 2
annotation_y_position <- highest_number_of_crimes

# Chart
ggplot(
  crime_type_occurrences,
  aes(x = Crime_Code_Description, y = Occurrences)
) +
  geom_col(
    aes(fill = ifelse(Occurrences == max(Occurrences), "#ff8081", "#b8b7b0"))
  ) +
  geom_text(
    aes(label = Occurrences),
    vjust = -1,
    fontface = "bold",
    size = 3.1
  ) +
  scale_fill_identity() +
  scale_y_continuous(breaks = seq(
    lowest_number_of_crimes,
    highest_number_of_crimes,
    number_interval)
  ) +
  labs(
    title = "Number of Occurrences by Unique Crime Type in Los Angeles",
    x = "Crime type description"
  ) +
  annotate(
    "text",
    x = annotation_x_position,
    y = annotation_y_position,
    label = annotation_label,
    hjust = 0.5,
    fontface = "italic"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Modifications
# - Used geom_col() to display the data by columns, with a condition to fill 
# the highest column in red, other columns are in gray.
# - Used geom_text() to add the number of occurrences above each column with 
# some padding. The text is set to bold with a font size of 3.1.
# - Used labs() to add a title to the chart and label the x-axis (the y-axis 
# retains its default label).
# - Used annotate() to add an annotation label indicating that there are 
# 96 unique crimes, and positioned it at the top center of the chart panel.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to rotate the texts on the x-axis by 90 degrees and align them 
# to the top.
# - Used scale_y_continuous() to adjust the scale of the y-axis, from 0-1250 
# (the default range) to 0-1200 and set the interval at 100.


# 2. [For questions 3] Use a horizontal bar chart to show the number of crimes 
# by area.

# Short description: This horizontal bar chart shows the number of crimes and 
# crime rates by area in Los Angeles. Central is a dangerous area having the 
# highest crime rate at about 6.86%. Besides, the colors range from light to 
# dark purple, where darker shades mean higher crime rates, and lighter shades 
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
    aes(label = paste(
      Number_Of_Crimes, " (", round(Rate, digits = 2), "%)", sep = "")
    ),
    hjust = 1.1,
    fontface = "bold",
    size = 4,
    color = "white"
  ) +
  scale_y_continuous(expand = expansion(add = 10)) +
  scale_fill_gradient(low = "#f5e55b", high = "#ea580c") +
  labs(
    title = "Number of Crimes and Crime Rate by Area in Los Angeles",
    x = "Area name",
    y= "Number of crimes"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  coord_flip()

# Modifications
# - Used reorder() to sort the data in descending order by the number of crimes.
# - Used geom_col() to display the data by columns.
# - Used geom_text() to show the number of crimes and their percentage 
# (rounded to two decimals) inside each column. The texts are set to bold with 
# a font size of 4 and white color.
# - Used scale_y_continuous() to add padding between the y-axis and the columns.
# - Used scale_fill_gradient() to fill the columns, with higher occurrences 
# being represented by red, and lower occurrences being represented by yellow.
# - Used labs() to add a title to the chart and label the x and y axes.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to change the font size of the titles on the x and y axes. 
# Additionally, hid the text on the x-axis, as well as the axes and ticks.
# - Used coord_flip() to display the chart horizontally.


# 3. Find the premise where the most crimes occurred, then analyze the 
# relationship between time and the age of the victims.

# Short description: The chart illustrates the distribution of victims by sex, 
# age, and the time at which the crime occurred on the street, identified as the 
# most common premise. It reveals a downward trend for victims of both genders, 
# 'F' and 'M,' as age increases from 10 to approximately 80, indicating that 
# younger individuals are more likely to become victims of street crime, 
# particularly in the evening. In contrast, individuals over the age of 30 with 
# gender type 'X' have a higher likelihood of being victims during evening hours.

# Get the premise with the highest number of crimes 
top_premise <- crime_tbl %>%
  group_by(Premise_Description) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes)) %>%
  slice(1)

# Select columns and filter data
cleaned_data_by_most_frequent_premise <- crime_tbl %>%
  #select(Time_of_Occurrence, Premise_Description, Victim_Sex, Victim_Age) %>%
  filter(
    Victim_Age > 0, 
    Victim_Sex != "",
    Time_of_Occurrence >= 0,
    #Premise_Description == most_frequent_premise,
    Premise_Description == top_premise$Premise_Description
  )

# Set variables
new_born <- 0
old_age <- 99
age_interval <- 9
early_morning_time <- 0
midnight_time <- 2400
time_interval <- 200 # every 2 hours

ggplot(
  cleaned_data_by_most_frequent_premise,
  aes(x = Time_of_Occurrence,
      y = Victim_Age,
      color = Victim_Sex)
) +
  geom_point(size = 3, aes(shape = Victim_Sex)) +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  scale_x_continuous(breaks = seq(
    early_morning_time,
    midnight_time,
    time_interval)
  ) +
  scale_y_continuous(breaks = seq(new_born, old_age, age_interval)) +
  scale_colour_manual(values = c(
    "F" = "#F786AA",
    "M" = "#38bdf8",
    "X" = "#4ade80")
  ) +
  labs(title = "Victims' Sex by Age and from 2020 to 2024 in Los Angeles",
       x = "Victim's age",
       y = "Time of occurrence") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(
      color = "#e2e8f0",
      size = 0.5,
      linetype = 1
    ),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) + geom_density_2d_filled(alpha = 0.1)


# Modifications
# - Used geom_point() to display scatterplots, set the size of the points to 3, 
# and show different shapes based on victims' gender.
# - Used geom_smooth() to show the linear regression line, disabled the 
# confidence interval, and set the color of the line to black.
# - Used scale_x_continuous() and scale_y_continuous() to adjust the scale range 
# and interval on the x-axis and y-axis, respectively.
# - Used scale_colour_manual() to apply a unique color to each victim's sex.
# - Used facet_grid() to view different scatterplots based on victims' sex.
# - Used labs() to add a title to the chart and label the axes.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to change the grid color and add a border to the chart panel.
