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
most_occurring_crime <- crime_type_occurrences[which.max(crime_type_occurrences$Occurrences), ]
most_occurring_crime

# 3) Which neighborhoods area experiencing the highest crime rates?
total_number_of_crimes <- nrow(crime_tbl)

number_of_crimes_by_area <- crime_tbl %>%
  group_by(Neighborhood) %>%
  summarise(Number_Of_Crimes = n())

number_of_crimes_by_area$Rate <- number_of_crimes_by_area$Number_Of_Crimes / total_number_of_crimes * 100

highest_crime_rate_area <- number_of_crimes_by_area[which.max(number_of_crimes_by_area$Rate), ]
highest_crime_rate_area

# 4) Graphs
# 1. [For questions 1 & 2] Use a bar chart to display the number of occurrences 
# by each unique crime type.

# Short description: This bar chart shows the number of occurrences for each of 
# the 96 unique crime types in Los Angeles, with 'Vehicle - Stolen' being the 
# most frequent with 1,101 occurrences. The color gradient transitions from red 
# to yellow, indicating from high to low in the number of occurrences.

# Set variables
y_lower_limit <- 0
y_upper_limit <- 1200
y_interval <- 100
annotation <- paste(
  "The total number of unique crime types: ", 
  number_of_unique_crime_types
) 

# Chart
ggplot(
  crime_type_occurrences,
  aes(
    x=reorder(Crime_Code_Description, -Occurrences),
    y=Occurrences,
    fill = Occurrences
  )
) +
geom_col() +
geom_text(aes(label = Occurrences), vjust = -1, fontface = "bold",size = 2.9) +
labs(
  title = "Number of Occurrences by Unique Crime Type in Los Angeles",
  x = "Crime type description"
) +
annotate(
  "text",
  x = number_of_unique_crime_types,
  y = y_upper_limit,
  label = annotation,
  hjust = 1,
  fontface = "italic"
) +
theme_classic() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_y_continuous(breaks = seq(y_lower_limit, y_upper_limit, y_interval)) +
scale_fill_gradient(low = "#facc15", high = "#e03158")

# Modifications
# - Used reorder() to sort the data in descending order by the "Occurrences" column and set the fill of the columns by Occurrences.
# - Used geom_col() since the variables are discrete and continuous.
# - Used geom_text() to display the number of occurrences above each column.
# - Used labs() to add a title to the chart and label the x-axis (the y-axis retains its default label).
# - Used annotate() to add an annotation indicating that there are 96 unique crimes, positioned at the top right.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to rotate the text on the x-axis by 90 degrees and align it to the top.
# - Used scale_y_continuous() to adjust the scale of the y-axis, from 0-1250 to 0-1200 and
# - Used scale_fill_gradient() to fill the columns, with higher occurrences being represented by red.


# 2. [For questions 3] Use a horizontal bar chart to show the number of crimes by area

# Short description: This horizontal bar chart shows the number of crimes and crime rates 
# by area in Los Angeles. Central is a "hot" area having the highest crime rate at about 6.86%. 
# The colors range from light to dark purple, where darker shades mean higher crime rates, 
# and lighter shades mean lower crime rates.
ggplot(number_of_crimes_by_area,
       aes(x=reorder(Neighborhood, Number_Of_Crimes),
           y=Number_Of_Crimes,
           fill = Number_Of_Crimes)) +
  coord_flip() +
  geom_col(fill = "#b06cf5") +
  geom_text(aes(label = paste(Number_Of_Crimes, " (", round(Rate, digits = 2), "%)", sep = "")),
            hjust = 1.1,
            fontface = "bold",
            size = 4,
            color = "white") +
  labs(title = "Number of Crimes and Crime Rate by Area in Los Angeles",
       x = "Area name",
       y= "Number of crimes") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.line = element_blank())

# Modifications
# - Used reorder() to sort the data in descending order by the Number_Of_Crimes column and set the fill of the columns by Number_Of_Crimes.
# - Used coord_flip() to display the bar chart horizontally.
# - Used geom_col() since the variables are discrete and continuous.
# - Used geom_text() to show the number of crimes and their percentage (rounded to two decimals) next to the horizontal bars.
# - Used labs() to add a title to the chart and label the x and y axes.
# - Used theme_classic() to apply the classic theme.
# - Used theme() to change the font size of the text and titles on the x and y axes. Additionally, hid the text on the x-axis, as well as the axes and ticks.
# - Used scale_fill_gradient2() to fill the color with a purple palette, midpoint at 400.


# 3. Use a scatterplot to analyze victims' sex by their age and the date the crime occurred

# Short description: The chart shows the distribution of victims' sex by age and the date the crime occurred. 
# It can be seen that crimes most frequently affect victims around the age of 40, with gender types 'F' and 'M'.
# In contrast, victims with gender type 'X' face fewer crimes, and most of them are between the age of 25 and 30.

# Clean rows having no information about victim's sex and rows having age is 0
# vsa: victime's sex and age
cleaned_crime_tbl_by_vsa <- crime_tbl %>%
  #select(Date_of_Occurrence, Victim_Age, Victim_Sex, Victim_Race, Crime_Code_Description) %>%
  filter(Victim_Age != 0, Victim_Sex != "")
y_lower_limit <- 0
y_upper_limit <- 100
y_interval <- 5

ggplot(cleaned_crime_tbl_by_vsa, 
       aes(x = Date_of_Occurrence, 
           y = Victim_Age, 
           color = Victim_Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  facet_grid(cols = vars(Victim_Sex)) +
  labs(title = "Victims' Sex by Age and from 2020 to 2024 in Los Angeles",
       x = "Year",
       y= "Victim's age") +
  theme_bw() +
  scale_colour_manual(values = c("F" = "#eab308","M" = "#38bdf8", "X"= "#4ade80")) +
  scale_y_continuous(breaks = seq(y_lower_limit, y_upper_limit, y_interval))

# Modifications
# - Used geom_point() to display scatterplots.
# - Used geom_smooth() to show the linear regression line, disable the confidence interval, and set the color of the line to black.
# - Used facet_grid() to view different scatterplots based on victims' sex.
# - Used labs() to add a title to the chart and label the axes.
# - Used theme_bw() to apply the classic dark-on-light theme.
# - Used scale_colour_manual() to apply specific colors to each sex type.
# - Used scale_y_continuous() to adjust the default range and set each segment size to 5.
