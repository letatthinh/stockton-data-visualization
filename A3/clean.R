library(dplyr)
library(stringr)
library(tidyr)

# Section 2: Load data, review data, and format ----
df <- read.csv("data.csv")

# Filter data in Washington only (biggest percentage) and model year <= 2024
df <- df %>% filter(State == "WA", Model.Year <= 2024)

# Remove columns
df[, 1] <- NULL
df$DOL.Vehicle.ID <- NULL
df$Base.MSRP <- NULL
df$Legislative.District <- NULL
df$State <- NULL

# Extract vehicle latitude and longitude into separate columns
df$Vehicle.Location = str_replace(df$Vehicle.Location, "POINT ", "")
df$Vehicle.Location = str_replace(df$Vehicle.Location, "\\(", "")
df$Vehicle.Location = str_replace(df$Vehicle.Location, "\\)", "")

df <- df %>%
  separate(
    Vehicle.Location, 
    into = c("Vehicle_Longitude", "Vehicle_Latitude"), 
    sep = " "
  )

df$Vehicle_Longitude <- as.numeric(df$Vehicle_Longitude)
df$Vehicle_Latitude <- as.numeric(df$Vehicle_Latitude)

# Extract FIPS code and remove 2020.Census.Tract column
df$FIPS_Code <- substr(df$X2020.Census.Tract, 1, 5)
df$X2020.Census.Tract <- NULL

# Rename columns
colnames(df) <- gsub("[. ]", "_", colnames(df))

df <- df %>%
  rename(
    "EV_Type"      = E_V_Type,
    "Manufacturer" = Make
  )

# Remove NAs
df <- na.omit(df)

# Write to a new csv file
write.csv(df, paste0(getwd(), "/electric-vehicles-in-washington.csv"))