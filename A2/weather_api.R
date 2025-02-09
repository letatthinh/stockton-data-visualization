# Short description: Get weather forecast from tomorrow website APIs.
# Reference: https://docs.tomorrow.io/reference/weather-forecast

# Load libraries
library(httr) # Stands for Hypertext Transfer Request
library(jsonlite) # Stands for JavaScript Object Notation lite.

url <- "https://api.tomorrow.io/v4/weather/forecast"

queryString <- list(
  location = "39.634117487161284, -74.80332254500941", # Kramer hall - Hammonton
  timesteps = "1d", # daily
  apikey = "wlAobhmd00pnUSTi5FhzQHkoCzreQFpy"
)

# Make a GET request
weather_forecast_response <- GET(
  url, query = queryString, 
  content_type("application/octet-stream"), 
  accept("application/json")
)

# If we made the request successfully
if (weather_forecast_response$status_code == 200) {
  # Extract the content from the response as text
  content_text <- content(weather_forecast_response, "text")
  
  # Parse the JSON
  content_json <- fromJSON(content_text)
  
  # Create a data frame to store daily weather
  daily_weather_df <- as.data.frame(content_json$timelines$daily)
  
  # Write to CSV file
  write.csv(daily_weather_df, "daily-weather.csv")
} else {
  print("Failed to get the daily weather information.")
}