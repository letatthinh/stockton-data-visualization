# Short description: Scrape items by a search keyword on Amazon.

# Import libraries
library(rvest)
library(stringr)

# Search keyword, the item you want to search
search_keyword <- 'smartphones'

# In case the search keyword contains space bar character, it must be replaced 
# by '+' for the search query to understand
search_keyword <- str_replace(search_keyword, " ", "+")

# Number of pages, more pages, wait more!
number_of_pages <- 20

# The base URL without page number
base_url <- paste0( 
  "https://www.amazon.com/s?k=", search_keyword, "&page="
)

# Create a data frame to store the items
item_df <- data.frame(
  title = character(),
  price = character(), # because it contains currency symbol
  price_without_discount = character(), # because it contains currency symbol
  options = character(),
  free_delivery_description = character(),
  user_rating = character(),
  number_of_user_ratings = character(),
  units_sold_description = character()
)

# Get element text by selector
get_text <- function(item_element, css_selector) {
  item_element %>%
    html_element(css = css_selector) %>%
    html_text()
}

# Get element attribute value by selector
get_attribute_value <- function(item_element, css_selector, attribute_name) {
  item_element %>%
    html_element(css = css_selector) %>%
    html_attr(attribute_name)
}

# Create a new record in the item data frame
create_item <- function(item_element) {
  # Title
  title <- get_text(item_element, "div[data-cy='title-recipe'] > h2 span")
  
  # Price
  price <- get_text(item_element, paste(
    "span.a-price[data-a-color='base'] > span.a-offscreen"
  ))
  
  # Price without discount
  price_without_discount <- get_text(item_element, paste(
    "span.a-price[data-a-color='secondary'] > span.a-offscreen"
  ))
  
  # Options
  options <- get_text(item_element, "div.s-variation-options-options-text + a")
  
  # Free delivery description
  free_delivery_description_1 <- get_attribute_value(item_element, paste(
      "div[data-cy='delivery-recipe'] span[aria-label^='FREE']"
    ),
    "aria-label"
  )
  
  free_delivery_description_2 <- get_attribute_value(item_element, paste(
    "div[data-cy='delivery-recipe'] span[aria-label^='Or']"
    ),
    "aria-label"
  )
  
  # Join the descriptions
  free_delivery_description <- str_c(
    str_trim(free_delivery_description_1), 
    str_trim(free_delivery_description_2),
    sep = " "
  )
  
  # Ratings
  user_rating <- get_text(item_element, paste(
    "div[data-cy='reviews-block'] >",
    "div > span:not([data-component-type]) a"
  ))
  
  # Number of ratings
  number_of_user_ratings <- get_text(item_element, paste(
    "div[data-cy='reviews-block'] >",
    "div > span[data-component-type] a"
  ))
  
  # Units sold description
  units_sold_description <- get_text(item_element, paste(
    "div[data-cy='reviews-block'] > div.a-size-base > span.a-color-secondary"
  ))
  
  return(data.frame(
    title,
    price,
    price_without_discount,
    options,
    free_delivery_description,
    user_rating,
    number_of_user_ratings,
    units_sold_description
  ))
}

# Loop from page 1 to the defined page number
for (page_number in 1:number_of_pages) {
  # Notify that the item is being scraped
  print(paste("Scraping items on page", page_number, "..."))
  
  # Construct the full URL for each page
  url <- paste0(base_url, page_number)
  
  # Read the HTML
  html <- read_html(url)
  
  # Get all the item results on the page
  item_elements <- html %>%
    html_elements(css = "div[data-component-type='s-search-result']")
  
  # Collect items
  for (item_element in item_elements) {
    # Create a new item
    new_item <- create_item(item_element)
    
    # Add it to the data frame
    item_df <- rbind(item_df, new_item)
  }
}

# Notify that the process has been completed
print("Done!")

# Write the data frame to CSV file in the current working directory
write.csv(item_df, "items-on-amazon.csv")
