# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)

# Function to get current temperature
get_current_temperature <- function(location, api_key, by = "city") {
  base_url <- "https://api.weatherbit.io/v2.0/current"
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  } else {
    stop("Invalid 'by' parameter.")
  }
  
  response <- GET(base_url, query = params)
  if (http_error(response)) {
    stop("API request failed.")
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  
  tibble(
    city = data$city_name,
    country = data$country_code,
    temperature_C = data$temp,
    feels_like_C = data$app_temp,
    dew_point_C = data$dewpt
  )
}

# Function to get current wind
get_current_wind <- function(location, api_key, by = "city") {
  base_url <- "https://api.weatherbit.io/v2.0/current"
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  } else {
    stop("Invalid 'by' parameter.")
  }
  
  response <- GET(base_url, query = params)
  if (http_error(response)) {
    stop("API request failed.")
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  
  tibble(
    city = data$city_name,
    country = data$country_code,
    wind_speed_mps = data$wind_spd,
    wind_direction = data$wind_cdir_full,
    wind_gusts_mps = data$gust
  )
}

# Function to get current precipitation
get_current_precipitation <- function(location, api_key, by = "city") {
  base_url <- "https://api.weatherbit.io/v2.0/current"
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  } else {
    stop("Invalid 'by' parameter.")
  }
  
  response <- GET(base_url, query = params)
  if (http_error(response)) {
    stop("API request failed.")
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  
  tibble(
    city = data$city_name,
    country = data$country_code,
    precipitation_mm = data$precip,
    humidity_percent = data$rh,
    cloud_coverage_percent = data$clouds
  )
}