# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(glue)

source("R/constant.R")
source("R/utils.R")

# Define the API endpoint
base_url <- "https://api.weatherbit.io/v2.0/"
endpoint <- "current"
current_weather_url <- glue(base_url, endpoint)

# Function to validate location input
validate_location <- function(location, by) {
  if (missing(location) || location == "") stop("Error: Location must be provided.")
  
  if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    if (length(coords) != 2) stop("Error: Invalid lat/lon format. Use 'lat,lon'.")
    if (!is.numeric(as.numeric(coords[1])) || !is.numeric(as.numeric(coords[2]))) {
      stop("Error: Latitude and longitude must be numeric values.")
    }
  } else if (!(by %in% c("city", "postal"))) {
    stop("Error: Invalid 'by' parameter. Use 'city', 'latlon', or 'postal'.")
  }
}

# Function to process API response
process_api_response <- function(response) {
  if (response$status_code != 200) {
    content_data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content_data)
    error_message <- json_data$error
    stop(glue("Error: API request failed. Status code: {response$status_code}, Message: {error_message}"))
  }
  
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
  if (is.null(data)) stop("Error: No data returned. Check location or API key.")
  
  return(data)
}

# Function to get current temperature
get_current_temperature <- function(location, api_key, by = "city", save_dir = "") {
  if (!connect_api_key()) stop("Error: API key is missing. Set up your WeatherBit API key.")
  validate_location(location, by)
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  }
  
  response <- GET(current_weather_url, query = params)
  data <- process_api_response(response)
  
  result <- tibble(
    city = data$city_name,
    country = data$country_code,
    temperature_C = data$temp,
    feels_like_C = data$app_temp,
    dew_point_C = data$dewpt
  )
  
  if (save_dir != "") {
    save_csv(result, save_dir, endpoint = "current_weather", params = c(location))
  }
  
  return(result)
}

# Function to get current wind
get_current_wind <- function(location, api_key, by = "city", save_dir = "") {
  if (!connect_api_key()) stop("Error: API key is missing. Set up your WeatherBit API key.")
  validate_location(location, by)
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  }
  
  response <- GET(current_weather_url, query = params)
  data <- process_api_response(response)
  
  result <- tibble(
    city = data$city_name,
    country = data$country_code,
    wind_speed_mps = data$wind_spd,
    wind_direction = data$wind_cdir_full,
    wind_gusts_mps = ifelse(is.null(data$gust), NA, data$gust)
  )
  
  if (save_dir != "") {
    save_csv(result, save_dir, endpoint = "current_weather", params = c(location))
  }
  
  return(result)
}

# Function to get current precipitation
get_current_precipitation <- function(location, api_key, by = "city", save_dir = "") {
  if (!connect_api_key()) stop("Error: API key is missing. Set up your WeatherBit API key.")
  validate_location(location, by)
  
  params <- list(key = api_key)
  if (by == "city") {
    params$city <- location
  } else if (by == "latlon") {
    coords <- unlist(strsplit(location, ","))
    params$lat <- coords[1]
    params$lon <- coords[2]
  } else if (by == "postal") {
    params$postal_code <- location
  }
  
  response <- GET(current_weather_url, query = params)
  data <- process_api_response(response)
  
  result <- tibble(
    city = data$city_name,
    country = data$country_code,
    precipitation_mm = ifelse(is.null(data$precip), 0, data$precip),
    humidity_percent = data$rh,
    cloud_coverage_percent = data$clouds
  )
  
  if (save_dir != "") {
    save_csv(result, save_dir, endpoint = "current_weather", params = c(location))
  }
  
  return(result)
}