# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(glue)

source("R/constant.R")
source("R/utils.R")

# Set up endpoint for forecast API.
endpoint <- "forecast/daily"
endpoint_name <- "forecast"

# Get forecast url.
forecast_url <- glue(base_url, endpoint)

# Get forecast by city function.
get_forecast_by_city <- function(
  city, save_dir = '', language = 'en', unit = 'M', day = 16, plot = './'
  ){
  # Check API key.
  if (!connect_api_key()) {
      stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }

  # Check inputs - language.
  language <- tolower(language)
  if (!language %in% valid_langs){
    warning(
      glue("Warning: {language} doesn't support in Weatherbit API. Use default 'English' instead.")
      )
    language <- 'en'
  }

  # Check inputs - unit.
  unit <- toupper(unit)
  if (!unit %in% vaild_units){
    warning(
      glue("Warning: {unit} doesn't support in Weatherbit API. Use default 'M' - Metric (Celsius, m/s, mm) instead.")
      )
    unit <- 'M'
  }

  # Check inputs - days.
  if (!is.integer(day) & !(day >= 1 && day <= 16)){
    warning(
      glue("Warning: {day} should be an integer from 1 to 16, got {day} instead. Use default '16' instead.")
      )
    day <- 16
  }

  # Query.
  params <- list(key = api_key)
  params$city <- city
  params$lang <- language
  params$units <- unit
  params$day <- day

  # Connect API.
  response <- GET(forecast_url, query = params)

  if (response$status_code != 200){
    # Extract content as text
    content_data <- content(response, as = "text", encoding = "UTF-8")  

    # Convert JSON string to R list
    json_data <- fromJSON(content_data)  

    # Extract the error message
    error_message <- json_data$error
    stop(
        glue(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  # Convert reponse to a dataframe.
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame

  if (save_dir != ''){
    params = c(
      dataframe$city_name[1], 
      dataframe$data.datetime[1], 
      tail(dataframe$data.datetime, 1)[[1]]
      )

    save_csv(
      dataframe = dataframe, dir = save_dir, 
      endpoint = endpoint_name, params = params
      )
  }
    
  return(dataframe)
}


# Get forecast by lat and lon.
get_forecast_by_lat_lon <- function(
  lat, lon, save_dir = '', language = 'en', unit = 'M', day = 16
  ){
  # Check API key.
  if (!connect_api_key()) {
      stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }

  # Check inputs - language.
  language <- tolower(language)
  if (!language %in% valid_langs){
    warning(
      glue("Warning: {language} doesn't support in Weatherbit API. Use default 'English' instead.")
      )
    language <- 'en'
  }

  # Check inputs - unit.
  unit <- toupper(unit)
  if (!unit %in% vaild_units){
    warning(
      glue("Warning: {unit} doesn't support in Weatherbit API. Use default 'M' - Metric (Celsius, m/s, mm) instead.")
      )
    unit <- 'M'
  }

  # Check inputs - days.
  if (!is.integer(day) & !(day >= 1 && day <= 16)){
    warning(
      glue("Warning: {day} should be an integer from 1 to 16, got {day} instead. Use default '16' instead.")
      )
    day <- 16
  }

  # Check inputs - lat, lon
  if (!(is.double(lat) & is.double(lon))){
    stop(glue('Get forecast by lat/lon requires both lat and lon are double type. Got {typeof(lat)} instead.'))
  }

  # Query.
  params <- list(key = api_key)
  params$lat <- lat
  params$lon <- lon
  params$lang <- language
  params$units <- unit
  params$day <- day

  # Connect API.
  response <- GET(forecast_url, query = params)

  if (response$status_code != 200){
    # Extract content as text
    content_data <- content(response, as = "text", encoding = "UTF-8")  

    # Convert JSON string to R list
    json_data <- fromJSON(content_data)  

    # Extract the error message
    error_message <- json_data$error
    stop(
        glue(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  # Convert reponse to a dataframe.
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame

  # Save to CSV.
  if (save_dir != ''){
    params = c(
      dataframe$lat[1], 
      dataframe$lon[1], 
      dataframe$data.datetime[1],
      tail(dataframe$data.datetime, 1)[[1]]
      )

    save_csv(
      dataframe = dataframe, dir = save_dir, 
      endpoint = endpoint_name, params = params
      )
  }
 
  return(dataframe)
}


# Get forecast by postal code.
get_forecast_by_postal_code <- function(
  postal_code, country, save_dir = '', language = 'en', unit = 'M', day = 16
  ){
  # Check API key.
  if (!connect_api_key()) {
      stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }

  # Check inputs - language.
  language <- tolower(language)
  if (!language %in% valid_langs){
    warning(
      glue("Warning: {language} doesn't support in Weatherbit API. Use default 'English' instead.")
      )
    language <- 'en'
  }

  # Check inputs - unit.
  unit <- toupper(unit)
  if (!unit %in% vaild_units){
    warning(
      glue("Warning: {unit} doesn't support in Weatherbit API. Use default 'M' - Metric (Celsius, m/s, mm) instead.")
      )
    unit <- 'M'
  }

  # Check inputs - days.
  if (!is.integer(day) & !(day >= 1 && day <= 16)){
    warning(
      glue("Warning: {day} should be an integer from 1 to 16, got {day} instead. Use default '16' instead.")
      )
    day <- 16
  }

  # Query.
  params <- list(key = api_key)
  params$postal_code <- postal_code
  params$country <- country
  params$lang <- language
  params$units <- unit
  params$day <- day

  # Connect API.
  response <- GET(forecast_url, query = params)

  if (response$status_code != 200){
    # Extract content as text
    content_data <- content(response, as = "text", encoding = "UTF-8")  

    # Convert JSON string to R list
    json_data <- fromJSON(content_data)  

    # Extract the error message
    error_message <- json_data$error
    stop(
        glue(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  # Convert reponse to a dataframe.
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame

  # Save to CSV.
  if (save_dir != ''){
    params <- c(
      postal_code, 
      dataframe$country_code[1], 
      dataframe$data.datetime[1],
      tail(dataframe$data.datetime, 1)[[1]]
      )

    save_csv(
      dataframe = dataframe, dir = save_dir, 
      endpoint = endpoint_name, params = params
      )
  }
    
  return(dataframe)
}


# Get forecast by city id.
get_forecast_by_city_id <- function(
  city_id, save_dir = '', language = 'en', unit = 'M', day = 16
  ){
  # Check API key.
  if (!connect_api_key()) {
      stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }

  # Check inputs - language.
  language <- tolower(language)
  if (!language %in% valid_langs){
    warning(
      glue("Warning: {language} doesn't support in Weatherbit API. Use default 'English' instead.")
      )
    language <- 'en'
  }

  # Check inputs - unit.
  unit <- toupper(unit)
  if (!unit %in% vaild_units){
    warning(
      glue("Warning: {unit} doesn't support in Weatherbit API. Use default 'M' - Metric (Celsius, m/s, mm) instead.")
      )
    unit <- 'M'
  }

  # Check inputs - days.
  if (!is.integer(day) & !(day >= 1 && day <= 16)){
    warning(
      glue("Warning: {day} should be an integer from 1 to 16, got {day} instead. Use default '16' instead.")
      )
    day <- 16
  }

  # Query.
  params <- list(key = api_key)
  params$city_id <- city_id
  params$lang <- language
  params$units <- unit
  params$day <- day

  # Connect API.
  response <- GET(forecast_url, query = params)

  if (response$status_code != 200){
    # Extract content as text
    content_data <- content(response, as = "text", encoding = "UTF-8")  

    # Convert JSON string to R list
    json_data <- fromJSON(content_data)  

    # Extract the error message
    error_message <- json_data$error
    stop(
        paste(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  # Convert reponse to a dataframe.
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame

  # Save to CSV.
  if (save_dir != ''){
    params <- c(
      city_id, 
      dataframe$data.datetime[1],
      tail(dataframe$data.datetime, 1)[[1]]
      )

    save_csv(
      dataframe = dataframe, dir = save_dir, 
      endpoint = endpoint_name, params = params
      )
  }
    
  return(dataframe)
}


