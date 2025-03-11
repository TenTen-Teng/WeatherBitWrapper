# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(glue)

source("R/constant.R")

# Set up endpoint for forecast API.
endpoint <- "forecast/daily"

# Get forecast url.
forecast_url <- paste(base_url, endpoint, sep='')

connect_api_key <- function(){
  # Check API key.
  if (api_key == ''){
      return (FALSE)
  } else {
    return (TRUE)
  }
}

get_forecast_by_city <- function(
  city, save_dir = '', language = 'en', unit = 'M', day = 16
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
        paste(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame
  if (save_dir != ''){
    # Create directory.
    if (!file.exists(save_dir)){
      dir.create(save_dir)
    } 
    
    # Save to csv file
    write.csv(
      dataframe,
      paste(
        glue(
          save_dir, 
          "./forecase_{dataframe$city_name[1]}_{dataframe$data.datetime[1]}_{tail(dataframe$data.datetime, 1)[[1]]}.csv", 
          sep=''
          )
      ),
      row.names = FALSE
    )
  }
    
  return(dataframe)
}

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
        paste(
            "Error: Connect to API request failed :(\n", " Status code is", 
            response$status_code, "\n", " Error message is", error_message
            )
        )
    } else {
          print("Connect WeatherBit API success!")
    }
  
  dataframe <- fromJSON(content(response, "text", encoding = "UTF-8")) %>% as.data.frame
  if (save_dir != ''){
    # Create directory.
    if (!file.exists(save_dir)){
      dir.create(save_dir)
    } 
    
    # Save to csv file
    write.csv(
      dataframe,
      paste(
        glue(
          save_dir, 
          "./forecase_{dataframe$lat[1]}_{dataframe$lon[1]}_{dataframe$data.datetime[1]}_{tail(dataframe$data.datetime, 1)[[1]]}.csv", 
          sep=''
          )
      ),
      row.names = FALSE
    )
  }
    
  return(dataframe)
}

# df <- get_forecast_by_lat_lon(lat=38.123, lon=-78.543, save_dir = './')
df <- get_forecast_by_lat_lon(lat='38.123', lon=-78.543)


# get_forecast_by_postal_code <- function(){

# }

# get_forecast_by_station <- function(){

# }
