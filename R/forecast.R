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

get_forecast_by_city <- function(city, save_dir = ''){
    # Check API key.
    if (api_key == ''){
        stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
    }

  # Query.
  params <- list(key = api_key)
  params$city <- city

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


# get_forecast_by_lat_lon <- function(){

# }

# get_forecast_by_postal_code <- function(){

# }

# get_forecast_by_station <- function(){

# }
