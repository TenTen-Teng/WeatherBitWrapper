# Load necessary packages
library(httr)
library(jsonlite)

source("constant.R")
source("utils.R")

# Set up endpoint for alerts API
endpoint <- "alerts"

# Get alerts url
alerts_url <- paste(base_url, endpoint, sep='')

# Get alerts by lat&lon
weather_alert_lat <- function(lat, lon, save_dir = ''){
  # Check API key.
  if (!connect_api_key()) {
    stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }
  # Check lat & lon
  if (missing(lat) || missing(lon)) {
    stop("All parameters (lat, lon) are required.")
  }
  
  # Set parameters 
  params <- list(
    lat = lat,
    lon = lon,
    key = api_key
  )
  
  # Request API
  response <- GET(alerts_url, query = params)
  
  # Check response status
  if (response$status_code == 200) {
    print("Connected to WeatherBit API successfully!")
    response_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
  } else {
    stop(paste("Error: API request failed! \nStatus code:", response$status_code))
  }
  
  # Location data
  location <- data.frame(
    city_name = response_data$city_name,
    state_code = response_data$state_code,
    country_code = response_data$country_code,
    lat = response_data$lat,
    lon = response_data$lon,
    timezone = response_data$timezone
  )
  
  # Ensure output is a dataframe even if no alerts exist
  if (!("alerts" %in% names(response_data)) || length(response_data$alerts) == 0) {
    message("No active weather alerts for the location.")
    
    # create an empty alerts dataframe with location
    empty_alerts_df <- data.frame(
      title = NA, severity = NA, effective_local = NA, expires_local = NA,
      regions = NA, uri = NA
    )
    
    # combine location with empty alerts dataframe
    return(cbind(location, empty_alerts_df))
  }
  
  # convert alerts to a data frame
  alerts_df <- as.data.frame(response_data$alerts)
  
  # flatten 'regions' if it's a list
  if ("regions" %in% colnames(alerts_df)) {
    alerts_df$regions <- sapply(alerts_df$regions, function(x) paste(x, collapse = "; "))
  }
  
  dataframe <- cbind(alerts_df, location)
  
  # Save to CSV
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


# Get alerts by city name
weather_alert_city <- function(city, state = NULL, country = NULL, save_dir = ''){
  # Check API key.
  if (!connect_api_key()) {
    stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }
  # Check city name 
  if (missing(city)) {
    stop("Parameter 'city' is required.")
  }
  
  # handle the optional parameter
  if (is.null(state) && is.null(country)) {
    stop("Error: The API requires either a state or a country along with the city name.")
  }
  
  # Construct city parameter with state if provided
  if (!is.null(state)) {
    # Replace spaces with "+" for multi-word states
    formatted_state <- gsub(" ", "+", state)
    city_param <- paste0(city, ",", formatted_state)
  } else {
    city_param <- city
  }
  
  # Set parameters 
  params <- list(
    city = city_param,
    key = api_key
  )
  
  # Add the optional parameter 
  if (!is.null(country)) {
    params$country <- country
  }
  
  # Request API
  response <- GET(alerts_url, query = params)
  
  # Check response status
  if (response$status_code == 200) {
    print("Connected to WeatherBit API successfully!")
    response_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
  } else {
    stop(paste("Error: API request failed! \nStatus code:", response$status_code))
  }
  
  # Location data
  location <- data.frame(
    city_name = response_data$city_name,
    state_code = response_data$state_code,
    country_code = response_data$country_code,
    lat = response_data$lat,
    lon = response_data$lon,
    timezone = response_data$timezone
  )
  
  # Ensure output is a dataframe even if no alerts exist
  if (!("alerts" %in% names(response_data)) || length(response_data$alerts) == 0) {
    message("No active weather alerts for the location.")
    
    # Create an empty alerts dataframe with location
    empty_alerts_df <- data.frame(
      title = NA, severity = NA, effective_local = NA, expires_local = NA,
      regions = NA, uri = NA
    )
    
    # combine location with empty alerts dataframe
    return(cbind(location, empty_alerts_df))
  }
  
  # convert alerts to a data frame
  alerts_df <- as.data.frame(response_data$alerts)
  
  # flatten 'regions' if it's a list
  if ("regions" %in% colnames(alerts_df)) {
    alerts_df$regions <- sapply(alerts_df$regions, function(x) paste(x, collapse = "; "))
  }
  
  dataframe <- cbind(alerts_df, location)
  
  # Save to CSV
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


# Get alerts by postal code
weather_alert_postal <- function(postal_code, country = NULL, save_dir = ''){
  # Check API key.
  if (!connect_api_key()) {
    stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }
  # Check postal code
  if (missing(postal_code)) {
    stop("Parameter 'postal_code' is required.")
  }
  
  # Set parameters 
  params <- list(
    postal_code = postal_code,
    key = api_key
  )
  
  # Add the optional parameter 
  if (!is.null(country)) {
    params$country <- country
  }
  
  # Request API
  response <- GET(alerts_url, query = params)
  
  # Check response status
  if (response$status_code == 200) {
    print("Connected to WeatherBit API successfully!")
    response_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
  } else {
    stop(paste("Error: API request failed! \nStatus code:", response$status_code))
  }
  
  # Location data
  location <- data.frame(
    city_name = response_data$city_name,
    state_code = response_data$state_code,
    country_code = response_data$country_code,
    lat = response_data$lat,
    lon = response_data$lon,
    timezone = response_data$timezone
  )
  
  # Ensure output is a dataframe even if no alerts exist
  if (!("alerts" %in% names(response_data)) || length(response_data$alerts) == 0) {
    message("No active weather alerts for the location.")
    
    # create an empty alerts dataframe with location
    empty_alerts_df <- data.frame(
      title = NA, severity = NA, effective_local = NA, expires_local = NA,
      regions = NA, uri = NA
    )
    
    # combine location with empty alerts dataframe
    return(cbind(location, empty_alerts_df))
  }
  
  # convert alerts to a data frame
  alerts_df <- as.data.frame(response_data$alerts)
  
  # flatten 'regions' if it's a list
  if ("regions" %in% colnames(alerts_df)) {
    alerts_df$regions <- sapply(alerts_df$regions, function(x) paste(x, collapse = "; "))
  }
  
  dataframe <- cbind(alerts_df, location)
  
  # Save to CSV
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


# Get alerts by city id 
weather_alert_id <- function(city_id, save_dir = ''){
  # Check API key.
  if (!connect_api_key()) {
    stop("Error: The WeatherBit API is empty! Please set up your WeatherBit
        API key to your enviornment.
        ")
  }
  # Check postal code
  if (missing(city_id)) {
    stop("Parameter 'city_id' is required.")
  }
  
  # Set parameters 
  params <- list(
    city_id = city_id,
    key = api_key
  )
  
  # Request API
  response <- GET(alerts_url, query = params)
  
  # Check response status
  if (response$status_code == 200) {
    print("Connected to WeatherBit API successfully!")
    response_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
  } else {
    stop(paste("Error: API request failed! \nStatus code:", response$status_code))
  }
  
  # Location data
  location <- data.frame(
    city_name = response_data$city_name,
    state_code = response_data$state_code,
    country_code = response_data$country_code,
    lat = response_data$lat,
    lon = response_data$lon,
    timezone = response_data$timezone
  )
  
  # Ensure output is a dataframe even if no alerts exist
  if (!("alerts" %in% names(response_data)) || length(response_data$alerts) == 0) {
    message("No active weather alerts for the location.")
    
    # create an empty alerts dataframe with location
    empty_alerts_df <- data.frame(
      title = NA, severity = NA, effective_local = NA, expires_local = NA,
      regions = NA, uri = NA
    )
    
    # combine location with empty alerts dataframe
    return(cbind(location, empty_alerts_df))
  }
  
  # convert alerts to a data frame
  alerts_df <- as.data.frame(response_data$alerts)
  
  # flatten 'regions' if it's a list
  if ("regions" %in% colnames(alerts_df)) {
    alerts_df$regions <- sapply(alerts_df$regions, function(x) paste(x, collapse = "; "))
  }
  
  dataframe <- cbind(alerts_df, location)
  
  # Save to CSV
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





