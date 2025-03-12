# Load necessary functions
source("R/current_weather_functions.R")

# Load API key from environment variable
api_key <- Sys.getenv("WEATHERBIT_API_KEY")

# Run tests
print("Testing temperature function...")
print(get_current_temperature("Kelowna,CA", api_key, by = "city"))

print("Testing wind function...")
print(get_current_wind("Kelowna,CA", api_key, by = "city"))

print("Testing precipitation function...")
print(get_current_precipitation("Kelowna,CA", api_key, by = "city"), width = Inf)