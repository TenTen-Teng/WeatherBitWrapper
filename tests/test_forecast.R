# covr::report()

# Tests for forecast.R
library(testthat)
library(httptest)

testthat(usethis::use_testthat())

usethis::use_testthat()
httptest::use_httptest()
source('R/forecast.R')

# Test get_forecast_by_city function.
# testthat::
get_forecast_by_city(city = 'Raleigh,NC')
get_forecast_by_city(
    city = 'Raleigh,NC', save_dir = './', 
    language = 'zh', unit = 'M', day = 3
    )

get_forecast_by_city(
    city = 'Raleigh,NC', save_dir = './examples', 
    )

get_forecast_by_city(
    city = 'Raleigh,NC', save_dir = './', 
    language = 'abc', unit = 'M', day = 3
    )

get_forecast_by_city(
    city = 'Raleigh,NC', unit = 'abc', day = 3
    )

get_forecast_by_city(
    city = 'Raleigh,NC', day = 30
    )

# Test get_forecast_by_lat_lon function.
df <- get_forecast_by_lat_lon(lat=38.123, lon=-78.543, save_dir = './')
df <- get_forecast_by_lat_lon(lat='38.123', lon=-78.543)


# Test get_forecast_by_postal_code
df <- get_forecast_by_postal_code(postal_code=27601, country='US', save_dir = './')
df <- get_forecast_by_postal_code(postal_code='27601', country='US', save_dir = './')

df <- get_forecast_by_city(city = 'Raleigh,NC', save_dir = './')
df <- get_forecast_by_lat_lon(lat=38.123, lon=-78.543, save_dir = './')
df <- get_forecast_by_postal_code(postal_code=27601, country='US', save_dir = './')
df <- get_forecast_by_city_id('8953360', save_dir = './')