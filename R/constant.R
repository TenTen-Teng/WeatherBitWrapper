# Set base url.
base_url <- "https://api.weatherbit.io/v2.0/"

# Get API_KEY from environment.
api_key <- Sys.getenv("WEATHERBIT_API_KEY")

# Available language list:
# en - [DEFAULT] English
# ar - Arabic
# az - Azerbaijani
# be - Belarusian
# bg - Bulgarian
# bs - Bosnian
# ca - Catalan
# cz - Czech
# da - Danish
# de - German
# fi - Finnish
# fr - French
# el - Greek
# es - Spanish
# et - Estonian
# ja - Japanese
# hr - Croation
# hu - Hungarian
# id - Indonesian
# it - Italian
# is - Icelandic
# iw - Hebrew
# kw - Cornish
# lt - Lithuanian
# nb - Norwegian Bokmål
# nl - Dutch
# pl - Polish
# pt - Portuguese
# ro - Romanian
# ru - Russian
# sk - Slovak
# sl - Slovenian
# sr - Serbian
# sv - Swedish
# tr - Turkish
# uk - Ukrainian
# zh - Chinese (Simplified)
# zh-tw - Chinese (Traditional)
valid_langs <- c(
    'en', 'ar', 'az', 'be', 'bg', 'bs', 'ca', 'cz', 'da', 'de', 'fi', 'fr', 
    'el', 'es', 'et', 'ja', 'hr', 'hu', 'id', 'it', 'is', 'iw', 'kw', 'lt', 
    'nb', 'nl', 'pl', 'pt', 'ro', 'ru', 'sk', 'sl', 'sr', 'sv', 'tr', 'uk', 
    'zh', 'zh-tw'
    )


# Available units:
# M - [DEFAULT] Metric (Celsius, m/s, mm)
# S - Scientific (Kelvin, m/s, mm)
# I - Fahrenheit (F, mph, in)
vaild_units <- c('M', 'S', 'I')