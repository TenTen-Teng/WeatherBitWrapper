# Helpful functions.
source("R/constant.R")

# Connect API key function.
connect_api_key <- function(){
  # Check API key.
  if (api_key == ''){
      return (FALSE)
  } else {
    return (TRUE)
  }
}

# Save dataframe into a CSV file.
save_csv <- function(dataframe, dir = '', endpoint = '', params = c()){
    if (dir == '') {
        warning("Empty directory. Use current directory instead.")
        dir <- './'
    }

    # Create directory.
    if (!file.exists(dir)){
      dir.create(dir)
    } 

    # Generate file name.
    file_name <- ''
    if (length(params) != 0){
        file_name <- paste(
            glue({endpoint}), '_', glue_collapse(params, sep = "_"), sep = ''
            ) %>% glue('.csv')
    } else {
        file_name <- glue({endpoint}, '_', 'dataframe.csv')
    }
    print(file_name)
    path <- paste(dir, file_name, sep='')

    # Save to csv file
    tryCatch(
        {
            write.csv(dataframe, file = path, row.names = FALSE)
            cat(glue("File {file_name} saved successfully into {path}.\n"))
        }, warning = function(w) {
            cat("Warning: ", conditionMessage(w), "\n")
        }, error = function(e) {
            cat("Error: ", conditionMessage(e), "\n")
        }
    )
}

