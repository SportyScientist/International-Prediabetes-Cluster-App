# This skript combines the .json files into a single one

library(jsonlite)

# List of JSON file names
files <- list.files(path = "./supplementary/texts/json", pattern = "*.json", full.names = TRUE)


# Initialize an empty list to store the data
data <- list("GER" = list(), "ENG" = list())

# Loop over the files
for (file in files) {

  file_data <- fromJSON(file)

  # change order of json file because the converter gets it wrong
  file_data <- setNames(names(file_data), file_data)

  # Get the base name of the file (without extension) to use as the key
  key <- tools::file_path_sans_ext(basename(file))
  
  if (grepl("_eng", key)) {
    language <- "ENG"
    key <- sub("_eng$", "", key)
  } else {
    language <- "GER"
  }
  
  # Add the data to the list with the key
  data[[language]][[key]] <- file_data

}

write_json(data, "./supplementary/combined.json", auto_unbox = TRUE, pretty = TRUE)


