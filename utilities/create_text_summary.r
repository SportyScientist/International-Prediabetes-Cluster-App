# Combine text files into  a single one

folder_path <- "./supplementary"

# List all .txt files in the folder
txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty string for the summary
summary_text <- ""

for (file in txt_files) {
  # Read the content of the file
  file_content <- readLines(file, warn = FALSE)
  
  # Add a title with the file name
  title <- paste("=== ", basename(file), " ===", sep = "")
  
  # Combine the title and content
  file_summary <- paste(title, paste(file_content, collapse = "\n"), sep = "\n\n")
  
  # Append to the summary
  summary_text <- paste(summary_text, file_summary, sep = "\n\n")
}

# Write the summary to a new file
writeLines(summary_text, "./supplementary/summary.txt")
