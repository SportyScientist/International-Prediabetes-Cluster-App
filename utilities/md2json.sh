#!/bin/bash

# bash skript to convert text files into .json format using CLI from vertopal 
# using CLI costs money
#vertopal config --app "af5abd73-d96d-c523-f3b7-45c2cccd976f" --token ".X9bF1FsXzHBn4w01em71Guhxj01DUd9orhxkZKzaI25to0Z29LqZIC6UwoxlcXbwAL5a8.8bIR6EScA41RuHW9uGBWdOt5S6dgd"

directory="/mnt/c/Users/idm/OneDrive/Documents/Projects/prediabetes_clusters_shiny/supplementary/texts"

for file in "$directory"/raw/*.txt; do
  if [ -f "$file" ]; then
    echo "Converting $file to json..."
    vertopal convert "$file" --to json -d "$directory"/json
  fi
done