# create random example files to demonstrate file structure
# sample files can be downloaded from website

setwd("C:/Users/idm/OneDrive/Documents/Projects/prediabetes_clusters_shiny/supplementary")

wh <- data.frame(
  PATNO = 1:5,
  SEX = c(1, 1, 0, 0, 1),
  HIP = rnorm(5, mean = 90, sd = 5),
  WAIST = rnorm(5, mean = 70, sd = 5),
  BMI = rnorm(5, mean = 25, sd = 2),
  BG_0 = rnorm(5, mean = 90, sd = 10),
  BG_120 = rnorm(5, mean = 110, sd = 10),
  INS_0 = rnorm(5, mean = 10, sd = 2),
  INS_120 = rnorm(5, mean = 15, sd = 3),
  TG = rnorm(5, mean = 150, sd = 20),
  HDL = rnorm(5, mean = 45, sd = 5)
)
wh <- round(wh, 0)

write.csv(wh, "example_wh.csv", row.names = FALSE)
openxlsx::write.xlsx(wh, "example_wh.xlsx", rowNames = FALSE)

zip("example_files.zip", c("example_wh.csv", "example_wh.xlsx"))

