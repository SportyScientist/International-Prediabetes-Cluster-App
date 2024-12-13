# Create a mock data frame with 100 rows to use for testing
library(dplyr)

mock_data <- data.frame(
  PATNO = seq(1:100),
  SEX = sample(c(0, 1), 100, replace = TRUE),
  AGE = rnorm(100, mean = 52, sd = 13.5),
  WAIST = rnorm(100, mean = 104, sd = 18),
  HDL = rnorm(100, mean = 1.34, sd = 0.36),
  LDL = rnorm(100, mean = 3.34, sd = 0.9),
  TG = rnorm(100, mean = 1.54, sd = 0.8),
  HBA1C = rnorm(100, mean = 5.5, sd = 0.4),
  BG_0 = rnorm(100, mean = 5.7, sd = 0.5),
  BG_60 = rnorm(100, mean = 10.6, sd = 2.2),
  BG_120 = rnorm(100, mean = 8, sd = 1.6)
)

write.csv(mock_data, "supplementary/example_files/example_prediabetes_cluster.csv")
openxlsx::write.xlsx(mock_data, "supplementary/example_files/example_prediabetes_cluster.xlsx")

utils::zip("supplementary/example_files.zip", files = c("supplementary/example_files/example_prediabetes_cluster.csv", "supplementary/example_files/example_prediabetes_cluster.xlsx"))

