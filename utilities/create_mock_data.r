# Create a mock data frame with 10 rows to use for testing
library(dplyr)

mock_data <- data.frame(
  PATNO = seq(1:10),
  SEX = sample(c(0, 1), 10, replace = TRUE),
  HIP = rnorm(10, mean = 105, sd = 15),
  WAIST = rnorm(10, mean = 90, sd = 12),
  BMI = rnorm(10, mean = 30, sd = 6),
  BG_0 = rnorm(10, mean = 5.7, sd = 0.5),
  BG_120 = rnorm(10, mean = 8, sd = 1),
  INS_0 = rnorm(10, mean = 80, sd = 10),
  INS_120 = rnorm(10, mean = 1000, sd = 50),
  TG = rnorm(10, mean = 1.23, sd = 0.7),
  HDL = rnorm(10, mean = 1.2, sd = 0.25)
)

write.csv(mock_data, "supplementary/example_files/example_wh.csv")
openxlsx::write.xlsx(mock_data, "supplementary/example_files/example_wh.xlsx")

utils::zip("supplementary/example_files.zip", files = c("supplementary/example_files/example_wh.csv", "supplementary/example_files/example_wh.xlsx"))



mock_data_oldapp <- mock_data %>% 
mutate(TG = TG * 88.57)%>%
mutate(HDL = HDL * 38.67)%>%
rename(NR = PATNO, HDL_mgdl = HDL)

openxlsx::write.xlsx(mock_data_oldapp, "supplementary/example_files/WH.xlsx")
