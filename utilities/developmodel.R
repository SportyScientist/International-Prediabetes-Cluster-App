library(caret)
library(glmnet)
library(dplyr)
set.seed(28051996)  

variables <- c("AGE", "WAIST", "TG", "HDL", "LDL", "BZN0_MG", "BZ60_MG", "BZ120_MG", "HBA1C", "SEX")

data <- read.csv("../06_clustering_chennai_new/analysis/data_clustered.csv") %>% 
select(all_of(variables), cluster_names) %>% mutate(SEX = ifelse(SEX == "Male", 1, 0)) %>% 
mutate(BG_0 = BZN0_MG / 18)%>%
mutate(BG_60 = BZ60_MG / 18)%>%
mutate(BG_120 = BZ120_MG / 18) %>% select(-c("BZN0_MG", "BZ60_MG", "BZ120_MG"))
names(data)


test <-  read.csv("../06_clustering_chennai_new/analysis/data_clustered.csv") %>% 
select(all_of(variables), cluster_names, id) %>% mutate(SEX = ifelse(SEX == "Male", 1, 0)) %>% 
mutate(BG_0 = BZN0_MG / 18)%>%
mutate(BG_60 = BZ60_MG / 18)%>%
mutate(BG_120 = BZ120_MG / 18) %>%
rename("PATNO" = "id","BG_0" =  "BZN0_MG", "BG_60"="BZ60_MG", "BG_120" = "BZ120_MG")
write.csv(test, "testfile.csv")


table(data_testing$cluster, data$cluster_names)

# Scaled data is now stored in scaled_trainData and scaled_testData
data$cluster_names <- as.factor(data$cluster_names)
trainIndex <- createDataPartition(data$cluster_names, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


x_train <- model.matrix(cluster_names ~ ., data = trainData)[, -1]  # Remove intercept column
y_train <- trainData$cluster_names

x_test <- model.matrix(cluster_names ~ ., data = testData)[, -1]    # Remove intercept column
y_test <- testData$cluster_names

# Train LASSO model using cross-validation
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "multinomial")  # Alpha = 1 for LASSO

# Get the best lambda (regularization parameter)
best_lambda <- cv.lasso$lambda.min

# Train final LASSO model using the best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda, family = "multinomial")

# Make predictions on the test set
lasso_pred <- predict(lasso_model, s = best_lambda, newx = x_test, type = "class")

# Evaluate the model
confMatrix <- confusionMatrix(as.factor(lasso_pred), y_test)

print(confMatrix)
row.names((coef(lasso_model)[[1]]))[2:length(row.names((coef(lasso_model)[[1]])))]

saveRDS(lasso_model, "lasso_model.rds")

