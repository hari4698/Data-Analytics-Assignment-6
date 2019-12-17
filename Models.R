library(caret)
library(tidyr)
library(dplyr)
library(randomForest)
library(Hmisc)

all_violations <- read.csv("C:\\Users\\USER\\Documents\\College\\F19\\Data_Analytics\\Assignments\\Final project\\Datasets\\SDWA_downloads\\SDWA_VIOLATIONS.csv")
str(all_violations)
levels(all_violations$SOURCE_WATER)
describe(all_violations)

#EDA
g1 <- ggplot(all_violations,aes(x=PWS_SIZE))+stat_count(color="white", fill="lightblue")+ ggtitle ("Public Water Systems by size \nof population served") +
  xlab("PWS_Size") +
  ylab("Total count")
g2 <- ggplot(all_violations,aes(x=SOURCE_WATER))+stat_count(color="white", fill="#baf6ff")+ ggtitle ("Source of water") +
  xlab("Source of Water") +
  ylab("Total count")
b1 <- ggplot(all_violations,aes(all_violations$POPULATION_SERVED_COUNT))+geom_area(stat = "bin")
grid.arrange(g1, g2,nrow = 1)
b1

dataset <- all_violations[,c("SOURCE_WATER","PWS_SIZE",
              "POPULATION_SERVED_COUNT","HEALTH_BASED")]
str(dataset)

validation_index <- createDataPartition(
                              dataset$HEALTH_BASED,
                             p=0.80, list = FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

dataset_1 <- dataset[1:10000,]

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
set.seed(7)
fit.rf <- train(HEALTH_BASED~.,data=dataset_1,method="rf",
                 metric = metric ,trControl=control)

set.seed(7)
fit.lda <- train(HEALTH_BASED~.,data=dataset,method="lda",
                 metric = metric ,trControl=control)

results <- resamples(list(lda=fit.lda, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.lda)
print(fit.rf)
predictions_lda <- predict(fit.lda,validation)
confusionMatrix(predictions_lda,validation$HEALTH_BASED)
predictions_rf <- predict(fit.lda,validation)
confusionMatrix(predictions_rf,validation$HEALTH_BASED)


