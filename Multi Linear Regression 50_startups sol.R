install.packages("caTools")
install.packages("car")
cat("\014")
dataset <- read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\50_Startups.csv")

dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = Profit ~ .,
               data = training_set)
par(mfrow=c(2,2))
plot(regressor)
library(car)
summary(regressor)
regressor = lm(formula = Profit ~ `RnD` + Administration + `Marketing.Spend`,
               data = training_set)

summary(regressor)
regressor = lm(formula = Profit ~ `RnD` + `Marketing.Spend`,
               data = training_set)

summary(regressor)

regressor <- lm(formula = Profit ~ `RnD`,
                data = training_set)

summary(regressor)

regressor = lm(formula = Profit ~ `RnD` + `Marketing.Spend`,
               data = training_set)

summary(regressor)

y_pred = predict(regressor, newdata = test_set)

y_pred
test_set$Profit
plot(y_pred, test_set$Profit)
