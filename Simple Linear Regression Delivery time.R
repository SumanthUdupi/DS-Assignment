cat("\014")

var = readline("Enter Sorting Time : ");

var = as.integer(var);
print(var)

library(ggplot2)
deliverytime <- read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\delivery_time.csv")

p <-  as.data.frame(var)
colnames(p) <- "SortingTime"
deliverytime$SortingTime <- as.numeric(deliverytime$SortingTime)
str(deliverytime)

ggplot(deliverytime,aes(x = DeliveryTime, y = SortingTime)) + geom_point()+geom_smooth(method=lm)
cor(deliverytime$SortingTime,deliverytime$DeliveryTime)
model <- lm(DeliveryTime ~ SortingTime, data = deliverytime)

summary(model)
ans = predict(model, newdata = p)

print(paste(ans, "is the predicted delivery time for",var))

confint(model,level = 0.95)
predict(model,interval="predict")

# Logarithmic transformation -----------------------------------------------

model1 <- lm(DeliveryTime ~ log(SortingTime), data = deliverytime)
summary(model1)
confint(model1,level=0.95)
predict(model1,interval="predict")
ans1 = predict(model1, newdata = p)
print(paste(ans1, "is the predicted delivery time for",var))

#  Exponential model ------------------------------------------------------

model2 <- lm(log(SortingTime)~DeliveryTime, data = deliverytime)
summary(model2)
confint(model2,level=0.95)
exp(predict(model2,interval="predict"))
ans2 =exp(predict(model2,interval="predict"))
print(paste(ans2, "is the predicted delivery time for",var))

# Quadratic model ---------------------------------------------------------

model3 <- lm(DeliveryTime~SortingTime+I(SortingTime^2),data=deliverytime)
summary(model3)
confint(model3,level=0.95)
exp(predict(model3,interval="predict"))
ans3 =exp(predict(model3,interval="predict"))
print(paste(ans3, "is the predicted delivery time for",var))


