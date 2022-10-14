cat("\014")

var = readline("Enter years of expeience : ");

now = readline("Enter present salary : ");

var = as.numeric(var);
print(var)
now = as.numeric(now);
print(now)
library(ggplot2)
Salary_Data <- read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\Salary_Data.csv")
p <-  as.data.frame(var)
colnames(p) <- "YearsExperience"
Salary_Data$YearsExperience <- as.numeric(Salary_Data$YearsExperience)
str(Salary_Data)
ggplot(Salary_Data,aes(x = Salary, y = YearsExperience)) + geom_point()+geom_smooth(method=lm)
cor(Salary_Data$YearsExperience,Salary_Data$Salary)
model <- lm(Salary ~ YearsExperience, data = Salary_Data)
summary(model)
x = predict(model, newdata = p)
ans = x-now;
print(paste(ans, "is the predicted hike for",var,"years."))


# Logarithmic transformation -----------------------------------------------

model1 <- lm(Salary ~ log(YearsExperience), data = Salary_Data)
summary(model1)
confint(model1,level=0.95)
predict(model1,interval="predict")
x1 = predict(model1, newdata = p)
ans1 = x1-now;
print(paste(ans1, "is the predicted hike for",var,"years."))

#  Exponential model ------------------------------------------------------

model2 <- lm(log(YearsExperience)~Salary, data = Salary_Data)
summary(model2)
confint(model2,level=0.95)
x2 = exp(predict(model2, interval="predict"))
ans2 = x2-now;
print(paste(ans2, "is the predicted hike for",var,"years."))

# Quadratic model ---------------------------------------------------------

model3 <- lm(Salary~YearsExperience+I(YearsExperience^2),data=Salary_Data)
summary(model3)
confint(model3,level=0.95)
x3 =predict(model3,interval="predict")
ans3 = x3-now;
print(paste(ans3, "is the predicted hike for",var,"years."))
