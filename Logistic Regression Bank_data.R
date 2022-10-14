cat("\014")

bank_full = read.csv("C:\\Users\\suman\\Desktop\\Assignment\\DS Assignment\\bank-full.csv")

head(bank_full)
bank_full$job <-as.factor(bank_full$job)
bank_full$marital <-as.factor(bank_full$marital)
bank_full$education <-as.factor(bank_full$education)
bank_full$default <-as.factor(bank_full$default)
bank_full$housing <-as.factor(bank_full$housing)
bank_full$loan <-as.factor(bank_full$loan)
bank_full$contact <-as.factor(bank_full$contact)
bank_full$month <-as.factor(bank_full$month)
bank_full$poutcome <-as.factor(bank_full$poutcome)
bank_full$y <-as.factor(bank_full$y)

str(bank_full)

x<-glm(y~.,data = bank_full, family = "binomial")
summary(x)

bank_full$predict<-predict(x,data=bank_full,type="response")
a=table(bank_full$y,bank_full$predict>0.5)
n = sum(a)
diag = diag(a) 
rowsums = apply(a, 1, sum)
colsums = apply(a, 2, sum) 
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)
print("Hence according to the above observation, the model is better at recognizing false.")

library(ROCR)
rocrpred<-prediction(bank_full$predict,bank_full$default)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
