#import datset
library(e1071)
Data <- read.csv(file="f:/layer.csv", header=TRUE, sep=",")
newdata <-  read.csv(file="f:/RBL/final dataset.csv", header=TRUE, sep=",")
Data = Data[3:4]
View(Data)
     View(Dataset)

regressor = svm(formula = chol ~ ., 
                data = Data,
                type = 'eps-regression')


Y_pred = predict(regressor, data.frame(Dataset = 1))

library(ggplot2)
ggplot() + geom_point(aes(x = Data$Dataset, y = Data$chol),
                      color = 'red') + 
  geom_point(aes(x = Data$Dataset, y = predict(regressor, 
                              newdata = Data)),
             colour = 'blue') + 
ggtitle('Truth or Bluff (SVR)') + 
  xlab('Dataset') + 
  ylab('chol')
library(gridExtra)
p1 <- qplot(x = chol, data = Data)
p1
p2 <- qplot(x = log10(chol + 1), data = Data)
p2
p3 <- qplot(x = sqrt(chol), data = Data)
p3

ecg <- qplot(x = RestECG, data = Data)
ecg

Data$RestECG
test <- Data$RestECG
which(is.na(test))
Data$RestECG[which(is.na(Data$RestECG))] <- mean(Data$RestECG,
 na.rm = TRUE)
Data
summary(Data)
summary(Data$chol)

summary(log10(Data$chol + 1))

summary(sqrt(Data$chol))
qq <- na.omit(Data)
View(qq)
        
is.na(newdata)
which(is.na(newdata$Albumin_and_Globulin_Ratio)) 
newdata$Albumin_and_Globulin_Ratio[which(is.na(newdata$Albumin_and_Globulin_Ratio))]<- mean(newdata$Albumin_and_Globulin_Ratio,na.rm = TRUE)
View(newdata)
