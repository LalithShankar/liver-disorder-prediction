# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(svm)
library(RPART)
# Data
data <- read.csv(file.choose(), header = T)
View(data)

str(data)
xtabs(~Dataset+Total_Bilirubin, data = data)
data$Dataset <- as.factor(data$Dataset)
data$Aspartate_Aminotransferase <- as.factor(data$Aspartate_Aminotransferase)

# Visualization
pairs.panels(data[-1])
data %>%
         ggplot(aes(x=Albumin_and_Globulin_Ratio, y=Total_Protiens, fill = Albumin_and_Globulin_Ratio)) +
         geom_boxplot() +
         ggtitle("Box Plot")

data %>% ggplot(aes(x=gpa, fill = admit)) +
         geom_density(alpha=0.8, color= 'black') +
         ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.6, 0.4))
train <- data[ind == 1,]
train
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(selector ~ ., data = train, usekernel = T)
model

model1 <- model <- svm(selector ~ ., data = train, usekernel = T)

train %>%
         filter(selector == "1") %>%
         summarise(mean(Total_Protiens), sd(Total_Protiens))

plot(model)


# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$selector))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$selector))
1 - sum(diag(tab2)) / sum(tab2)




#svm

