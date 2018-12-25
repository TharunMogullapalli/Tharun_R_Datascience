# Simple Linear Regression
dataset <- read.csv("Salary_Data.csv")
View(dataset)

library(caTools)
set.seed(123)

# Split the data

split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset,split == TRUE)
trest_set = subset(dataset, split == FALSE)

regressor = lm( formula = Salary ~ ., data= training_set)

y_pred = predict(regressor, newdata = trest_set)

# plot the regression equation

library(ggplot2)

ggplot()+ 
  geom_point(aes(x= training_set$YearsExperience, y= training_set$Salary), colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y= predict(regressor, newdata = training_set)),
            colour = 'blue') +
ggtitle(" Sal Vs Exp") + xlab(' Years exp') + ylab('sal')

