#Import the dataset
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[3:5]
str(dataset)

#factor is used to handle categorical/qualitative variables
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

#Import the library
library(caTools)
set.seed(123)

#Split sample into training & test sets
split = sample.split(dataset$Purchased, SplitRatio = 0.75) 
training_Set = subset(dataset, split == TRUE)
test_set = subset(dataset, split ==  FALSE)

#feature scaling (-3 omits third column/depandant variable/scale)
training_Set[-3] = scale(training_Set[-3])
test_set[-3] = scale(test_set[-3])

#Fitting regression model to the classifier
classifier = glm(formula = Purchased ~., family = binomial,data = training_Set)

#predicting test set results
prob_prod = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_prod > .5, 1, 0)

#table for y_pred
cm = table(test_set[,3], y_pred)

#contour plot for training set
library(ElemStatLearn)
set = training_Set
#intervals are given by min 0 & max 1 by (0.1 way)
x1 = seq(min(set[,1]) -1, max(set[,1]) +1, by = .01)
x2 = seq(min(set[,2]) -1, max(set[,2]) +1, by = .01)
grid_Set = expand.grid(x1, x2)
colnames(grid_Set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_Set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,-3],
main = 'Logistic Regression (Training Set)',
xlab = 'Age', ylab = 'Estimated Salary',
xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_Set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
#tomato, springgreen - these are the colors
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
#Model Accuracy
Accurancy <- sum(diag(cm))/sum(cm)
Accurancy