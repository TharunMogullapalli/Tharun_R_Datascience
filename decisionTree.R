# Decision Tree

hrdata <- read.csv("Position_Salaries1.csv")
View(hrdata)

hrdata <- hrdata[2:3]

# Cleaning the data

# Splitting the dataset into training and test sets - Not used in this 

# Feature Scaling - Not Used here

# install the library

# install.packages("rpart")

# Creating the regressor 

# Load the library
library(rpart)

regressor = rpart(formula = Salary ~ ., data = hrdata, control = rpart.control(minsplit = 1))

# Predictor
predictor = predict(regressor, data.frame(Level= 6.5))
# Graphical visualization of data

library(ggplot2)

x_grid = seq(min(hrdata$Level), max(hrdata$Level), .001)

ggplot() + geom_point(aes(x = hrdata$Level, y= hrdata$Salary), colour = 'red') + 
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))), 
            colour = 'blue')+ xlab('Level') + ylab('Salary')
