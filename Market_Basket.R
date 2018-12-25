#apriori

library(arules)

dataset <- read.transactions("Market_Basket_Optimisation.csv", sep = ',', rm.duplicates = TRUE)

summary(dataset)

itemFrequencyPlot(dataset, topN= 10)

rules <- apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.4))

rules = sort(rules, by= "lift", decreasing = T)

inspect(rules[1:10])


######ECLAT

library(arules)

dataset <- read.transactions("Market_Basket_Optimisation.csv", sep = ',', rm.duplicates = TRUE)

summary(dataset)

itemFrequencyPlot(dataset, topN= 100)

rules <- eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))

rules = sort(rules, by= "support", decreasing = T)

inspect(rules[1:10])