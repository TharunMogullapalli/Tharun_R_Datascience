# Association rules for Market Basket Analysis
library(arules)
library(arulesViz)
library(RColorBrewer)

data(Groceries)

#show the dimensions of the transaction object

print(dim(Groceries)[1]) # 9835 market baskets for shopping trips
print(dim(Groceries)[2]) # 169 initial Store Items

# examine frequency for each item with support greater than 0.025
pdf(file = "fig_market_basket_initial_item_support.pdf",width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.025, cex.names = 0.8, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Item", "\n(Item Relative Frequency)"))
dev.off()

# explore possibilities for combining similar items
print(head(itemInfo(Groceries)))
print(levels(itemInfo(Groceries)[["level1"]]))
print(levels(itemInfo(Groceries)[["level2"]]))

# to create a more meaningful set of items
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])

print(dim(groceries)[1])
print(dim(groceries)[2])

pdf(file = "fig_market_basket_initial_item_support.pdf",width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.025, cex.names = 0.8, xlim = c(0,0.5),
                  type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Item", "\n(Item Relative Frequency)"))
dev.off()

first.rules <- apriori(groceries, parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules)) # yields 69,921 rules...

second.rules <- apriori(groceries, parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules)) # yields 344 rules


#data visualization of association rules in scatter plot

plot(second.rules, control = list(jitter=2, col = rev(brewer.pal(9,"Greens")[4:9])),shading = "lift")
dev.off()

# grouped matrix of rules

plot(second.rules, method = "grouped", control = list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()

# Select rules with vegetables in consequent

vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")
inspect(vegie.rules)

# sort by lift and identify the top 10 rules

top.vegie.rules <- head(sort(vegie.rules, decreasing = TRUE, by = "lift"),10)
inspect(top.vegie.rules)

plot(top.vegie.rules, method = "graph", control = list(type= "items"), shading = "lift")
dev.off()

