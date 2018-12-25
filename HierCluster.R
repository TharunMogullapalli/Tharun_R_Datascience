dataset<- read.csv("Mall_Customers.csv")
dataset <- dataset[4:5]

#Using the dendogram to discover optimal number of clusters

dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')

plot(dendrogram, main = "Number of Clusters in Dendrogram method", xlab = "Customer", ylab = "Distances")

# Fitting hierarchical clustering to the mall customers

hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')

y_hc = cutree(hc,5)

#Visualizing the cluster

clusplot(dataset, y_hc, lines = 0, labels =2, color = TRUE, shade = TRUE, plotchar = FALSE, span = TRUE
         , main = "Cluster of Customers in MALL", xlab= "Annual Income", ylab = "Spensing Score")