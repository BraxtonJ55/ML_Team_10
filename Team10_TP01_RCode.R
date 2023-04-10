###################################################
######## TEAM 10: Braxton, Lindabeth, Zach ########
###################################################

#TP01: Exercise 12.6.9 from the Textbook
# In this code we look at hierarchical clustering using the US Arrests Dataset from the ISLR2 library 
rm(list=ls())
library(ISLR2)
set.seed(1)
data("USArrests")
str(USArrests)


# PART A): Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.
dist <- dist(USArrests, method = 'euclidean')

hc_complete <- hclust(dist, method = 'complete') #Uses dist as the dissimilarity measure


plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", 
     cex = 0.65, cex.main = 1.2, cex.lab = 1.1, las = 2, 
     hang = -1, col = "#6E9DA4")

rect.hclust(hc_complete, k = 3, border = "black") #This is to better see the boundaries between clusters

# PART B): Cut the dendrogram at a height that results in three distinct clusters. 
# Which states belong to which clusters?

num_clusters <- cutree(hc_complete, 3) # Cutting the dendrogram
num_clusters # Outputs the State and the cluster to which they belong 

# Separating clusters in high, mid, and low crime states 
high_crime_cluster <- USArrests[num_clusters == 1, ]
mid_crime_cluster <- USArrests[num_clusters == 2, ]
low_crime_cluster <- USArrests[num_clusters == 3, ]

#Look at the Clusters 

# PART C): Hierarchically cluster the states using complete linkage and Euclidean distance, after 
# scaling the variables to have standard deviation one. 

scaled_data <- scale(USArrests)
scaled_dist <- dist(scaled_data)
scaled_hc_complete <- hclust(scaled_dist, method = 'complete')


num_clusters_scaled <- cutree(scaled_hc_complete, 3)
num_clusters_scaled

#Comparing graphs side by side
par(mfrow = c(1, 2))

plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "", 
     cex = 0.65, cex.main = 1.2, cex.lab = 1.1, las = 2, 
     hang = -1, col = "#6E9DA4")

rect.hclust(hc_complete, k = 3, border = "black")

plot(scaled_hc_complete, main = "Complete Linkage with Scaled Data", xlab = "", sub = "", 
     cex = 0.65, cex.main = 1.2, cex.lab = 1.1, las = 2, 
     hang = -1, col = "#6E9DA4")

rect.hclust(scaled_hc_complete, k = 3, border = "black")
# The boundaries between clusters shifted. There is now one large group with 2 smaller subgroups. 



