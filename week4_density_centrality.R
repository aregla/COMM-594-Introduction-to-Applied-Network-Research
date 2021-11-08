#Week 4: Density and Centrality 
#Alejandra Regla-Vargas 
#Date: November 8, 2021 

#Load network 
rm(list = ls())
library(igraph)

#set working directory 
setwd("~/Desktop")
load("week02_networks.RData")

#remove g2 
rm(g2)
summary(g1)

#1. extract the largest connected component 
c1 <- clusters(g1)
g <- induced.subgraph(g1, which(c1$membership == which.max(c1$csize)))
g <- induced.subgraph(g1, which(c1$membership == which.max (c1$size)))

#2. Calculate Freeman's centrality measures and assign the values to a node attribute 
V(g)$degree <- degree(g, normalized = T)
V(g)$betweenness <- betweenness(g, normalized = T)
V(g)$closeness <- closeness(g, normalized = T)

#normalized: normalized	
#Logical scalar, whether to calculate the normalized closeness. 
#Normalization is performed by multiplying the raw closeness by n-1, 
#where n is the number of vertices in the graph.

#summary statistics 
summary(V(g)$degree)
summary(V(g)$betweenness)
summary(V(g)$closeness)


#plot the distributions 
par(mfrow = c(2,3)) 
hist(V(g)$degree, col = "light blue", main = "degree", cex.main = 1.5, xlab = "")
hist(V(g)$betweenness, col = "orange", main = "betweenness",cex.main = 1.5, xlab = "")
hist(V(g)$closeness, col = "light green", main = "closeness",cex.main = 1.5, xlab = "")

plot(g, vertex.size = V(g)$degree*30,
     vertex.label = NA, vertex.color = "light blue",
     layout = layout.fruchterman.reingold)

plot(g, vertex.size = V(g)$betweenness*30,
     vertex.label = NA, vertex.color = "orange",
     layout = layout.fruchterman.reingold)

plot(g, vertex.size = V(g)$closeness*25, 
     vertex.label = NA, vertex.color = "light green",
     layout = layout.fruchterman.reingold)



#eigenvector centrality 
V(g)$eigen <- eigen_centrality(g)$vector
summary(V(g)$eigen)

#eigenvector graph 
par(mfrow = c(1,2)) 
hist(V(g)$eigen, col = "red", main = "eigenvector", cex.main = 1.5, xlab = "")

plot(g, vertex.size = V(g)$eigen*30, 
     vertex.label = NA, vertex.color = "red",
     layout = layout.fruchterman.reingold)

#create a data frame for later use 
df_g <- as.data.frame(V(g)$name)
df_g$degree <- V(g)$degree
df_g$betweenness <- V(g)$betweenness
df_g$closeness <- V(g)$closeness 
df_g$eigen <- V(g)$eigen
names(df_g) <- c("names", "degree", "betweenness", "closeness", "eigen")
head(df_g)

#correlation matrix of centrality measures 
cor(df_g[,c(2:5)], method = "spearman")

#co-authorship data 
g2 <- read_graph(file = "week04_netscience_authors_network.graphml", format = "graphml")
summary(g2)

#1. examine the largest connected component 
cl <- clusters(g2)
g <- induced.subgraph(g2, which(cl$membership == which.max(cl$csize)))

#visualzied network 
par(mar = c(0,0,0,0))
plot(g, vertex.size = 3, 
     vertex.label = NA, vertex.color = "red",
     layout = layout.fruchterman.reingold)

#calculate centrality measures 
V(g)$degree <- degree(g, normalized = T)
V(g)$betweenness <- betweenness(g, normalized = T)
V(g)$closeness <- closeness(g, normalized = T)
V(g)$eigen <- eigen_centrality(g)$vector

#create a data frame 
df_g2 <- as.data.frame(V(g)$label)
df_g2$degree <- V(g)$degree
df_g2$betweenness <- V(g)$betweenness
df_g2$closeness <- V(g)$closeness
df_g2$eigen <- V(g)$eigen
names(df_g2) <- c("name", "degree", "betweenness", "closeness", "eigen")
head(df_g2)

#plotpar(mfrow = c(2,2), mar = c(5,5,5,5)) 
hist(V(g)$degree, col = "light blue", main = "degree", cex.main = 1.5, xlab = "")
hist(V(g)$betweenness, col = "orange", main = "betweenness",cex.main = 1.5, xlab = "")
hist(V(g)$closeness, col = "light green", main = "closeness",cex.main = 1.5, xlab = "")
hist(V(g)$eigen, col = "red", main = "eigenvector",cex.main = 1.5, xlab = "")

#rank authors by centrality measures: degree 
head(df_g2[order(-df_g2$degree),], n = 10)

#betweenness
head(df_g2[order(-df_g2$betweenness),], n = 10)

#closeness
head(df_g2[order(-df_g2$closeness),], n = 10)

#eigen
head(df_g2[order(-df_g2$eigen),], n = 10)

#correlation matrix 
cor(df_g2[,c(2:5)], method = "spearman")

#extract central node 
egonet <- make_ego_graph(g,1,nodes = match("BARABASI, A", V(g)$label)) 
egonet <- egonet[[1]] 

par(mfrow = c(1,1), mar = c(0,0,0,0)) 
plot(egonet,vertex.size = 0, 
     vertex.label.cex = 1, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)
