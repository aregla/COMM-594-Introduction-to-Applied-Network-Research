#Week 2: Data format import/export steps 
#Name: Alejandra Regla-Vargas 
#Date: October 04, 2021 

#Import data 
#Nodes (people) and edges (connection to connection) needed 
#Directed: ordered pair of vertices
#Undirected: unordered pair of vertices

#clear working directory 
rm(list = ls())

#load packages 
library(igraph)

#working directory 
getwd()
setwd("~/Desktop/Network Analysis/week 02")

#import data 
#note: To import the data, we ask R to read the csv files (the argument ‘header’
#tells R that the columns are labeled)
nodes <- read.csv("week2_nodes_table.csv", header = T)
edges <- read.csv("week2_edges_table.csv", header = T)

#view data
head(edges)
head(nodes)

#create a network from the graph 
g1 <- graph_from_edgelist(as.matrix(edges[,1:2]), directed = FALSE)
summary(g1)

#add names 
V(g1)$name <- as.character(nodes$Label)

#plot graph 
plot(g1)

#visualization #2
par(mar = c(0,0,0,0))
plot(g1, vertex.size = 5, vertex.label.cex = 0.6, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)
#interactive visualization
tkplot(g1,vertex.size = 5, 
       vertex.label.cex = 0.6, vertex.label.color = "black",
       layout = layout.fruchterman.reingold)

#check commands 
?V

#import data from Gephi 
g2 <- read_graph("week2.graphml", format = "graphml")
plot(g2,vertex.size = 5, vertex.label.cex = 0.6, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#export visaulization 
png(filename = "week02_TrumpWorld.png", width = 1600,height = 1400)
plot(g2,vertex.size = 5, vertex.label.cex = 1, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)
dev.off()

#save objects 
save(g1, g2, file = "week02_networks.RData")




