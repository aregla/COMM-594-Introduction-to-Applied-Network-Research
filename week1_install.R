#Week 1: Install i-graph and gephi 
#Name: Alejandra Regla-Vargas 
#Date: October 4, 2021 

#install packages 
install.packages("igraph")

#load packages 
library(igraph)

#network example 
example <- sample_pa(20)
plot(example)

#Undirected network: unordered pair of vertices 
#Directed network: ordered pair of vertices 
