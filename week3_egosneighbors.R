#Week 3: Networks, Egos, and Neighbors 
#Alejandra Regla-Vargas 
#Date: October 04, 2021 

#load data 
load("week02_networks.RData")

#remove g2, since we only need g1
?rm
rm(g2)

#summary of g1
summary(g1)

#print list of the nodes 
V(g1)$name

#print edges 
?ends
ends(g1, E(g1))

#produce graph
?par
par(mar = c(0,0,0,0))
plot(g1,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)
#Note: too dense to be legible; visualizing just a subset of nodes help us gain clarity.

#extract an ego network find trump's unique ID
match("DONALD J. TRUMP", V(g1)$name)

#ego network: someone's personal network 
dt_egonet <- make_ego_graph(g1, 1 ,nodes = 21) 
dt_egonet <- dt_egonet[[1]]

plot(dt_egonet,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#ivanka trump's network 
it_egonet <- make_ego_graph(g1, 1, nodes = match("IVANKA TRUMP", V(g1)$name))
it_egonet <- it_egonet[[1]]

plot(it_egonet,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#donald trump jr. 
dt_jr_egonet <- make_ego_graph(g1,1, nodes = match("DONALD TRUMP JR.", V(g1)$name))
dt_jr_egonet <- dt_jr_egonet[[1]]

plot(dt_jr_egonet,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#jared kushner
jk_egonet <- make_ego_graph(g1, 1, nodes = match("JARED KUSHNER", V(g1)$name))
jk_egonet <- jk_egonet[[1]]

plot(jk_egonet,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#extended network
jk_egonet_extended <- make_ego_graph(g1, 2,nodes = match("JARED KUSHNER", V(g1)$name))
jk_egonet_extended <- jk_egonet_extended[[1]]

plot(jk_egonet_extended,vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout= layout.fruchterman.reingold)

#remove ego 
dt_egonet_minus <- delete_vertices(dt_egonet, match("DONALD J. TRUMP", V(dt_egonet)$name))

plot(dt_egonet_minus,vertex.size = 4, 
     vertex.label.cex = 0.5, vertex.label.color = "black",
     layout = layout.fruchterman.reingold)

#tell you that most of the people in the data set are connected to each other
summary(dt_egonet_minus)

#calculate overlap in neighborhoods: 
dt_jr_it <- intersection(dt_jr_egonet,it_egonet, keep.all.vertices = F)
summary(dt_jr_it)

#overlap between ivanka and donald jr. 
vcount(dt_jr_it) / vcount(it_egonet) #Ivanka Trump shares 50% of her contacts with her brother:





