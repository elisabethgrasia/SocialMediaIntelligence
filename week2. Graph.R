# Introduction to Graphs
# 14th March 2022

install.packages("igraph")
library("igraph")

# Creating a Graph
## To create a graph, we can use the formula interface or provide an adjacency (berdampingan) matrix.

# Graph Formula
g1 = graph.formula(A-B, A-C, A-D, B-D)
print(g1)
V(g1) #viewing the vertices
E(g1) # viewing the edges

## Visualizing the graph by plotting it
par(mar = c(0, 0, 0, 0))
V(g1)$label.cex=0.7
plot(g1, layout = layout.fruchterman.reingold, vertex.size = 30)


# Adjacency Matrix
A = matrix(0, 4, 4)
A[1, c(2, 3, 4)] = 1
A[2,c(1,4)] = 1
A[3, 1] = 1
A[4,c(1, 2)] = 1
print(A)  

## Creating the Matrix
g2 = graph.adjacency(A)
par(mar = c(0, 0, 0, 0))
V(g2)$label.cex = 0.7
plot(g2, layout = layout.fruchterman.reingold, vertex.size = 20)

## Making an undirected graph from the matrix
g2a = graph_from_adjacency_matrix(A, mode = "undirected")
par(mar = c(0, 0, 0, 0))
V(g2a)$label.cex = 0.7
plot(g2a, layout = layout.fruchterman.reingold, vertex.size = 20)

# Edge List
e1 = matrix(c("A", "A", "A", "B", "B", "C", "D", "D"), 4, 2)
print(e1)

g4 = graph.edgelist(e1, directed = FALSE)
par(mar = c(0, 0, 0, 0))
V(g4)$label.cex = 0.7
plot(g4, layout = layout.fruchterman.reingold, vertex.size = 20)

# Creating Random Graph
## Erdos-Renyi Graph
g.er = erdos.renyi.game(n = 100, p = 0.1)
par(mar = c(0, 0, 0, 0))
V(g.er)$label.cex = 0.7
plot(g.er, layout = layout.fruchterman.reingold, vertex.size = 8)
plot(g.er, layout = layout.fruchterman.reingold, vertex.size = 5)

## Barabasi-Albert Graph
g.ba = barabasi.game(n = 100, directed = FALSE)
par(mar = c(0, 0, 0, 0))
V(g.ba)$layout.cex = 0.7
plot(g.ba, layout = layout.fruchterman.reingold, vertex.size = 8)

# Examining the Graphs
## Density
graph.density(g.er)
graph.density(g.ba)

## Diameter
diameter(g.er)
diameter(g.ba)

## Degree Distribution
degree.distribution(g1)
degree.distribution(g.ba)
degree.distribution(g.er)
degree(g.ba, 2)

## Closeness
closeness(g.ba)

## Betweenness
betweenness(g.er)

# Small Graph
g3 = graph.formula(A-B, A-C, A-D, B-D, B-E, E-D, C-E)
par(mar = c(0, 0, 0, 0))
V(g3)$label.cex = 0.7
plot(g3, layout = layout.fruchterman.reingold, vertex.size = 20)

## Degree of each vertex
degree(g3)

## Distribution
degree.distribution(g3)

## Closeness Centrality
closeness(g3)

## Betweenness Centrality
betweenness(g3)

