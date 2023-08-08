# week 09
# Link Analysis and Web Search
# Elisabeth Putri


# Loading Libraries
library(igraph)
library(Matrix)

## HUBS AND AUTHORITIES
g1 = graph.formula(A-+C, A-+E, A-+F, A-+B, A-+D, B-+C, B-+D, C-+E, C-+F,
                   D-+A, D-+C, E-+F, E-+B)
plot(g1)

A=get.adjacency(g1)

# Compute the hub score and authority score of each node
N = 100 # the number of iterations (a large number)

# eigen value decomposition method
hub=eigen(A*t(A))
hub
hub$vectors[,1]
authority=eigen(t(A)*A)
authority
authority$vectors[,1]

# iteration method
Authority = t(A) * hub(initialize the hub with 1)
hub = A * authority

for (a in 1:N){
  
}

?eigen

# Page Rank