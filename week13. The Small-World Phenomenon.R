# The Small-World Phenomenon
# 301116 Social Media Intelligence
# 30 May 2022

N = 10 # width of grid

# create empty adjaceny matrix
A = sparseMatrix(i = 1, j = 1, x = 0, dims = c(N^2,N^2))
for (i in 1:N) {
  for (j in 1:N) {
    ## add local connections
    B = matrix(0, N, N)
    if (j > 1) B[i, j-1] = 1
    if (j < N) B[i, j+1] = 1
    if (i > 1) B[i-1, j] = 1
    if (i < N) B[i+1, j] = 1
    ## flatten matrix
    a = c(B)
    ## change to a sparse matrix and add to adjacency matrix
    A[(i - 1)*N + j,] = drop0(a)
  }
}
g_local = graph.adjacency(A, mode = "undirected")

B = A

for (i in seq(nrow(B))){
  nodes = seq(nrow(B))
  nodes = nodes[-i]
  rndm_node = sample(nodes, size = 1)
  B[i, rndm_node]=1
  B[rndm_node, i]=1
}

g = graph.adjacency(B, mode = "undirected")

p = c(shortest.paths(g))
hist(p)
summary(p)

B = A
for (i in seq(nrow(B))){
  nodes = seq(nrow(B))
  nodes = nodes[-i]
  rndm_node = sample(nodes, size = 1)
  B[i, rndm_node]=1
  B[rndm_node, i]=1
}

g = graph.adjacency(B, mode = "undirected")
p = c(shortest.paths(g))
hist(p)
summary(p)

# Watts-Strogatz model with Clustering Coefficient
C = A
dist = shortest.paths(g_local)
q = 3

for (i in seq(nrow(C))){
  nodes = seq(nrow(C))
  nodes = nodes[-i]
  node_prob=1/dist[i,-i]^q
  rndm_node = sample(nodes, size = 1, prob = node_prob)
  C[i, rndm_node]=1
  C[rndm_node, i]=1
}


g = graph.adjacency(C, mode = "undirected")
p = c(shortest.paths(g))
hist(p)
summary(p)
