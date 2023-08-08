# Power Laws and Rich-Get-Richer Phenomena
# week 11 lab
# 301116  - Social Media Intelligence
# Elisabeth Putri

library(igraph)
library(Matrix)

# Generating a random graph
# generate a nxn adjacency matrix filled with zeros
n = 20
p = 0.4
A = matrix(0, n, n)

# for each pair of nodes i and j, create an edge with probability p
for (i in seq(n)){ #for row
  for (j in seq(n)){ # for column
    choice =sample(c(1, 0), size = 1, prob = c(p, 1-p))
    A[i, j]=choice
    A[j, i]=choice
    }
  }

A

# if the edge from i to j was created, add the edge j to i(to create an undirected graph)
# finally convert the adjacency matrix to a graph
adjA = graph.adjacency(A, mode = "undirected")
plot(A)

dd_g1 = degree.distribution(g1)
hist(dd_g1)
mean(dd_g1)

## generating erdos-renyi graph using R function
g.er = erdos.renyi.game(n = 20, p = 0.4)
dd_g.er = degree.distribution(g.er)
hist(dd_g.er)





# Random Power Law Graph
# generate a nxn adjacency matrix filled with zeros.
n = 20
p = 0.4
A = matrix(0, n, n)

# the first node does not link.
A[2, 1] = 1
for (i in seq(3, n)){#for row
  choice=sample(c(1, 0), size=1, prob=c(p, 1-p))
  if (choice==1){ # linking to a random node p
    linkingto = sample(c(1:i-1), size=1)
  } else { # copying a random edge 1-p
    inlinks = apply(A, 1, sum)
    linkingto=sample(seq(1, i-1), size = 1, prob=inlinks[1:i-1])
  }
  A[i, linkingto]=1
}

g2=graph.adjacency(A)
dd_g2=degree.distribution(g2)
hist(dd_g2)
mean(dd_g2)

# the second node links to the first.

# the j-th links to one of 1 to j-1 with probability p and copies an edge with prob 1-p

# finally convert the adjacency matrix to graph

## generating a random Barabasi-Albert graph using R function
k=2  #k is the power law coefficient computed
g.ba = barabasi.game(n = 20, power = k)
dd_g.ba=degree.distribution(g.ba)
hist(dd_g.ba)
mean(dd_g.ba)

