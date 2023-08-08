library("igraph")
setwd("//unix.scem.westernsydney.edu.au/homes/Downloads")

g = read.graph(file="telephoneCalls.net", format="pajek")

# run the following lines if you want to see how it looks like
V(g)$label.cex = 0.5
plot(g, layout=layout.fruchterman.reingold, vertex.size=10)

print(g)

# Neighbourhood overlap vs edge strength
## Compute the neighbourhood overlap for the telephone calls graph 
## and plot it against edge strength for all edges. (Use for loops)


### Step 1. find end nodes of all the edges
g_ends= ends(g, E(g)) ## ends = incident vertices of some graph edges; E means parsing the graphs into all the g
E(g)
g_ends[i, 1]
ne.over(g_ends[i, 1], g_ends[i, 2], g)

### Step2. Create a function to calculate neighborhood overlap
no = function(node1, node2, g_) {
  cmn = intersection(neighbors(g_, node1), neighbors(g_, node2))
  uni = union(neighbors(g_, node1), neighbors(g_, node2))
  ans = length(cmn)/ (length(uni)-2) # here we are removing 2 nodes those are our node1 & node 2
  return(ans)
}

### Step 3. Calculate the Neighbourhood Overlap
#### Create a list to store the Neighbourhood Overlap
no_list = list()

for (i in seq(nrow(g_ends))){
  # a for loop that will go through the list of end nodes
  
  # extract node 1 and 2 from the list of "pair of end nodes"
  n1 = g_ends[i, 1]
  n2 = g_ends[i, 2]
  
  # calculate neighborhood overlap for all pairs
  no_calc = no(node1 = n1, node2 = n2, g)
  
  # add neighborhood overlap to the list
  no_list[i] = no_calc
}

print(no_list)

### Step 4. Plot edge strength against Neighborhood Overlap
plot(E(g)$weight, no_list)
E(g)$weight
  
# Edge betweenness
## Plot the relationship about how edge strength is related to edge betweennness.

### Step 1. Get edge betweenness for all edges of graph 'g'
e_bet = edge_betweenness(g, E(g))
print(e_bet)

# betweenness(g) # betweenness : The vertex and edge betweenness are (roughly) 
# defined by the number of geodesics (shortest paths) going through a vertex or an edge

# Step 2. Get the weight of edge shows its strength
e_strength = E(g)$weight
print(e_strength)

print(E(g))
?E
# Step 3. Plot the graph
plot(e_strength, e_bet)

### edge_betweenness

# Graph Partitioning
## Extract the largest component of the graph and partition this component

### Step 1. Split the graph into components
g_comp = decompose(g)

### Step 2. Count the number of nodes in each component
n_nodes = sapply(g_comp, function(x) {length(V(x))})

### Step 3. Select the largest component for partitioning
lc_index = which(n_nodes == max(n_nodes)) #by doing this we are finding an index of the largest component

### Step 4. Get the edges of the largest component
lc_edges = g_comp[[lc_index]]

### Step 5. Perform Partitioning

# Note: this might take a few minutes to run. There might be warnings about scores. 
g_part = cluster_edge_betweenness(lc_edges, directed = FALSE)

print(g_part$membership) # see all the partitions
table(g_part$membership) # run this if you want to see how many edges are there in the partition

### Step 6. Visualisation

### From the table, we can see that the largest component is partition number 1. Therefore, 
partition_no = 1
partition_index = which(g_part$membership == partition_no)

### Extract  the subgraph containing only nodes from chosen partition.
sub_g = subgraph(g_comp[[lc_index]], partition_index)
plot(sub_g, vertex.size=5)

### Examine the dendrogram of the partitioning 
plot(as.dendrogram(g_part)) # It is useful to study hierarchical relationship
