# Networks in their surrounding context
# 28 Maret 2022

library("igraph")
g = graph.formula(A-B, A-C, A-D, A-E, B-E, C-D, C-F, D-F, F-G,
                  F-H, G-E, G-H, G-I, E-I, H-I, I-J, J-H)
par(mar = c(0, 0, 0, 0))
V(g)$label.cex = 0.7
V(g)$label = c("X", "X", "X", "X", "Y", "X", "X", "Y", "Y", "Y")
plot(g, layout = layout.fruchterman.reingold, vertex.size = 20)

# Measuring Homophily
## randomly permute the graph labels
labels = V(g)$label
adj_matrix = get.adjacency(g)
adj_matrix

# shuffling the graph labels
sample(labels)

# reassigning the shuffled labels into the graph (1000 times)
number_of_cross_edges = vector()

for (i in seq(1000)){
  permuted_labels=sample(labels) #vector of shuffled label
  class_x = which(permuted_labels=='X') # get the index of all the X class
  class_y = which(permuted_labels=='Y') # get the index of all the Y class
  # for this sample in next loop, these labels will have different index
  total_cross_edges=sum(adj_matrix[class_x, class_y]) #total cross edges
  number_of_cross_edges[i]=total_cross_edges #store total cross edges for permuted labels
}

# examine the distribution of cross edges
par(mar = c(0, 0, 0, 0))
hist(number_of_cross_edges, 10)

## record the number of cross type edges
### compare the permuted/ randomized cross edges with the given data
class_x = which(labels=='X')
class_y = which(labels=='Y')
original_cross_edges = sum(adj_matrix[class_x, class_y])

## repeat many times to build up a "no homophily" distribution,
## examine the probability that the given number of cross edges could appear randomly.
### graph as matrix to randomly permute the node labels. Permute the vectors of labels assigned to rows and columns.
## replicate: taking number of replicates wanted & a function to compute the replication.
### we want the number of cross type edges, so we have to write a function to permute the edges then compute the number cross type edges for the permutation.

# find out p that significantly low -> reject the null
p_val = mean(number_of_cross_edges < original_cross_edges)
print(paste('P value:', p_val))

# reject the null ==  homophily


# Schelling Model
## showing how clusters of communities can form even though they are not intended.
N = 10
grid = matrix(0, N, N) #this grid is filled by 0 as it is an empty places.

# Initializing the grid randomly
## Op is the number of O and Xp is the number of X.

# set O and X objects
Op = 20
Xp = 30
# there will be 50 objects in total. The grid size is 10 x 10 so 100 locations to allocate

# Allocate location to 50 objects
pos_o_x = sample(N*N, Op+Xp, replace=FALSE)

# Allocate to O objects
pos_o = pos_o_x[1:Op]

# Allocate to X objects
pos_x = pos_o_x[Op+1:Xp]

# Creating function to check neighbors
check_neighbors = function(row, col, grid, grid_length=N, type){
  # set starting row
  if(row==1){
    s_row=1
  }else{
    s_row=row-1
  }
  #set ending row
  if(row==grid_length){
    e_row=row
  }else{
    e_col=col+1
  }
  ## sum 9x9 grid - centre
  # scan starting to ending index for all those objects that we are looking for
  
  # substract current objects from scanned results (because current objects can't be its own neighbors)
  scan_results=((G[s_row:e_row, s_column:e_col]==type)-(G[row, col]==type))
  
  return(sum(scan_results)) #sum results to find total number of similar neighbors
}

for (i in seq(100)){
  for (t in c(1,2)){ #scan grid for object type 1=O, then for 2=X in above line t is for type
    for (r in seq(N)){ #loop for row
      for (c in seq(N)){ #loop for column
        if (grid[r, c]==t){ #perform neighbor checking only if the current position's object is matched with the type
          total_neighbor=check_neighbors(row=r, col=c, G=grid, grid_length=N, type=t)
          
          T=8 #number of neighbour to avoid rellocation
          if (total_neighbor<T){ #reallocate objects who has less than T neighbors
            #reallocate our object if the column and row is empty
            while(TRUE){
              rndom_row=sample(N, 1)
              rndom_col=sample(N, 1)
              if (grid[rndom_row, rndom_col]==0){ #checking wheter random row and col are empty
                #reallocating our object if the column and row is empty
                grid[rndom_row, rndom_col]=t
                grid[r, c]=0
                break #to stop while looping
              }
            }
          }
        }
      }
    }
  }
  image(grid, col=c('white', 'green', 'red')) #plot image of grid
  # white = 0, green = 1, red = 2
  Sys.sleep(0.1) #allowing R to draw image
}

