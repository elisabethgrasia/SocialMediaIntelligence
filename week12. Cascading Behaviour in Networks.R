# Cascading Behaviour in Networks
# 301116 Social Media Intelligence
# 30 May 2022

library(igraph)
library(Matrix)

# Cascading Behaviour
gi = graph.formula(A - B, A - C, B - C, A - D, D - E, D - F, E - F)
g = barabasi.game(10, power = 1.5, m = 2, directed = FALSE, start.graph = gi)
plot(g)
state = rep(0,10)

state[c(3,6)] = 1
q = 0.4
plot(g, vertex.label = state)

while(TRUE) {
  nodes_zero = which(state==0)

for (i in nodes_zero) {
  nb = neighbors(g, i)
  s1 = vector()
  for (j in nb){
    s1 = append(s1, which(state[j]==1))
  }
  
  p = length(s1)/ length(nb)
  
  if (p>q){
    state[i] = 1
  }
}

Sys.sleep(0.6)
plot(g, vertex.label = state)

}
