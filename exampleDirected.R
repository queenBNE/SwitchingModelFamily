# Example randomising directed traingle 1->2->3->1 
# There are 2 states in the stategraph 
g <- graph.empty(3)
V(g)$color = c("red", "green", "blue")
g <- add.edges(g, c(1,2,2,3,3,1))
N <- 1000

# The first should only contain the original network, the second both orientations
sample <- switching.randomise.dir(g,N, return.all = T)
sample.tri <- switching.randomise.dir.triangle.adj(g, N, return.all = T)

# plot samples 
par(mfrow = c(2,3))
for(h in sample)
  plot(h, vertex.label="")
for(h in sample.tri)
  plot(h, vertex.label="")


# Example randomising directed network: 1 <-> 2 -> 3 <- 4
# There are 7 states in the stategraph
g <- graph.empty(4)
g <- add.edges(g, c(1,2,2,1,2,3,4,3))
sample <- switching.randomise.dir(g,N, T, T, T)
sample.adj <- switching.randomise.dir.adj(g,N, T, T, T)

# We count the occurence of each of the 7 states in both samples
ensemble <- list(get.adjacency(g))
states <- 7
sample.distr <- mat.or.vec(1, states)
sample.distr.adj <- mat.or.vec(1, states)
i = 2
for(h in sample){
  exists = F
  h.adj = get.adjacency(h)
  for(j in 1:length(ensemble))
    if(prod(ensemble[[j]] == h.adj) == 1){
      exists = T 
      sample.distr[j] = sample.distr[j] + 1
    }
  
  if(!exists){
    ensemble[[i]] <- h.adj
    sample.distr[i] = 1
    i <- i + 1
  }
}

for(h in sample.adj){
  exists = F
  h.adj = get.adjacency(h)
  for(j in 1:length(ensemble))
    if(prod(ensemble[[j]] == h.adj) == 1){
      exists = T 
      sample.distr.adj[j] = sample.distr.adj[j] + 1
    }
  
  if(!exists){
    ensemble[[i]] <- h.adj
    sample.distr.adj[i] = 1
    i <- i + 1
  }
}

# We plot the different distributions
plot(1:states, sample.distr, type="h", lwd=3, ylim = c(0,N/5))
points(1:states+0.1, sample.distr.adj, type="h", lwd=3, col=2)
abline(h=N/states)

plot(1:states, sample.distr.adj, type="h", lwd=3, ylim = c(0,N/5), col=2)
abline(h=N/states)
