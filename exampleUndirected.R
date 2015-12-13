# Example randomising undirected chain 1--2--3--4--5 
# There are 23 states in the stategraph (jacobiencarstens.com/stategraph.html)
g <- graph.empty(5, directed=F)
g <- add.edges(g, c(1,2,2,3,3,4,4,5))
N <- 5000

# We sample with the adjusted and non-adjusted chain 
sample <- switching.randomise(g, N, T, T, T)
sample.adj <- switching.randomise.adj(g, N, T, T, T)

# We count the occurence of each of the 23 states in both samples
ensemble <- list(get.adjacency(g))
sample.distr <- mat.or.vec(1, 23)
sample.distr.adj <- mat.or.vec(1, 23)
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
plot(1:23, sample.distr, type="h", lwd=3, ylim = c(0,N/10))
points(1:23+0.3, sample.distr.adj, type="h", lwd=3, col=2)
abline(h=N/23)

plot(1:23, sample.distr.adj, type="h", lwd=3, ylim = c(0,N/10), col=2)
abline(h=N/23)