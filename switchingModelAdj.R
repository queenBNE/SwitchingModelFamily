require('igraph')

switching.randomise.adj = function(g, N, loops=F, multiple=F, return.all=F, debug=F){
  if(is.directed(g))
    return(switching.randomise.dir.adj(g, N, loops, multiple, return.all, debug))
  return(switching.randomise.undir.adj(g, N, loops, multiple, return.all, debug))
}

switching.randomise.dir.adj = function(g, N, loops=F, multiple=F, return.all=F, debug=F){
  if(debug)
    print("Randomising directed network")
  all <- list()
  for(i in 1:N){
    # Select edges to try a switch with
    edge.ids = sample(1:ecount(g), 2, F)
    edge1 = ends(g, edge.ids[1])
    edge2 = ends(g, edge.ids[2])
    x = edge1[1]
    y = edge1[2]
    u = edge2[1]
    v = edge2[2]
    
    # Try a switch (x,y) & (u,v) --> (x,v) & (u,y)
    if(loops | (x != v & u != y))
      if(multiple | (g[x,v] == 0 & g[u,y] == 0)){
        # Acceptance probability: this is equal to 1 if the network does not contain multiple edges 
        p.acceptance <- 1 / (g[x,y]*g[u,v])
        if(runif(1) <= p.acceptance){
          g <- delete.edges(g, edge.ids)
          g <- add.edges(g, c(x,v,u,y))
        }
      }
    if(return.all)
      all[[i]] <- g
  }
  if(return.all)
    return(all)
  return(g)
}

switching.randomise.undir.adj = function(g, N, loops=F, multiple=F, return.all=F, debug=F){
  if(debug)
    print("Randomising undirected network")
  all <- list()
  for(i in 1:N){
    # Select edges to try a switch with
    edge.ids = sample(1:ecount(g), 2, F)
    edge1 = ends(g, edge.ids[1])
    edge2 = ends(g, edge.ids[2])
    x = edge1[1]
    y = edge1[2]
    u = edge2[1]
    v = edge2[2]
    
    # Try either switch 1 or switch 2 
    if(runif(1) > 0.5){
      #Switch 1: {x,y} & {u,v} --> {x,v} & {u,y}
      if(loops | (x != v & u != y))
        if(multiple | ((g[x,v] == 0 & g[u,y] == 0) & (x != y | u != v)) ){
          p.acceptance = 1
          if(x==y)
            p.acceptance = 0.5 * p.acceptance
          if(u==v)
            p.acceptance = 0.5 * p.acceptance
          p.acceptance = p.acceptance * 1/(g[x,y]*g[u,v])
          if(runif(1) <= p.acceptance){  
            g <- delete.edges(g, edge.ids)
            g <- add.edges(g, c(x,v,u,y))
          }
        }
          
    }else{
      #Switch 2: {x,y} & {u,v} --> {x,u} & {y,v}
      if(loops | (x != u & y != v))
        if(multiple | ((g[x,u] == 0 & g[y,v] == 0) & (x != y | u != v))){
          p.acceptance = 1
          if(x==y)
            p.acceptance = 0.5 * p.acceptance
          if(u==v)
            p.acceptance = 0.5 * p.acceptance
          p.acceptance = p.acceptance * 1/(g[x,y]*g[u,v])
          if(runif(1) <= p.acceptance){  
            g <- delete.edges(g, edge.ids)
            g <- add.edges(g, c(x,u,y,v))
          }
        }
    }
    if(return.all)
      all[[i]] <- g
  }
  if(return.all)
    return(all)
  return(g)
}