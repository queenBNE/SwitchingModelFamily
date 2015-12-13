require('igraph')

switching.randomise.triangle.adj = function(g, N, loops=F, multiple=F, return.all=F){
  if(is.directed(g))
    return(switching.randomise.dir.triangle.adj(g, N, loops, multiple, return.all))
  return(switching.randomise.undir.adj(g, N, loops, multiple, return.all))
}

switching.randomise.dir.triangle.adj = function(g, N, loops=F, multiple=F, return.all=F){
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
    
    # For directed networks without selfloops, we introduce the triangle reorientation move 
    # Try a triangle move: if y=u, x!=v AND (v,x) in g: try x --> y --> v --> x to x --> v --> y --> x
    if(!loops & y == u & g[v,x] > 0){ # enough to check (v,x) is edge since this implies v != x
      if(multiple | (g[x,v]==0 & g[v,y]==0 & g[y,x]==0)){
        edge.id = get.edge.ids(g, c(v,x))
        g <- delete.edges(g, c(edge.ids, edge.id))
        g <- add.edges(g, c(x,v,v,y,y,x))
      }
    }
    # Try a triangle move: if x=v, y!=u AND (y,u) in g: try u --> v --> y --> u to u --> y --> v --> u 
    if(!loops & x == v & g[y,u] > 0){ # enough to check (y,u) is edge since this implies y != u
      if(multiple | (g[u,y]==0 & g[y,v]==0 & g[v,u]==0)){
        edge.id = get.edge.ids(g, c(y,u))
        g <- delete.edges(g, c(edge.ids, edge.id))
        g <- add.edges(g, c(u,y,y,v,v,u))
      }
    }
    
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

switching.randomise.undir.adj = function(g, N, loops=F, multiple=F, return.all=F){
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