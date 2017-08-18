generate_cities <- function(cities =5){
  pos_x <- sample(1:100, size = cities , replace=TRUE) 
  pos_y <- sample(1:100, size = cities , replace=TRUE)
  out <- data.frame(city = 1:cities, pos_x, pos_y )
  return(out)
}


distance_route <- function(route,distance){
  sum_distance<-0
  for(i in 1:length(route[-1])){
    out <- distance[ route[i], route[i+1]  ]
    sum_distance <- sum_distance + out
  }
  return(sum_distance)
}


sample_route <- function(initial_node=1, nodes=4){
  nodes<-1:nodes
  sample_nodes <- nodes[-initial_node]
  attach_nodes <- sample(sample_nodes)
  out<- c(initial_node,attach_nodes,initial_node)
  return(out)
}

swap <- function(vec,i,j){
  out <- vec
  out[i] <- vec[j]
  out[j] <- vec[i]
  return(out)
}

lexicographic_order <- function(vec){
  n <- length(vec)-1
  i <- 0
  for(pos in 1:n){
    if(vec[pos]<vec[pos+1]){
      i <- pos
    }
  }
  if(i != 0){
    j<-max(which(vec > vec[i]))
    vec <- swap(vec,i,j)
    vec <- c(vec[1:i],vec[length(vec):(i+1)] )
  }
  return(vec)
}
