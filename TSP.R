

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



cities_space <- generate_cities(cities = 15) 
cities_dist <- as.matrix(dist(cities_space[,2:3]))
dist<-data.frame()


for(i in 1:150){
  Sys.sleep(0.09)
  ruta <- sample_route(1,nrow(cities_dist))
  dist<-rbind( dist, c(ruta, distance_route(ruta,cities_dist) )  )
  names(dist)[ncol(dist)]<-"distance"
  x <- t(dist[dist$distance==min(dist$distance), ])[,1]
  y <- t(dist[nrow(dist),])[,1]
  plot(cities_space$pos_x, cities_space$pos_y, main=min(dist$distance))
  polygon(cities_space[x[-length(x)],][,2:3],border = "red",lwd=4)
  polygon(cities_space[y[-length(y)],][,2:3],border = "gray",lwd=1)
}
names(dist)[ncol(dist)]<-"distance"















