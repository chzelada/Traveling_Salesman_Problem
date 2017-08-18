source("functions.R")

lex_sol <- function(cities=4){
  cities_space <- generate_cities(cities) 
  cities_dist <- as.matrix(dist(cities_space[,2:3]))
  route <- c(1,2:cities,1)
  distance <- distance_route(route,cities_dist)
  out <- c(route,distance)
  n <- factorial(cities-1)-1
  for(i in 1:n){
    route[2:(cities)] <- lexicographic_order( route[2:(cities)] )
    distance <- distance_route(route,cities_dist)
    out <- rbind(out,c(route,distance), deparse.level = 0)
  }
  out <- as.data.frame(out)
  names(out)<-c(paste("city_",1:cities),"city_1","distance")
  return(out)
}


lex_graph_sol <- function(cities=4){
  cities_space <- generate_cities(cities) 
  cities_dist <- as.matrix(dist(cities_space[,2:3]))
  route <- c(1,2:cities,1)
  distance <- distance_route(route,cities_dist)
  out <- c(route,distance)
  n <- factorial(cities-1)-1
  best_route <- c(route,distance)
  poly_data <- cities_space[best_route[1:(cities+1)],2:3]
  plot(cities_space$pos_x,cities_space$pos_y)
  polygon(poly_data,border = "red",lwd=4)
  for(i in 1:n){
    route[2:(cities)] <- lexicographic_order( route[2:(cities)] )
    distance <- distance_route(route,cities_dist)
    plot(cities_space$pos_x,cities_space$pos_y)
    polygon(cities_space[route,2:3],border = "gray",lwd=1)
    out <- rbind(out,c(route,distance), deparse.level = 0)
    if(out[i+1,cities+2]<best_route[- (1:(cities+1)) ] ){
      best_route <-  out[i+1,]
    }
    polygon(cities_space[best_route[1:(cities+1)],2:3],border = "red",lwd=4)
    Sys.sleep(0.2)
  }
  out <- as.data.frame(out)
  names(out)<-c(paste("city_",1:cities),"city_1","distance")
  return(out)
}

sol <- lex_sol(8)
plot(1:nrow(sol),sol$distance,type = "l")


lex_graph_sol(5)
