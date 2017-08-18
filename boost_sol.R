source("functions.R")




boostrap_sol <- function(cities=4,n=200){
  cities_space <- generate_cities(cities) 
  cities_dist <- as.matrix(dist(cities_space[,2:3]))
  dist<-data.frame()
  for(i in 1:n){
    Sys.sleep(0.05)
    ruta <- sample_route(1,nrow(cities_dist))
    dist<-rbind( dist, c(ruta, distance_route(ruta,cities_dist) )  )
    names(dist)[ncol(dist)]<-"distance"
    x <- t(dist[dist$distance==min(dist$distance), ])[,1]
    y <- t(dist[nrow(dist),])[,1]
    plot(cities_space$pos_x, cities_space$pos_y, main=min(dist$distance))
    polygon(cities_space[x[-length(x)],][,2:3],border = "red",lwd=4)
    polygon(cities_space[y[-length(y)],][,2:3],border = "gray",lwd=1)
  }
} 

boostrap_sol(12,100)
