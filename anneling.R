source("functions.R")
## http://katrinaeg.com/simulated-annealing.html

initial_route <- function(n=200,cities_dist){
  dist<-data.frame()
  cities <- nrow(cities_dist)
  for(i in 1:n){
    ruta <- sample_route(1,nrow(cities_dist))
    dist<-rbind( dist, c(ruta, distance_route(ruta,cities_dist) )  )
  }
  names(dist)[ncol(dist)]<-"distance"
  x <- t(dist[dist$distance==min(dist$distance), ])[,1]
  x <- as.integer(x[1:(cities+1) ])
  return(x)
} 

rnd_neighbord <- function(vec){
  n <- length(vec)-1
  change <- sample(2:n, size = 2, replace = FALSE)
  temp <- vec[change[1]]
  vec[change[1]] <- vec[change[2]]
  vec[change[2]] <- temp
  return(vec)
} 



anneal <- function(cities=20,N=300, temp=10,   alpha = 0.9){
  temp_min = 0.0001

  cities_space <- generate_cities(cities) 
  cities_dist <- as.matrix(dist(cities_space[,2:3]))
  ruta <- initial_route(N,cities_dist)
  distancia <- distance_route(ruta,cities_dist)
  while(temp > temp_min){
    i <- 1
    while(i <= 100){
      new_route <- rnd_neighbord(ruta)
      new_distancia <- distance_route(new_route,cities_dist) 
      acceptance_probability <- exp( (distancia-new_distancia)/temp)
      if(acceptance_probability > runif(1) ){
        ruta <- new_route
        distancia <- new_distancia
      } else if(new_distancia<distancia) {
        distancia <- new_distancia
        ruta <- new_route
      }
      i <- i+1
    }
  temp <- temp*alpha  
  }
  plot(cities_space[,2:3])
  x<- ruta 
  polygon(cities_space[x,2:3],border="red",lwd=3)
return(c(ruta,distancia))
}

  



debug(anneal)
anneal(50,500,20,0.99)


