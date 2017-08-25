
get_population <- function(N=500, vec=2:8){
  first <- c(1,sample(vec),1)
  population <- matrix(first,ncol = length(first)  )
  for(i in 1:(N-1)){
    population <- rbind( population, c(1,sample(vec),1) )
  }
  unique_population <- unique(population)
  return(unique_population)
}

distance_route <- function(route,distance){
  sum_distance<-0
  for(i in 1:length(route[-1])){
    out <- distance[ route[i], route[i+1]  ]
    sum_distance <- sum_distance + out
  }
  return(sum_distance)
}

generate_cities <- function(cities =5){
  pos_x <- sample(1:100, size = cities , replace=TRUE) 
  pos_y <- sample(1:100, size = cities , replace=TRUE)
  out <- data.frame(city = 1:cities, pos_x, pos_y )
  return(out)
}


cities <- generate_cities(10)
cities_dist <- as.matrix(dist(cities[,2:3]))
pop <- get_population(N = 100, vec =2:10)

fitness<-apply(pop , 1, distance_route, distance = cities_dist)
pop <- cbind(pop,fitness)




prob_pick<-(1-fitness/max(fitness))/sum(1-fitness/max(fitness))
pop <- cbind(pop,prob_pick)

index <- sample(1:nrow(pop), size = 2, prob = prob_pick)
 pop[index,1:12]
 
 
 
 rejection_sampling <- function (fitness){
   N = length(fitness)
   stop <- TRUE
   while(stop){
     out <- sample(1:N,size = 1)
     rango <- round(min(fitness)-1):max(fitness+1)
     if( fitness[out] <= sample(rango,size=1) ){
       stop <- FALSE
       return(out)
     } 
   }
 }
 
out<-c() 
for(i in 1:10000){
  out<-c(out,rejection_sampling(fitness))
}
 

out2<- c()
for(i in 1:10000){
  out2<-c(out2,sample(1:nrow(pop), size = 1, prob = prob_pick))
}


par(mfrow=c(1,2))
hist(out)
hist(out2)


sample(2:7)
sample(2:7, size = 1)



rejection_sampling(fitness)


