

ox_crossover <- function(p1,p2){
  length_parent <- length(p1)
  index<-sample(2:(length_parent-1),size = 2,replace = FALSE)
  index <- sort(index)
  child1 <- p1
  keep_index<-index[1]:index[2]
  fill_values <- setdiff(p2,p1[keep_index]) 
  child1[-keep_index]<-fill_values
  child2 <- p2
  fill_values <- setdiff(p1,p2[keep_index]) 
  child2[-keep_index]<-fill_values
  return(list(child1,child2))
}

pos_crossover <- function(p1,p2,n=3){
  length_parent <- length(p1)
  index<-sample(1:length_parent, size = n,replace = FALSE)
  child1 <- p1
  fill_values <- setdiff(p2,p1[index]) 
  child1[-index]<-fill_values
  child2 <- p2
  fill_values <- setdiff(p1,p2[index]) 
  child2[-index]<-fill_values
  return(list(child1,child2))
}




pmx_crossover <- function(p1,p2){
  length_parent <- length(p1)
  index<-sample(2:(length_parent-1),size = 2,replace = FALSE)
  index <- sort(index)
  child1 <- p1
  keep_index<-index[1]:index[2]
  swath_p1 <- p2[keep_index]
}

debug(pmx_crossover)
pmx_crossover(sample(1:10),sample(1:10) )
