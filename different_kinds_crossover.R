
## http://mat.uab.cat/~alseda/MasterOpt/GeneticOperations.pdf
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

## http://mat.uab.cat/~alseda/MasterOpt/GeneticOperations.pdf
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



## http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators/PMXCrossoverOperator.aspx/
pmx_crossover <- function(p1,p2){
  length_parent <- length(p1)
  index<-sample(2:(length_parent-1),size = 2,replace = FALSE)
  index <- sort(index)
  child1 <- p1
  keep_index<-index[1]:index[2]
  child1[-keep_index] <- 0
  (swath1 <- p1[keep_index])
  (swath2 <- p2[keep_index])
  elements <- setdiff(swath2,swath1)
  for(i in elements){
    print(i)
    cycle<-TRUE
    j<-i
    while(cycle){
      index2 <- which(p2==j)
      index1 <- which(p2==p1[index2])
      cycle<-index1 %in%keep_index
      if(!cycle){
        child1[index1]<-i
      } else {
        j<-p2[index1]
      }
    }
  }
  cindex<-which(child1==0)
  child1[cindex]<-p2[cindex]
  return(child1)
}

debug(pmx_crossover)
pmx_crossover(sample(1:10),sample(1:10) )
