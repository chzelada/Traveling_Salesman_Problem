
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


##http://www.rubicite.com/Tutorials/GeneticAlgorithms/CrossoverOperators/CycleCrossoverOperator.aspx
cx_crossover <- function(p1=sample(1:10),p2=sample(1:10)){
  
  find_cycle <- function(p1=sample(1:10),p2=sample(1:10)){
    cycle <- c()
    i <- p1[1]
    cycle <- c(cycle,i)
    j <- p2[1]
    cycle <- c(cycle,j)
    while(j!=i){
      j <- p2[which(p1==j)]
      cycle <- c(cycle,j)
    }
    return(cycle[-length(cycle) ])
  }
  
  child1<-rep(NA,length(p1))
  child2<-rep(NA,length(p1))
  cycle<-find_cycle(p1,p2)
  g_idx_p1<-c()
  g_idx_p2<-c()
  
  
  idx_p1 <- sapply(cycle,function(x) which(p1==x) )
  g_idx_p1 <-idx_p1
  child2[idx_p1] <- p1[idx_p1]
  idx_p2 <- sapply(cycle,function(x) which(p2==x) )
  g_idx_p2 <- idx_p2
  child1[idx_p2] <- p2[idx_p2]
  change<-1
  
  while(sum(is.na(child1))!=0){
    p1_i<-p1[-g_idx_p1]
    p2_i<-p2[-g_idx_p2]
    cycle <- find_cycle(p1_i,p2_i)
    idx_p1 <- sapply(cycle,function(x) which(p1==x) )
    g_idx_p1 <-c(g_idx_p1,idx_p1)
    idx_p2 <- sapply(cycle,function(x) which(p2==x) )
    g_idx_p2 <-c(g_idx_p2,idx_p2)
    
    if(change%%2==1){
      child1[idx_p1] <- p1[idx_p1]
      child2[idx_p2] <- p2[idx_p2]
    } else {
      child2[idx_p1] <- p1[idx_p1]
      child1[idx_p2] <- p2[idx_p2]
    }
    change<-change+1
  }
  return(list(child1,child2))
}


find_edges <- function(p1=sample(1:10),p2=sample(1:10)){
  N<-list()
  for(num in 1:10){
    i <- which( p1==num)-1
    j <- which( p2==num)-1
    
    n1 <- p1[ (i+2)%%10 ]
    n2 <- p1[ (i)%%10 ]
    n3 <- p2[ (j+2)%%10  ]
    n4 <- p2[ (j)%%10 ]
    N[[num]] <- unique(c(n1,n2,n3,n4)) 
  } 
}
