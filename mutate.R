# swap mutate

swap_mutate <- function(vec=sample(1:10)){
  N <- length(vec)
  genes <- sample(1:N,replace=FALSE, size =2)
  temp <- vec[genes[1]]
  vec[genes[1]]<- vec[genes[2]]
  vec[genes[2]]<- temp
  return(vec)
}

# Insert Mutation

insert_mutate <- function(vec=sample(1:10)){
  N <- length(vec)
  genes <- sort(sample(1:N,replace=FALSE, size =2))
  x <- (genes[1]+1):N
  x <- x[x!=genes[2]]
  out <- c(1:genes[1],genes[2], x)
  print(vec)
  print(out)
  return(vec[out])
}

# Inversion Mutation

inversion_mutation <- function(vec=sample(1:10)){
  N <- length(vec)
  genes <- sort(sample(1:N,replace=FALSE, size =2))
  x<-1:N
 x[genes[1]:genes[2]] <- genes[2]:genes[1]
 return(vec[x])
}
debug(inversion_mutation)
inversion_mutation(1:10)






