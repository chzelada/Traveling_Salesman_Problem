
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
  print(vec)
  return(vec)
}
undebug(lexicographic_order)


lexicographic_order(c(1,3,2,4))
lexicographic_order(c(1,3,4,2))
lexicographic_order(c(1,4,2,3))
lexicographic_order(c(1,4,3,2))
lexicographic_order(c(2,1,3,4))
lexicographic_order(c(2,1,4,3))
lexicographic_order(c(2,3,1,4))


lexicographic_order(c(1,3,2,4))

test <- 0:5
print(test)
n <-factorial(length(test))-1
for(i in 1:n){
  Sys.sleep(0.01)
  test<-lexicographic_order(test)
}


test<- lexicographic_orderV2(test)
test
