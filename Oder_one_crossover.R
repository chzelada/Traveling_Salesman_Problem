# order one crossover 
main <- function(){
  N=10
  p1<-sample(1:N)
  p2<-sample(1:N)
  
  # Empty child
  child <- rep(0,N)
  
  
  # step 1: fill genetic data from parent 1
  window <- sort(sample(1:N , 2))
  child[window[1]:window[2]]<-p1[window[1]:window[2]]
  if(window[2]==N){
    positions <- c(  1:N  )
  }else{
    positions <- c( (window[2]+1):N, 1:(window[2])  )
      }
  
  #print(window[1]:window[2])
  #print(positions)
  
  if(length(window[1]:window[2])==N){
    return(p1)
  }
  
  j=1
  for(i in positions ){
    if( !(p2[i] %in% p1[window[1]:window[2]] )  ){
      #print(c(positions[i],positions[j]) )
      child[ positions[j] ]<-p2[i]
      j <- j+1
    }
  }
  return(child)
}

debug(main)
for(i in 1:100){
  print(main())
}
main()
