
#######################################################################################

##################   REFERENCE POINTS         #########################################

#######################################################################################




ref_points <- function(n_objectives){
  m <- n_objectives
  p <- m+1
  
  if( m==2){
    a <- seq(0,0.5,0.05)
    b <- seq(0.5,0,-0.05)
    refs <- cbind(a,b)
    return(refs)
  }
  
  #N = number of points
  a <- m+p-1
  n <- factorial(a)/(factorial(p)*factorial(a-p))
  
  #divisions per side
  d <- n/m-1
  step <- 1/d
  
  options <- c(0,1)
  for(j in 1:d){
    options[(2+j)]<- step*j
  }
  
  point <- vector(length=m)
  point[1]<-1
  for(i in 2:m){
    point[i]<-0
  }
  set <- rbind(point)
  
  while(nrow(unique(set))<n){
    point <- vector(length=m)
    for(col in 1:m){
      point[col] <- sample(options,1)
    }
    if(sum(point)==1){
      set <- rbind(set,point)
    }
  }
  set <- unique(set)
  return(set)
}

