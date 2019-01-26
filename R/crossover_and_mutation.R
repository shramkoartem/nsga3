
#######################################################################################

##################     CROSSOVER AND MUTATION   #######################################

#######################################################################################



#CROSSOVER

crossover <- function(ind1, ind2){
  child <- vector(length=length(ind1))
  for(i in 1:length(child)){
    if(runif(1,0,1)>0.5){
      child[i] <- ind1[i]
    } else {
      child[i] <- ind2[i]
    }
  }
  return(child)
}


create_children <- function(mating_pool){
  children <- list()
  len <- length(mating_pool)
  #for(i in 1:(round(len/2,0))){
  for(i in 1:len){
    children[[i]] <- crossover(mating_pool[[i]],mating_pool[[(len-i+1)]])
  }
  return(children)
}


#MUTATION


mutate_ind <- function(ind, mutation_rate){
  for(i in 1:length(ind)){
    if(runif(1,0,1) < mutation_rate){
      ind[i] <- as.integer(!ind[i])
    }
  }
  if(sum(ind) == 0){
    i <- sample(length(ind))
    ind[i] <- 1
  }
  return(ind)
}

mutate_pop <- function(pop, mutation_rate=mutation_rate){
  mutated_pop <- mpop <- lapply(pop,mutate_ind,mutation_rate)
  return(mutated_pop)  
}

