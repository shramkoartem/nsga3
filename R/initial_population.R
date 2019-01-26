
#INITIAL POPULATION

generate_ind <- function(len, p){
  ind <- sample(0:1, size = len, replace = TRUE, prob = c(p,1-p))
  return(ind)
} 


generate_init_pop <- function(data, size){
  
  len <- ncol(data)-1
  population <- list()
  probs <- seq(0.1,0.9,length.out = size)
  
  for(i in 1:size){
    population[[i]] <- generate_ind(len,probs[i])
  }
  return(population)
}

#NON-DOMINATED SORTING

non_dom_sort <- function(pop, pareto_criteria){
  #works with data frames only
  sorted_pop <- psel(pop, pareto_criteria, top = nrow(pop), show_level = TRUE)
  return(sorted_pop)
}
