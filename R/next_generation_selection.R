
#iterator for selecting points from current generation
select_next_generation <- function(sorted_evaluated_comb_pop, 
                                   combined_pop_individuals, rp, n){
  next_pop = c()
  
  lvl <- 1
  while(length(next_pop) != n){
    
    pf <- sorted_evaluated_comb_pop[which(sorted_evaluated_comb_pop$.level==lvl),]
    pf <- pf[,-ncol(pf)]
    
    len <- length(next_pop)
    
    if((nrow(pf)+len) <= n){
      
      for(i in 1:nrow(pf)){
        next_pop[len+i] <- rownames(pf)[i]
      }
      
      lvl <- lvl+1
      
    } else {
      k <- n-len
      selected_points <- execute_selection(pf, k)
      
      for(i in 1:k){
        next_pop[len+i] <- rownames(selected_points)[i]
      }
    }
  }
  
  next_gen = list()
  eval_next_gen = data.frame()
  for(i in 1:length(next_pop)){
    id <- next_pop[i]
    eval_next_gen <- rbind(eval_next_gen, sorted_evaluated_comb_pop[id,-ncol(sorted_evaluated_comb_pop)])
  }
  ids <- rownames(eval_next_gen)
  ids <- sapply(ids, as.integer)
  next_gen <- combined_pop_individuals[ids]
  
  ans <- list(next_gen, eval_next_gen)
  return(ans)
}

