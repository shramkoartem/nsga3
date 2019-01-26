
#######################################################################################

##################      NORMALISATION         #########################################

#######################################################################################





compute_ideal_point <- function(pareto_front){
  z_hat <- vector(length=ncol(pareto_front))
  for(i in 1:length(z_hat)){
    z_hat[i] <- min(pareto_front[,(i)])
  }
  return(z_hat)
}

translate_objectives <- function(pareto_front, ideal_point){
  for(i in 1:length(ideal_point)){
    for(j in 1:length(pareto_front[,i])){
      pareto_front[j,i] <- pareto_front[j,i] - ideal_point[i]
    }
  }
  return(pareto_front)
}

#different from paper. Just delivers the extrem points
#description of the achievement scalarizing function (ASF) in paper is not clear

get_extreme_points <- function(t_pareto_front){
  df <- t_pareto_front
  extreme_points <- vector("list",length=(length(t_pareto_front)))
  for(i in 1:length(extreme_points)){
    extreme_points[[i]] <- df[which.max(df[,i]),]
  }
  return(extreme_points)
}

#part-time sol, to be replaced

get_intercepts <- function(extreme_points){
  intercept <- vector(length=length(extreme_points))
  for(i in 1:length(intercept)){
    intercept[i] <- extreme_points[[i]][[i]]
  }
  return(intercept)
}


#a is already calculated from the f_prime, so no need to substract z_min
normalize <- function(f,a,z_min){
  if(a==0){
    f_n <- f/0.000001
  } else {
    f_n <- f/a
    return(f_n)
  }
}

normalize_objectives <- function(front, intercept, ideal_point){
  for(i in 1:ncol(front)){
    for(row in 1:nrow(front)){
      front[row,i] <- normalize(front[row,i], intercept[i], ideal_point[i])
    }
  }
  return(front)
}

