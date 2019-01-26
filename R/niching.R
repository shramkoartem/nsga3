
#######################################################################################

##################            NICHING         #########################################

#######################################################################################



find_ref_point <- function(point, rp){
  res <- matrix(ncol=ncol(point)+1, nrow=nrow(rp))
  
  for(i in 1:nrow(rp)){
    d <- dist(rbind(point,rp[i,]))
    res[i,1:(ncol(res)-1)] <- rp[i,]
    res[i,ncol(res)] <- d
  }
  ref <- rp[which.min(res[,ncol(res)]),]
  ans <- which.min(res[,ncol(res)])
  
  return(ans)
}

gen_refs <- function(data, rp){
  ref <- data.frame()
  for(point in 1:nrow(data)){
    p_ref <- find_ref_point(data[point,], rp)
    ref <- rbind(ref,c(p_ref,point))
  }
  colnames(ref) <- c("rp", "data")
  return(ref)
}

sel_points <- function(ref_list, dat, k){
  ref_list <- data.frame(ref_list)
  u <- unique(ref_list[,1])
  r <- data.frame()
  
  for(i in u){
    c <- length(ref_list[ref_list$rp==i,1])
    x <- c(i,c)
    r <- rbind(r,x)
  } 
  colnames(r) <- c("rp", "count")
  r <- r[order(r$count),]
  
  points <- c()
  while(length(points)< k){
    for(i in 1:nrow(r)){
      val <- r[i,1]
      point <- ref_list[ref_list$rp==val,][,2]
      if(length(point)>1){
        point <- sample(point,1)
      }
      if(point %in% points){next}
      else{
        points[length(points)+1] <- point 
      }
    }
  }
  res <- dat[points,]
  return(res)
}

execute_selection <- function(pf, k){
  m <- ncol(pf)
  
  ip <- compute_ideal_point(pf) #ip = ideal point
  tpf <- translate_objectives(pf, ip) #tpf = translated pareto front
  ep <- get_extreme_points(tpf) #ep = exteme points
  inter <- get_intercepts(ep) #intercept
  npf <- normalize_objectives(tpf, inter, ip) #normalized pareto front
  #colnames(npf) <- c("x", "y", "z")
  
  rp <- ref_points(m)
  
  
  
  
  selected_points <- gen_refs(npf, rp)
  selected_points <- sel_points(selected_points, npf,k)

  return(selected_points)
}


