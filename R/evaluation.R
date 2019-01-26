
#######################################################################################

##################     EVALUATION STEP       #########################################

#######################################################################################


####### Levels:
####### evaluate_population           - 1st
#######   evaluate_ind                - 2nd
#######       select_columns          - 3rd
#######       perform_prediction      - 3rd

evaluate_population <- function(pop, df, target, objectives, 
                                model. = model,
                                resampling. = resampling,
                                num_features. = num_features,
                                feature_cost. = feature_cost){


#############################################################################
  evaluate_ind <- function(ind, df, target, objectives, model, 
                           resampling, num_features, feature_cost){
  
  
    select_columns <- function(df, target, ind){
      
      goods <- data.frame(df[,c(target)])
      colnames(goods) <- target
      
      cnames <- colnames(df)
      cnames <- cnames[-which(cnames==target)]
      
      selected_columns <- cnames[as.logical(ind)]
      
      df <- df[,selected_columns]
      df <- cbind(df,goods)
      df <- mlr::createDummyFeatures(df, target = target, method = 'reference') 
      
      return(df)
    }
  
    
    perform_prediction <- function(df, target, model, resampling., remove_NA=FALSE){
      
      if(remove_NA==TRUE){
        df <- na.omit(df,cols=target)
      }
      #ndf <- mlr::normalizeFeatures(df, target = target)
      
      trainTask <- mlr::makeClassifTask(data = df, target = target, positive=1)
      
      learner <- model
      
      rdesc <- resampling.
      
      pred <- mlr::resample(learner, trainTask, rdesc, show.info = FALSE,
                            measures = list(mlr::mmce, mlr::fpr, mlr::fnr))
      res <- pred$pred
      return(res)
    }
    
  dat <- select_columns(df, target, ind)
  res <- perform_prediction(dat, target, model = model, 
                                resampling. = resampling.)
  
  get_objective_values <- function(a) {
    # call each function to a
    lapply(objectives, function(f) f(a))
  }
  
  ans <- get_objective_values(res)
  
  obj_vals <- data.frame()
  
  for(i in 1:length(ans)){
    obj_vals[1,i] <- ans[[i]]
  }
  
  if(num_features == TRUE){
    n <- length(obj_vals)+1
    obj_vals[1,n]<- sum(ind)
  }
  
  #if(feature_cost != FALSE){
    if(any(feature_cost)){
      cost <- sum(feature_cost[as.logical(ind)])
      n <- length(obj_vals)+1
      obj_vals[1,n]<- cost    
    }
 # }
  
  return(obj_vals)
}

  evaluated_pop_list <- parallelLapply(pop, evaluate_ind, df, target, objectives, model = model., 
                                       resampling = resampling., 
                                       num_features = num_features.,
                                       feature_cost = feature_cost.)
  
  evaluated_pop_res <- data.frame()
  
  evaluated_pop_res <- do.call(rbind, evaluated_pop_list)
  
  return(evaluated_pop_res)
}
################################################################## here

#  evaluated_pop <- data.frame()
  
#  for(i in 1:length(pop)){
#    ind <- pop[[i]]
#    evaluated_ind <- evaluate_ind(ind, df, target, objectives, model = model, 
#                                  resampling = resampling, 
#                                  num_features = num_features,
#                                  feature_cost = feature_cost)
#    rownames(evaluated_ind)<-i
    
#    evaluated_pop <- rbind(evaluated_pop, evaluated_ind)
#  }
#  return(evaluated_pop)
#}

