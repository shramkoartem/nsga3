# nsga3
## NSGA III for Multi-Objective Feature Selection


An adaptation of Non-dominated Sorting Genetic Algorithm III for multi
objective feature selection tasks in R programming language.
Non-dominated Sorting Genetic Algorithm III is a genetic algorithm that solves multiple
optimization problems simultaneously by applying a non-dominated sorting
technique. It uses a reference points based selection operator to explore
solution space and preserve diversity. See the paper by K. Deb and
H. Jain (2014) <DOI:10.1109/TEVC.2013.2281534> for a detailed description of the algorithm.

https://cran.r-project.org/web/packages/nsga3/nsga3.pdf


##Update
This is a 0.0.4 version, which is ahead of the 0.0.3 CRAN version by:
* added a backup option
* adjusted classifier output dependency 

##Description

This adaptation of NSGA III algorithm for Multi Objective Feature Selection is currently
available only for classification tasks.

As any other Genetic Algorithm (GA), NSGA III includes following steps:
* An initial population Pt of a size N is created
* A model is trained on each individual (subset) and fitness values are assigned
* An offsping population of a size N is created by crossover and mutation operators
* The offspring population is combined with its parent population
* A combined population of a size 2N is split into Pareto Fronts using non-dominated sorting technique
* A next generation's population Pt+1 of size N is selected from the top Pareto Fronts with help of elitism based selection operator

The loop is repeated until the final generation is reached
Each generation is populated by individuals representing different subsets.
Each individual is represented as a binary vector, where each gene represents a feature in the original dataset.

## Example
```R
xgb_learner <- mlr::makeLearner("classif.xgboost", predict.type = "prob",
                             par.vals = list(
                             objective = "binary:logistic",
                             eval_metric = "error",nrounds = 2))

rsmp <- mlr::makeResampleDesc("CV", iters = 2)
measures <- list(mlr::mmce)

f_auc <- function(pred){auc <- mlr::performance(pred, auc)
                         return(as.numeric(auc))}
objective <- c(f_auc)
o_names <- c("AUC", "nf")
par <- rPref::high(AUC)*rPref::low(nf)

nsga3fs(df = german_credit, target = "BAD", obj_list = objective,
         obj_names = o_names, pareto = par, pop_size = 1, max_gen = 1,
         model = xgb_learner, resampling = rsmp,
         num_features = TRUE, r_measures = measures, cpus = 2)
```

keywords: NSGA III, NSGA-III, NSGA 3, nsga 3, nsga iii, nsga-iii
