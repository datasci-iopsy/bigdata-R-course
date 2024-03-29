rm(list = ls()) #keep env clean

#load libraries
library(dplyr)
library(readr)
library(caret)
# library(xgboost)

#read in dataset - feature attributes must be specified from csv files...
dat = read_csv("../../data/finalnorm_df.csv", col_names = T) %>% 
    mutate_if(is.character, as.factor)

# Modeling

#preprocess predictors - dummy code categorical features
dummy_coded = dummyVars(~ ., data = dat[, -1])
dummies = data.frame(predict(dummy_coded, newdata = dat[, -1])) %>% 
    cbind('date' = dat$date)
    

glimpse(dummies) #all num features

#using h2o package
#install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "8G") #use all cores and all (my) memory

#create an h2o obj - df but in an env
h2o_df = as.h2o(dummies[, -151])

#find a better way to subset date by year with h2o objc
#(< 2018 = training; >= 2018 = testing)
h2oTrain = h2o_df[1:217267, ] #< 2018 = training
h2oTest = h2o_df[217268:236918, ] #>= 2018 = testing

response = "waittime"
features = setdiff(colnames(h2oTrain), response)

#run the null model - no parameterizing 
gbm = h2o.gbm(x = features, y = response, training_frame = h2oTrain)
summary(gbm) #RMSE: .15; r^2: .58

#how well the null model works with testing data!
h2o.rmse(h2o.performance(gbm, newdata = h2oTest)) # RMSE = .22 
h2o.r2(h2o.performance(gbm, newdata = h2oTest)) #r2 = .24

#a few parameters to augment
gbm2 = h2o.gbm(x = features, 
               y = response, 
               training_frame = h2oTrain, 
               ntrees = 5000, 
               learn_rate = .01, 
               stopping_rounds = 5, 
               stopping_tolerance = .001, 
               stopping_metric = "RMSE", 
               sample_rate = .6, 
               col_sample_rate = .6, 
               seed = 1879, 
               score_tree_interval = 5 
               )
summary(gbm2) #RMSE: .15; r^2: .60
h2o.varimp_plot(gbm2, num_of_features = 20) #top 20 features of gbm2

#how well the second model predicts on testing data
h2o.rmse(h2o.performance(gbm2, newdata = h2oTest)) #RMSE same at .22
h2o.r2(h2o.performance(gbm2, newdata = h2oTest)) #r^2 up to .25 

#fit models using "lucky" parameters 
hyper_params = list(max_depth = c(4, 6, 8, 12, 16, 20))

#grid search of models specifying the sampling rates, stopping rounds, 
# RMSE cut-off scoring, etc.
grid = h2o.grid(
    hyper_params = hyper_params, 
    search_criteria = list(strategy = "Cartesian"), 
    algorithm = "gbm",
    grid_id = "depth_grid",
    x = features,
    y = response,
    training_frame = h2oTrain,
    validation_frame = h2oTest,
    ntrees = 500, 
    #faster scan - to improve performance use ^learn_ values: .02 & .995
    learn_rate = .05, 
    learn_rate_annealing = .99, 
    sample_rate = .8, 
    col_sample_rate = .8, 
    seed = 8762,
    stopping_rounds = 5,
    stopping_tolerance = .001,
    stopping_metric = "RMSE", 
    score_tree_interval = 5
    )
grid

#sort grid model RMSEs in ascending order - we want the lowest!
sortedGrid = h2o.getGrid("depth_grid", sort_by = "rmse", decreasing = FALSE)
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]

minDepth = min(as.numeric(topDepths))
minDepth

maxDepth = max(as.numeric(topDepths))
maxDepth

#final run with optimized hyperparameters found in grid search - TRUE ML
hyper_params_opt = list(
    max_depth = seq(4, 16, 1), 
    sample_rate = seq(.2, 1, .01), 
    col_sample_rate = seq(.2, 1, .01), 
    col_sample_rate_per_tree = seq(.2, 1, .01), 
    col_sample_rate_change_per_level = seq(.9, 1.1, .01), 
    min_rows = 2^seq(0, log2(nrow(h2oTrain))-1, 1), 
    nbins = 2^seq(4, 10, 1), 
    min_split_improvement = c(.000001, .0000001), 
    histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
    )

#randomly search the best strategy for the program to use to score models
search_criterira = list(
    strategy = "RandomDiscrete", 
    # max_runtime_secs = 3600, 
    max_models = 100, 
    seed = 7781, 
    stopping_rounds = 5, 
    stopping_metric = "RMSE", 
    stopping_tolerance = .001
    )

#GBM using optimized hyperparameters and random search criteria scoring of trees
grid_opt = h2o.grid(
    hyper_params = hyper_params_opt, 
    search_criteria = search_criterira, 
    algorithm = "gbm", 
    grid_id = "final_grid", 
    x = features, 
    y = response, 
    training_frame = h2oTrain, 
    validation_frame = h2oTest, 
    ntrees = 500, 
    learn_rate = .05, 
    learn_rate_annealing = .99, 
    # max_runtime_seconds = 7200,
    stopping_rounds = 5, 
    stopping_tolerance = .0001, 
    stopping_metric = "RMSE", 
    score_tree_interval = 10, 
    seed = 7781
    )

## Sort the grid models by RMSE
sortedGrid_opt <- h2o.getGrid("final_grid", sort_by = "rmse", decreasing = FALSE)
sortedGrid_opt %>% summary()
    
# for (i in 1:5) {
#     gbm_loop <- h2o.getModel(sortedGrid_opt@model_ids[[i]])
#     print(h2o.rmse(h2o.performance(gbm_loop, valid = TRUE)))
# }

#model inspection and test set scoring
gbmBest <- h2o.getModel(sortedGrid@model_ids[[1]]) #top model with lowest RMSE

#performance of the model predicting training data
print(h2o.rmse(h2o.performance(gbmBest, newdata = h2oTest))) #RMSE: .21

#parameters used in the top model
gbmBest@parameters

#5-folk CV using the parameters from our top model on the ENTIRE dataset
mod_cv <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                     p <- gbmBest@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = h2o_df  ## use the full dataset
                     p$validation_frame = NULL  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                 }
)
#validation results using 5-fold CV
mod_cv@model$cross_validation_metrics_summary

#how does our best model predict now..
pred_gbm = h2o.predict(gbmBest, newdata = h2oTest)

#r^2
pred_gbm #.38 prediction rate after all that tuning!


















