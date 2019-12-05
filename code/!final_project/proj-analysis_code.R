rm(list = ls()) #keep env clean

#load libraries
library(tidyverse)
library(caret)
library(h2o)
# library(randomForest)
# library(psych)
# library(xgboost)
# library(glmnet)

#read in dataset - feature attributes must be specified from csv files...
dat = read_csv("../../data/finalnorm_df.csv", col_names = T) %>% 
    mutate_if(is.character, as.factor)

#Modeling

#preprocess predictors - dummy code variables
dummy_coded = dummyVars(~ ., data = dat[, -1])
dummies = data.frame(predict(dummy_coded, newdata = dat[, -1])) %>% 
    cbind('date' = dat$date)
    

glimpse(dummies) #all num features

# #create a list containing training and testing dfs
datList = list(
    train = filter(dummies, date < "2018-01-01") %>%
        mutate(date = NULL),
    test = filter(dummies, date >= "2018-01-01") %>%
        mutate(date = NULL)
)

#using h2o package
h2o.init(nthreads = -1)

h2o_df = as.h2o(dummies[, -151])

h2oTrain = h2o_df[1:217267, ]
h2oTest = h2o_df[217268:236918, ]

response = "waittime"
features = setdiff(colnames(h2oTrain), response)

gbm = h2o.gbm(x = features, y = response, training_frame = h2oTrain)
summary(gbm)
h2o.rmse(gbm)


hyper_params = list(max_depth = c(4, 6, 8, 12, 16, 20, 25))
grid = h2o.grid(
    hyper_params = hyper_params, 
    # search_criteria = list(strategy = "Cartesian"), 
    algorithm = "gbm",
    grid_id = "depth_grid",
    x = features,
    y = response,
    training_frame = h2oTrain,
    validation_frame = h2oTest,
    # ntrees = 1000, 
    # learn_rate = .05, 
    # learn_rate_annealing = .99, 
    # sample_rate = .8, 
    # col_sample_rate = .0, 
    seed = 1234,
    # stopping_rounds = 3, 
    stopping_metric = "RMSE"
    # score_tree_interval = 10
)

sortedGrid = h2o.getGrid("depth_grid", sort_by = "rmse", decreasing = TRUE)
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]

minDepth = min(as.numeric(topDepths))
minDepth
maxDepth = max(as.numeric(topDepths))
maxDepth

#####

hyper_params2 = list(max_depth = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50))
grid = h2o.grid(
    hyper_params = hyper_params2, 
    # search_criteria = list(strategy = "Cartesian"), 
    algorithm = "gbm",
    grid_id = "depth_grid2",
    x = features,
    y = response,
    training_frame = h2oTrain,
    validation_frame = h2oTest,
    # ntrees = 1000, 
    # learn_rate = .05, 
    # learn_rate_annealing = .99, 
    # sample_rate = .8, 
    # col_sample_rate = .0, 
    seed = 4321,
    # stopping_rounds = 3, 
    stopping_metric = "RMSE"
    # score_tree_interval = 10
)

sortedGrid2 = h2o.getGrid("depth_grid2", sort_by = "rmse", decreasing = TRUE)
sortedGrid2

## find the range of max_depth for the top 5 models
topDepths2 = sortedGrid2@summary_table$max_depth[1:5]

minDepth2 = min(as.numeric(topDepths2))
minDepth2
maxDepth2 = max(as.numeric(topDepths2))
maxDepth2


# Model inspection
#this is our best model!
gbm_final = h2o.getModel(sortedGrid2@model_ids[[1]])
gbm_final

print(h2o.rmse(h2o.performance(gbm_final, newdata = h2oTest)))
#.24

gbm_final@parameters

model_check <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                     p <- gbm_final@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = h2o_df      ## use the full dataset
                     p$validation_frame = NULL  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                 }
)
model@model$cross_validation_metrics_summary


# #### too big? 
# hyper_params3 = list(max_depth = c(10, 15, 20, 25, 30))
# grid = h2o.grid(
#     hyper_params = hyper_params3, 
#     # search_criteria = list(strategy = "Cartesian"), 
#     algorithm = "gbm",
#     grid_id = "depth_grid2",
#     x = features,
#     y = response,
#     training_frame = h2oTrain,
#     validation_frame = h2oTest,
#     ntrees = 1000, 
#     # learn_rate = .05, 
#     # learn_rate_annealing = .99, 
#     # sample_rate = .8, 
#     # col_sample_rate = .0, 
#     seed = 1423,
#     # stopping_rounds = 3, 
#     stopping_metric = "RMSE"
#     # score_tree_interval = 10
# )
# 
# sortedGrid3 = h2o.getGrid("depth_grid2", sort_by = "rmse", decreasing = TRUE)
# sortedGrid3
# 
# ## find the range of max_depth for the top 5 models
# topDepths3 = sortedGrid3@summary_table$max_depth[1:5]
# 
# minDepth3 = min(as.numeric(topDepths3))
# minDepth3
# maxDepth3 = max(as.numeric(topDepths3))
# maxDepth3
