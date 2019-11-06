#rm(list = ls())

#load appropriate libraries
library(tidyverse)
library(broom) 
library(psych)
library(lubridate)

#read in pirates of carribean data
pir_raw = read_csv('../data/disneydata/pirates_of_caribbean.csv', col_names = T)

#read in metaata
meta_raw = read_csv('../data/disneydata/metadata.csv', col_names = TRUE, 
                    #change guess arg to better guage col type
                    guess_max = 5000)

#wrangle pirate of carribean df
pir_dat = pir_raw %>% 
    replace_na(list('SPOSTMIN' = 0, 'SACTMIN' = 0)) %>% 
    mutate(
        'date' = mdy(date),
        'waittime' = SPOSTMIN + SACTMIN,
        #dichotomize feature; is it a short wait?
        'fct_waittime' = factor(ifelse(waittime <= 25, 'short', 'long')),
        'SPOSTMIN' = NULL, 
        'SACTMIN' = NULL) %>% 
    filter(waittime != -999) #ride closed indicator

#wrangle metadata before merging
meta_dat = meta_raw %>% 
    rename_all(list(tolower)) %>% #changes column names to lowercase
    select(
        date, 
        'month' = monthofyear, 
        season, 
        'temp_mean' = wdwmeantemp, 
        'insesh_fl' = insession_florida, 
        'hist_precip' = weather_wdwprecip, 
        'total_hrs' = mkhoursemh, 
        'cap_lost' = capacitylost_mk, 
        'parades' = mkprdday) %>% 
    mutate(
        date = mdy(date), 
        month = recode(factor(month), 
                       `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', `4` = 'Apr', 
                       `5` = 'May', `6` = 'Jun', `7` = 'July', `8` = 'Aug', 
                       `9` = 'Sept', `10` = 'Oct', `11` = 'Nov', `12` = 'Dec'),
        season = factor(str_to_title(season)), 
        temp_mean = as.double(temp_mean),
        insesh_fl = parse_number(insesh_fl) / 100 #percentage
    ) %>%
    mutate_if(is.numeric, list(as.double)) %>% 
    drop_na()

#merge dfs by date; rm 2nd column of datetimes
mrg_dat = merge(pir_dat, meta_dat, by = 'date') %>%
    select(-c(date, datetime, waittime))

mrg_dat %>% 
    select_if(is.numeric) %>%
    sapply(function(x) summary(x)) %>% 
    data.frame()

# #summary statistics for numerical features using tidy; rmarkdown has issues rendering
# sum_stats = mrg_dat %>% 
#     select_if(is.numeric) %>% 
#     sapply(function(x) broom::tidy(summary((x)))) %>% 
#     as.data.frame()

#normalise the data
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#apply function to dataset
mrg_dat_norm = mrg_dat %>% 
    mutate_if(is.numeric, normalize)

#At this point, there are a lot of relatively large files in our environment so 
#it's good practice to tidy up env - R relies heavily on memory availablity. 
rm(list = ls()[grep("meta_|pir_", ls())])
rm(mrg_dat)

#required to split data and preprocessing
library(caTools)
library(caret)
library(doMC) #parallelization
registerDoMC(cores = 4)

set.seed(984) #for reproducibility 

sample = sample.split(mrg_dat_norm, SplitRatio = .75)

#training and testing dfs
train = subset(mrg_dat_norm, sample == TRUE)
test = subset(mrg_dat_norm, sample == FALSE) 

#run logistic model
log_mod = caret::train(fct_waittime ~. + season*hist_precip + cap_lost*total_hrs, 
                       data = train, 
                       trControl = trainControl(method = 'cv', number = 5), 
                       method = 'glm', 
                       family = 'binomial')

#get raw output of predictions
log_mod_pred = predict(log_mod, newdata = test, type = 'raw')

#summary table of non-significant coefs
logit_sum = summary(log_mod)$coefficients %>% 
    round(2) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    select(-4) %>% 
    rename('Betas' = 1, 'Est' = 2, 'p_Value' = 4) %>% 
    filter(p_Value > .05)
logit_sum

#function to calculate model accuracy
calc_acc = function(actual, predicted) {
    mean(actual == predicted)
}

# 64.4% % accuracy
calc_acc(actual = test$fct_waittime, 
         predicted = predict(log_mod, newdata = test))

#preprocess predictors - knn needs num values for factors
dummy_coded = dummyVars(fct_waittime ~ ., data = mrg_dat_norm)

dummies = data.frame(predict(dummy_coded, newdata = mrg_dat_norm)) %>% 
    #the 'fct_waittime' factor is removed; dummy coding is f-1 
    #combine criterion with new dataset
    cbind('fct_waittime' = as.factor(mrg_dat_norm$fct_waittime))

dummies_train = subset(dummies, sample == TRUE)
dummies_test = subset(dummies, sample == FALSE)

#knn model
#define train opts
ctrl = trainControl(method = 'cv', number = 5) #5-fold cross validation

#run model
knn_mod = caret::train(fct_waittime ~ .,  
                       data = dummies_train, 
                       method = 'knn', 
                       trControl = ctrl, 
                       tuneLength = 7)
knn_mod

knn_mod_pred = predict(knn_mod, dummies_train, type = 'raw')

#confusion matrices for both models
confusionMatrix(log_mod_pred, test$fct_waittime)
confusionMatrix(knn_mod_pred, x_test$fct_waittime)