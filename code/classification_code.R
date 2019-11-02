#keep env clean
rm(list = ls())

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

#summary statistics for numerical features
sum_stats = mrg_dat %>% 
    select_if(is.numeric) %>% 
    sapply(function(x) broom::tidy(summary((x)))) %>% 
    as.data.frame()
sum_stats

#normalise the data
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#apply function to dataset
mrg_dat_norm = mrg_dat %>% 
    mutate_if(is.numeric, normalize)
    

#required to split data
library(caTools)
set.seed(984) #for reproducibility 

sample = sample.split(mrg_dat_norm, SplitRatio = .75)

#training and testing dfs
train = subset(mrg_dat_norm, sample == TRUE)
test = subset(mrg_dat_norm, sample == FALSE)

# #complete logistic regression analysis
# 
# #fit includes all variables and no interactions
# fit = glm(fct_waittime ~ ., data = train, family = 'binomial')
# 
# #fit2 includes all variables and two interactions
# fit2 = glm(fct_waittime ~ . + season*hist_precip + insesh_fl*parades, data = train, 
#            family = 'binomial')
# 
# summary(fit)
# summary(fit2)
# 
# prop.table(table(train$fct_waittime)) %>% 
#     round(2)
# 
# exp(coef(fit2))

#load aret package
library(caret)

#run logistic model
log_mod = caret::train(form = fct_waittime ~ ., 
                       data = train, 
                       trControl = trainControl(method = 'cv', number = 5), 
                       method = 'glm', 
                       family = 'binomial')

#get raw output of predictions
log_mod_pred = predict(log_mod, test, type = 'raw')

#function to calculate model accuracy
calc_acc = function(actual, predicted) {
    mean(actual == predicted)
}

# 64.2% % accuracy
calc_acc(actual = test$fct_waittime, 
         predicted = predict(log_mod, newdata = test))

#confusion matrix
confusionMatrix(log_mod_pred, test$fct_waittime)

#knn model
knn_mod = caret::train(fct_waittime ~ ., 
                       data = train, 
                       method = 'knn', 
                       trControl= trainControl(method = 'cv', number = 5,), 
                       tuneLength = 10)
knn_mod











