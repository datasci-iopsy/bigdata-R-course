shhh = function(...){
    suppressWarnings(
        suppressPackageStartupMessages(base::library(...))
    )
}

#load appropriate libraries
shhh(dplyr)
shhh(ggplot2)
shhh(psych)

#read the dataset
dat = read.csv('../data/wdbc_data.csv', stringsAsFactors = FALSE)
head(dat)

#remove first column/'id' variable
dat = dat[, -1]

#display a table count of our criterion (i.e, diagnosis)
table(dat$diagnosis)

#change 'diagnosis' (our criteria) to a factor with new labels
dat = dat %>%
    mutate(diagnosis = recode(factor(diagnosis), 
                              B = 'Benign', M = 'Malignant'))
head(dat)

#check proportions of our critera
prop.table(table(dat$diagnosis)) %>%
    round(2)

#take a look at the three features of interests
summary(dat[c('radius_mean', 'area_mean', 'smoothness_mean')])

#create a func to normalize data; metrics should be equal across features
normalize = function(x){
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}
#normalize the dataset
dat_norm = as.data.frame(lapply(dat[, 2:31], normalize))
head(dat_norm)[c('radius_mean', 'area_mean', 'smoothness_mean')]

#create training and test datasets
datTrain = dat_norm[1:469, ]
datTest = dat_norm[470:569, ]
#create training and test labels
datTrain_labels = dat[1:469, 1]
datTest_labels = dat[470:569, 1]
#use the 'class' library for one kNN algorithm; there are a plethora of others!
library(class)
library(gmodels)

datTest_pred = knn(train = datTrain, test = datTest, cl = datTrain_labels, k = 21)
#table to see identifications
CrossTable(x = datTest_labels, y = datTest_pred, prop.chisq = F)

#let's try another transformation (z-score)
dat_z = as.data.frame(scale(dat[, -1]))
head(dat_z)[c('radius_mean', 'area_mean', 'smoothness_mean')]

#repeat steps from above with new z-scored data
datTrain = dat_z[1:469, ]
datTest = dat_z[470:569, ]

datTrain_labels = dat[1:469, 1]
datTest_labels = dat[470:569, 1]
#use the same parameters as well
datTest_pred = knn(train = datTrain, test = datTest, cl = datTrain_labels, k = 21)
#no differences between the transformations
CrossTable(x = datTest_labels, y = datTest_pred, prop.chisq = F)