## Basic R Stuff

library(psych)
library(tidyverse)

#read in a dataset
df = read.csv("../data/example.dat", header = T)


#recode reverse coded items
df_recd = df %>%
    mutate_at(vars(A1, A4:A6), list(~6 - .))

#take a peek a the datasets
head(df, 10)
head(df_recd, 10)

#lets try some if-else statements
age = 25

if (age < 25) {
    print("Youngin'")
} else if (age > 25) { 
    print("Wise")
} else if (age == 25) {
    print("Perfect timing!!")
} else {  #it's usually good to end with an else to close scoping
    print("Unborn")
}


## Sampling distributions with R
#using correlations
corrData = NULL #containing of correlations

n = 50 
for (i in 1:1000){
    r1 = rnorm(mean = 0, sd = 1, n)
    r2 = rnorm(mean = 0, sd = 1, n)
    
    c = cor(r1, r2)
    #print(c)
    
    corrData = append(corrData, c)
}

## Sample Error of Means Illustration

# library(psych)


# In this example, we are going to generate samples of various sizes

meanData=NULL  # first lets make a holder for our means
#now we are going to generate several datasets using a For loop
for (i in 1:100){ 
  x <- rnorm(mean=0, sd = 1, n=50)  # this generates a variable called X with a mean of zero
  m <- mean(x)    # just compute the mean as m
#  print(m)        # print it out just for fun
  meanData <- append(meanData,m)    # add the mean for this sample to the database of means from all samples
}   # end our for loop
#meanData      # just print out all of the means
describe(meanData)    # summary stats for all of the means
hist50 <- hist(meanData, breaks=seq(-.5,.5,by=.05), main="N=50")

describe(corrData)

#histogram of data with N = 50
hist(corrData)

#get critical value when rho = 0
sorted = sort(corrData)
my_position = length(sorted) * .95 #95th percentile position in array
critical_value = sorted[my_position]; critical_value





