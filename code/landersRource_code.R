# Landers' Datacamp Course: R

#keep it down!
shhh = function(...){
    suppressPackageStartupMessages(
        suppressWarnings(base::library(...)))
}

shhh(tidyverse)
shhh(psych)

#import and labeling
#create a df and name it

rt_df = read.csv("../data/landers_wk2data.csv", header = T)
str(rt_df)

#recode condition and gender factors

rt_df = rt_df %>%
    mutate(condition = recode(condition, A = "Control", B = "Experimental"), 
           gender = recode(gender, M = "Male", F = "Female", T = "Transgender"))

#Analysis
#mean reaction time across rt_df

mean(rt_df$rt)

#filter for female gender only

rt_f_df = rt_df %>%
    filter(gender == "Female")
rt_f_df

#display frequences and/or quantitative summaries

summary(rt_f_df)
describe(rt_f_df)

#create a histogram of female rt

ggplot(rt_f_df, aes(x = rt)) + 
    geom_histogram(binwidth = 350, color = "black", fill = "blue") +
    labs(title = "Histogram", x = "Reaction Time")
hist(rt_f_df$rt, xlab = "Reaction Time", main = "Histogram", col = "red")

#create a list with both dfs

datasets = list(rt_df, rt_f_df)
str(datasets)

#display contents of rt from first df within datasets
#datasets[1]

#read in raw data and create df
raw_df = read.csv("../data/landers_wk3data.csv", header = T)
str(raw_df)

# recast 'timeStart' & 'timeEnd' into POSIX format
raw_df = raw_df %>%
    mutate_at(vars('timeStart', 'timeEnd'), as.POSIXct)
#str(raw_df) #notice the vars changed from 'factor' to 'POSIX'

#research assistants participated during the month of June
#remove them and create a new df; q6 was check response item keyed for 1
clean_df = raw_df %>%
    filter_at(vars('timeStart', 'timeEnd', 'q6'), 
              any_vars(. >= "2017-07-01 00:00:00" & q6 == 1))

#save total time spent on study in seconds as a new variable
#recast timeSpent as num
clean_df = clean_df %>%
    mutate('timeSpent_secs' = as.numeric(difftime(
            timeEnd, timeStart, units = 'secs')))

#create a histogram of timeSpent
ggplot(clean_df, aes(x = timeSpent_secs)) + 
    geom_histogram(binwidth = 100, color = "white", fill = "dark green") +
    labs(title = "Research Study Time Expenditure", x = "# of seconds")

hist(clean_df$timeSpent_secs, col = "orange", main = "Research Study Time Expenditure", 
     xlab = "# of seconds")

#iterate table function over the 5th and 14th columns of df
freq_tbls_list = lapply(clean_df[, c(5, 14)], table)
freq_tbls_list

#tidyverse syntax
# clean_df %>% 
#     select('q1', 'q10') %>%
#     lapply(table)

lapply(freq_tbls_list, barplot)

