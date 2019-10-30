#load libraries
library(tidyverse)
library(readxl)
library(psych)
library(lubridate)


#read in pirates of carribean data
pir_raw = read_csv('../data/disneydata/pirates_of_caribbean.csv', col_names = T)

#read in metaata
meta_raw = read_csv('../data/disneydata/metadata.csv', col_names = TRUE, 
                    #change guess arg to better guage col type
                    guess_max = 5000)

#wrangle pirate of carribean data
pir_dat = pir_raw %>% 
    replace_na(
        list('SPOSTMIN' = 0, 'SACTMIN' = 0)) %>% 
    mutate(
        'date' = mdy(date),
        'waittime' = SPOSTMIN + SACTMIN,
        'SPOSTMIN' = NULL, 
        'SACTMIN' = NULL) %>% 
    filter(waittime != -999) #ride closed indicator


#group dates for plots
date_grps = pir_dat %>% 
    group_by(date) %>% 
    summarise(waittime = median(waittime)) #use median

#create a line graph of wait time over the years
date_grps %>% 
    ggplot(aes(x = date, y = waittime)) + 
    geom_line(color = 'dark blue', size = .25) +
    stat_smooth(color = 'red', fill = 'red', method = ) + 
    scale_x_date(date_labels = '%b %y', date_breaks = '1 month') + 
    labs(
        title = 'Disney\'s Pirates of the Carribean',
        subtitle = 'Median per Day Wait Time (2012-2018)',
        x = 'Date', 
        y = 'Wait Time (minutes)') + 
    annotate(
        geom = 'text', 
        x = as.Date('2015-06-08'), 
        y = 35, 
        label = 'No \n data...', 
        size = 2) + 
    annotate(
        geom = 'point', 
        x = as.Date('2015-08-01'), 
        y = 28, 
        size = 12, shape = 21, fill = 'transparent') + 
    theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
    
#wrangle metadata before merging
meta2 = meta_raw %>% 
    rename_all(list(tolower)) %>% #changes column names to lowercase
    select(date, dayofweek, weekofyear, monthofyear, wdwmeantemp) %>% 
    mutate(date = mdy(date))
    #use to recode values into specific factors...not useful in this assignment
    #        dayofweek = recode(dayofweek, 
    #                           `1` = 'Thurs', `2` = 'Fri', `3` = 'Sat', 
    #                           `4` = 'Sun', `5` = 'Mon', `6` = "Tues", 
    #                           `7` = 'Wed'), 
    #        monthofyear = recode(monthofyear, 
    #                             `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', 
    #                             `4` = 'Apr', `5` = 'May', `6` = 'Jun',
    #                             `7` = 'July', `8` = 'Aug', `9` = 'Sept', 
    #                             `10` = 'Oct', `11` = 'Nov', `12` = 'Dec')
    #        ) %>% 
    # mutate_at(c('dayofweek', 'monthofyear'), list(as.factor))

#merge dfs by date; rm 2nd column of datetimes
mrg_dat = merge(pir_dat, meta2, by = 'date')[, -2] %>% 
    drop_na() #rm NA values

library(GGally)
ggpairs(data.frame(pir_meta_mrg[, -1]))

#complete regression analysis
fit = lm(waittime ~ dayofweek + weekofyear + monthofyear + wdwmeantemp + 
             weekofyear*wdwmeantemp, data = mrg_dat)
summary(fit)
