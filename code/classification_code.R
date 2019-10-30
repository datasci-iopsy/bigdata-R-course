#keep env clean
rm(list = ls())

#load appropriate libraries
library(tidyverse)
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
    replace_na(
        list('SPOSTMIN' = 0, 'SACTMIN' = 0)) %>% 
    mutate(
        'date' = mdy(date),
        'waittime' = SPOSTMIN + SACTMIN,
        'fct_waittime' = ifelse(waittime <= 25, 'short', 'long'), #dichotomize feature
        'SPOSTMIN' = NULL, 
        'SACTMIN' = NULL) %>% 
    mutate_at('fct_waittime', list(as.factor)) %>% 
    filter(waittime != -999) #ride closed indicator

#wrangle metadata before merging
meta_dat = meta_raw %>% 
    rename_all(list(tolower)) %>% #changes column names to lowercase
    select(date, dayofweek, monthofyear, wdwmeantemp, insession_central_fl, 
           mkhoursemh) %>% 
    mutate(date = mdy(date), 
           #01/01/12 was a Thurs
           dayofweek = recode(dayofweek,
                              `1` = 'Thurs', `2` = 'Fri', `3` = 'Sat',
                              `4` = 'Sun', `5` = 'Mon', `6` = "Tues", 
                              `7` = 'Wed'), 
           monthofyear = recode(monthofyear, 
                                `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', 
                                `4` = 'Apr', `5` = 'May', `6` = 'Jun', 
                                `7` = 'July', `8` = 'Aug', `9` = 'Sept', 
                                `10` = 'Oct', `11` = 'Nov', `12` = 'Dec'), 
           insession_central_fl = parse_number(insession_central_fl) / 100, 
           wdwmeantemp = as.double(wdwmeantemp), 
           mkhoursemh = as.double(mkhoursemh)
           ) %>%
mutate_at(c('dayofweek', 'monthofyear'), list(as.factor))

#merge dfs by date; rm 2nd column of datetimes
mrg_dat = merge(pir_dat, meta_dat, by = 'date')[, -2] %>% 
    drop_na() #rm NA values

#this would be a good place ot explore the data

#bar graph of categorical variables
day_order = c('Sun', 'Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat')
month_order = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'July', 'Aug', 'Sept', 
              'Oct', 'Nov', 'Dec')

count(mrg_dat, dayofweek, monthofyear, fct_waittime) %>% 
    ggplot(aes(x = factor(dayofweek, levels = rev(day_order)), y = n, 
               fill = fct_waittime)) + 
        geom_bar(stat = 'identity') + 
        coord_flip() + 
    facet_wrap(~factor(monthofyear, levels = month_order), ncol = 4) + 
    theme_light() + 
    labs(
        title = 'Disney\'s Pirates of the Carribean Attraction', 
        subtitle = 'Daily Waittime by Month',
        x = 'Day of the Week', 
        y = 'Number of Riders'
        ) + 
    theme(legend.title = element_blank()) + 
    scale_fill_discrete(labels = c('Long Wait', 'Short Wait'))
    

#scatterplot of disney's mean temp, the # of hrs the park is open by waittime
(sct_plot = ggplot(mrg_dat, aes(x = wdwmeantemp, y = mkhoursemh, 
                               color = fct_waittime)) + 
        geom_point(size = 1))


#boxplots
(temp_plot = ggplot(mrg_dat, aes(x = fct_waittime, y = wdwmeantemp, 
                                fill = fct_waittime)) + 
        geom_boxplot() + 
        coord_flip())


(hrs_plot = ggplot(mrg_dat, aes(x = fct_waittime, y = mkhoursemh, 
                               fill = fct_waittime)) + 
        geom_boxplot()+ 
        coord_flip())

library(cowplot)
first_row = plot_grid(sct_plot)
second_row = plot_grid(temp_plot, hrs_plot)

#final plot layout
(gg_all = plot_grid(first_row, second_row, ncol = 1))

#complete logistic regression analysis
fit = glm(fct_waittime ~ dayofweek + monthofyear + wdwmeantemp + 
              insession_central_fl + mkhoursemh + wdwmeantemp*insession_central_fl, 
          data = mrg_dat[1:100000, ], 
          family = 'binomial')

summary(fit)









summary(fit)