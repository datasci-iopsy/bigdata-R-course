#keep env clean
rm(list = ls())

#keep it down!
shhh = function(...) {
    suppressWarnings(
        suppressPackageStartupMessages(base::library(...))
    )
}

#load libraries
shhh(tidyverse)
shhh(lubridate)
# library(pacman)
# pacman::p_load("tidyverse", "lubridate")

#read in pirates of carribean data
pir_raw = read_csv('../../data/disneydata/pirates_of_caribbean.csv', 
                   col_names = T)

#read in metaata
meta_raw = read_csv('../../data/disneydata/metadata.csv', col_names = TRUE, 
                    #change guess arg to better guage col type
                    guess_max = 5000)

#wrangle pirate of carribean df
pir = pir_raw %>% 
    replace_na(list('SPOSTMIN' = 0, 'SACTMIN' = 0)) %>% 
    mutate(
        'date' = mdy(date),
        datetime = force_tz(as.POSIXct(datetime), tzone = "America/New_York"),
        'waittime' = SPOSTMIN + SACTMIN,
        'SPOSTMIN' = NULL, 
        'SACTMIN' = NULL) %>% 
    filter(waittime != -999 & waittime != -7) #ride closed indicator

#wrangle metadata before merging
meta = meta_raw %>% 
    rename_all(list(tolower)) %>% #changes column names to lowercase
    select(
        date, 
        'month' = monthofyear, 
        season, 
        'holiday_prox' = holidaypx,
        'holiday_rnk' = holidaym, 
        'temp_mean' = wdwmeantemp, 
        'insesh_fl' = insession_florida,
        'extra_morn' = mkemhmorn, 
        'extra_eve' = mkemheve, 
        'total_hrs' = mkhoursemh,
        'event' = partyseason_wdw, 
        'hist_precip' = weather_wdwprecip, 
        'cap_lost' = capacitylost_mk, 
        'parades' = mkprdday, 
        'fireworks' = mkfirewk,
        ) %>% 
    mutate(
        date = mdy(date), 
        month = recode(factor(month), 
                       `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', `4` = 'Apr', 
                       `5` = 'May', `6` = 'Jun', `7` = 'July', `8` = 'Aug', 
                       `9` = 'Sept', `10` = 'Oct', `11` = 'Nov', `12` = 'Dec'),
        season = factor(str_to_title(season)), 
        insesh_fl = parse_number(insesh_fl) / 100, #percentage
        event = factor(str_to_upper(event))
    ) %>%
    mutate_if(is.numeric, list(as.double)) %>% 
    drop_na()

#merge dfs by date
mrg_dat = merge(pir, meta, by = 'date') %>% 
    mutate(
        datetime = strftime(datetime, format = '%H:%M'), 
        timeofday = as.factor(case_when(
            datetime >= "05:00" & datetime <= "11:59" ~ "Morning",
            datetime >= "12:00" & datetime <= "15:59" ~ "Afternoon",
            datetime >= "16:00" & datetime <= "19:59" ~ "Evening",
            datetime >= "20:00" & datetime <= "23:59" ~ "Night",
            datetime >= "00:00" & datetime <= "04:59" ~ "Extended Hrs"
        ))
    ) %>% 
    mutate_at(
        vars(contains("extra_")), as.factor 
        )

#final clean dataset
dat = mrg_dat %>% 
    select(-c(date, datetime))

rm(list = ls(pattern = "meta|pir|mrg"))

#write final dfs to excel files
#write_csv(final_df, path = "../../data/final_df.csv")
