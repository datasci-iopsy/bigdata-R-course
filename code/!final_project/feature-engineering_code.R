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

#read in pirates of carribean data
pir_raw = read_csv('../../data/disneydata/pirates_of_caribbean.csv', 
                   col_names = T)

#read in metaata
meta_raw = read_csv('../../data/disneydata/metadata.csv', col_names = TRUE, 
                    #change guess arg to better guage col type
                    guess_max = 5000)

#wrangle pirate of carribean dataset
pir = pir_raw %>% 
    replace_na(list('SPOSTMIN' = 0, 'SACTMIN' = 0)) %>% #replace NAs with 0s
    mutate(
        'date' = mdy(date), #change date format
        #NY timezone - default apparently is UTC 
        datetime = force_tz(as.POSIXct(datetime), tzone = "America/New_York"),
        #only one time is available per obs so combine features
        'waittime' = SPOSTMIN + SACTMIN,
        #rm rows with NULL
        'SPOSTMIN' = NULL, 
        'SACTMIN' = NULL) %>% 
    filter(waittime != -999 & waittime != -7) #ride closed indicators

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
        date = mdy(date), #changes date format to match pir_raw
        #change fct levels 
        month = recode(factor(month), 
                       `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', `4` = 'Apr', 
                       `5` = 'May', `6` = 'Jun', `7` = 'July', `8` = 'Aug', 
                       `9` = 'Sept', `10` = 'Oct', `11` = 'Nov', `12` = 'Dec'),
        #change season to title case; shorten MLK Jr day
        season = recode(factor(str_to_title(season)), 
                        "Martin Luther King Junior Day" = "MLK Jr. Day"),
        insesh_fl = parse_number(insesh_fl) / 100, #percentage 
        event = factor(str_to_upper(event)) #change codes to upper case
    ) %>%
    mutate_if(is.numeric, list(as.double)) %>% 
    drop_na() #drop NAs

#merge datasets by date
mrg_dat = merge(pir, meta, by = 'date') %>% 
    mutate(
        datetime = strftime(datetime, format = '%H:%M'), #extract time only
        #create new fct feature with appropriate levels
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
        ) #changes specific features to fct

#final wrangled dataset
dat = mrg_dat %>% 
    select(-c(date, datetime)) #rm date and datetime features

rm(list = ls(pattern = "meta|pir|mrg")) #keep env cln for sourcing!

#write final df to data path if one prefers reading in files
#write_csv(final_df, path = "../../data/final_df.csv")