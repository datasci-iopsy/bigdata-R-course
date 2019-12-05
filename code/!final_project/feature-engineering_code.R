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
        "day" = dayofweek,
        "week" = weekofyear, 
        'month' = monthofyear, 
        season, 
        'holiday_prox' = holidaypx,
        'holiday_rnk' = holidaym, 
        'holiday_name' = holidayn, 
        'holiday_YorN' = holiday, 
        'wdw_event_code' = wdwevent, 
        'wdw_race' = wdwrace, 
        'temp_mean' = wdwmeantemp, 
        'temp_max' = wdwmaxtemp, 
        'temp_min' = wdwmintemp, 
        'mk_event' = mkeventn, 
        'ep_event' = epeventn, 
        'hs_event' = hseventn, 
        'ak_event' = akeventn,
        'insesh_all' = insession, 
        'insesh_sqrt' = insession_sqrt_wdw,
        'insesh_cntrl_fl' = insession_central_fl, 
        'insesh_fl' = insession_florida,
        'mk_extra_morn' = mkemhmorn, 
        'mk_extra_eve' = mkemheve, 
        'mk_total_hrs' = mkhoursemh,
        'ep_extra_morn' = epemhmorn, 
        'ep_extra_eve' = epemheve, 
        'ep_total_hrs' = ephoursemh,
        'hs_extra_morn' = hsemhmorn, 
        'hs_extra_eve' = hsemheve, 
        'hs_total_hrs' = hshoursemh,
        'ak_extra_morn' = akemhmorn, 
        'ak_extra_eve' = akemheve, 
        'ak_total_hrs' = akhoursemh,
        'event' = partyseason_wdw, 
        'hist_precip' = weather_wdwprecip, 
        'mk_cap_lost' = capacitylost_mk, 
        'ep_cap_lost' = capacitylost_ep, 
        'hs_cap_lost' = capacitylost_hs, 
        'ak_cap_lost' = capacitylost_ak, 
        'mk_day_parades' = mkprdday, 
        'mk_fireworks' = mkfirewk,
        'ep_fireworks' = epfirewk, 
        'hs_day_parades' = hsprdday, 
        'hs_fireworks' = hsfirewk, 
        'hs_nightshows' = hsshwngt, 
        'ak_day_parades' = akprdday, 
        'ak_nightshows' = akshwngt
        ) %>% 
    mutate(
        date = mdy(date), #changes date format to match pir_raw
        #change fct levels 
        day = recode(factor(day), 
                     `1` = 'Sun', `2` = 'Mon', `3` = 'Tues', `4` = 'Wed', 
                     `5` = 'Thurs', `6` = 'Fri', `7` = 'Sat'), 
        month = recode(factor(month), 
                       `1` = 'Jan', `2` = 'Feb', `3` = 'Mar', `4` = 'Apr', 
                       `5` = 'May', `6` = 'Jun', `7` = 'July', `8` = 'Aug', 
                       `9` = 'Sept', `10` = 'Oct', `11` = 'Nov', `12` = 'Dec'),
        #change season to title case; shorten MLK Jr day
        season = recode(factor(str_to_title(season)),
                        "Martin Luther King Junior Day" = "MLK Jr. Day"),
        holiday_name = str_to_upper(holiday_name), 
        holiday_YorN = recode(factor(holiday_YorN),
                              `0` = 'No', `1` = 'Yes'), 
        wdw_event_code = recode(factor(wdw_event_code), 
                                `0` = "No Event", `1` = "Some Event"), 
        wdw_race = recode(factor(wdw_race),
                          `0` = "No Race", `1` = "Some Race"),
        # ***mk_event, ep_event, ak_event all need to drop_&_replace NA for factor, 
        event = factor(str_to_upper(event)) #change codes to upper case
    ) %>% 
    # mutate_at(
    #     vars(starts_with("insesh_")), parse_number
    # ) %>%
    mutate_if(is.numeric, list(as.double))

#to handle multiple mutate_at calls
mutate2 <- function(data, .vars, .funs) {
    stopifnot(length(.vars) == length(.funs))
    
    for (i in seq_along(.vars)) {
        data <- mutate_at(data, .vars[[i]], .funs[[i]])
    }
    data
}

meta = meta %>% 
    mutate2(
        list(vars(starts_with("insesh_")), vars(ends_with("_event")), 
             vars(ends_with("_name"))), 
        list(~ parse_number(.) / 100, ~ replace_na(., "NONE"), 
             ~replace_na(., "No Holiday"))
    ) %>% 
    mutate_if(is.character, as.factor)


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
    select(-datetime) %>%  #rm date and datetime features
    drop_na()

rm(list = ls(pattern = "meta|pir|mrg|mutate|shhh")) #keep env cln for sourcing!

