#keep env clean
rm(list = ls())

#load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tibble)

#read in dataset
dat = read_csv("../../data/final_df.csv", 
               col_types = cols(
                   month = col_factor(), 
                   season = col_factor(), 
                   extra_morn = col_factor(), 
                   extra_eve = col_factor(), 
                   event = col_factor(), 
                   timeofday = col_factor())
               )
                                

#summary statistics of dataframe
dat %>% 
    select_if(is.numeric) %>%
    sapply(function(x) summary(x)) %>% 
    round(2)

#outlier detection
detect_outs <- function(dt, var) { 
    var_name <- eval(substitute(var), eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
    
    boxplot(var_name, main = "With Outliers")
    hist(var_name, main = "With outliers", xlab = NA, ylab = NA)
    
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    
    boxplot(var_name, main = "Without Outliers")
    hist(var_name, main = "Without Outliers", xlab = NA, ylab = NA)
    title("Outlier Check", outer = TRUE)
    
    na2 <- sum(is.na(var_name))
    
    cat("Outliers identified:", na2 - na1, "n")
    cat("Propotion (%) of outliers:", 
        round((na2 - na1) / sum(!is.na(var_name)) * 100, 1), "n")
    cat("Mean of the outliers:", 
        round(mo, 2), "n")
    
    m2 <- mean(var_name, na.rm = T)
    
    cat("Mean without removing outliers:", 
        round(m1, 2), "n")
    cat("Mean if we remove outliers:", 
        round(m2, 2), "n")
    
    response <- readline(
        prompt = "Do you want to remove outliers and replace with NA? [yes/no]: ")
    if (response == "y" | response == "yes") {
        dt[as.character(substitute(var))] <- invisible(var_name)
        assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
        cat("Outliers successfully removed", "n")
        return(invisible(dt))
    } else {
        cat("Nothing changed", "n")
        return(invisible(var_name))
    }
}

#note outliers and replace with NA - be sure to answer prompt = y!
dat2 = detect_outs(dat, waittime)
dat2 = detect_outs(dat, holiday_prox)
dat2 = detect_outs(dat, total_hrs)
dat2 = detect_outs(dat, cap_lost)

# #features to disregard - no outliers and/or little mean change
# dat2 = detect_outs(dat, holiday_rnk)
# dat2 = detect_outs(dat, temp_mean) 
# dat2 = detect_outs(dat, insesh_fl)
# dat2 = detect_outs(dat, hist_precip)
# dat2 = detect_outs(dat, parades)
# dat2 = detect_outs(dat, fireworks)

#drop NA values from df
dat_cln = drop_na(dat2)

#keep env clean :)
rm(dat, dat2)

#transforom data using norm function
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#normalize features
dat_norm = dat_cln %>%
    mutate_if(is.numeric, normalize)





