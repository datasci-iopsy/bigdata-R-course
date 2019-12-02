#keep env clean
rm(list = ls())

# #load libraries
# library(dplyr)
# library(readr)
# library(ggplot2)
# library(tidyr)
# library(tibble)

# #read in dataset
# dat = read_csv("../../data/final_df.csv", 
#                col_types = cols(
#                    month = col_factor(), 
#                    season = col_factor(), 
#                    extra_morn = col_factor(), 
#                    extra_eve = col_factor(), 
#                    event = col_factor(), 
#                    timeofday = col_factor())
# )

#load df from feature code
source("feature-engineering_code.R")

#df structure
cat("The dataframe has", dim(dat)[1], "rows and", dim(dat)[2], "features")
str(dat)

#summary statistics of df
dat %>% 
    select_if(is.numeric) %>%
    sapply(function(x) summary(x)) %>% 
    round(2)

#### Outlier Analysis ####

#outlier detection
detect_outs <- function(dt, var) { 
    var_name <- eval(substitute(var), eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
    
    boxplot(var_name, main = "With Outliers")
    hist(var_name, main = "With Outliers", xlab = NA, ylab = NA)
    
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    
    cap_func = function(x) {
        quants = quantile(x, probs = c(.25, .75), na.rm = TRUE)
        caps = quantile(x, probs = c(.05, .95), na.rm = TRUE)
        H = 1.5 * IQR(x, na.rm = TRUE)
        
        x[x < (quants[1] - H)] <- caps[1]
        x[x > (quants[2] + H)] <- caps[2]
        
        x
    }
    
    var_name <- ifelse(var_name %in% outlier, cap_func(var_name), var_name)
    
    boxplot(var_name, main = "With Capped Outliers")
    hist(var_name, main = "With Capped Outliers", xlab = NA, ylab = NA)
    title("Outlier Check", outer = TRUE)
    
    na2 <- sum(is.na(var_name))
    
    cat("Mean of the outliers:", 
        round(mo, 2), "n")
    
    m2 <- mean(var_name, na.rm = T)
    
    cat("Mean without removing outliers:", 
        round(m1, 2), "n")
    cat("Mean if we remove outliers:", 
        round(m2, 2), "n")

    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    
    return(invisible(dt))
}

#cap outliers - disregarded features are commented out!
dat2 = detect_outs(dat, waittime)
dat2 = detect_outs(dat, holiday_prox)
#dat2 = detect_outs(dat, holiday_rnk)
dat2 = detect_outs(dat, temp_mean)
#dat2 = detect_outs(dat, insesh_fl)
dat2 = detect_outs(dat, total_hrs)
#dat2 = detect_outs(dat, hist_precip)
dat2 = detect_outs(dat, cap_lost)
#dat2 = detect_outs(dat, parades)
#dat2 = detect_outs(dat, fireworks)

#####

cat_vars = names(dat_fnl)[which(sapply(dat_fnl, is.factor))]
cont_vars = names(dat_fnl[which(sapply(dat_fnl, is.numeric))])

#rename df
dat_fnl = dat2

#keep env clean :)
rm(dat2)

#transforom data using norm function
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#normalize features
dat_norm = dat_fnl %>%
    mutate_if(is.numeric, normalize)

#### Correlation Analysis ####

library(GGally)
library(RColorBrewer)
#library(corrplot)

#fireworks doesn't load corr so it's rm
ggcorr(select_if(dat_fnl, is.numeric)[, -10], method = c("pairwise", "pearson"), 
       digits = 3, label = TRUE, low = "#F21A00", high = "#3B9AB2")

# ggpairs(dat_fnl, columns = c("waittime", "holiday_prox", "holiday_rnk"), 
#         ggplot2::aes(color = timeofday, alpha = .03))
# ggpairs(dat_fnl, columns = c("waittime", "temp_mean", "hist_precip"), 
#         ggplot2::aes(color = timeofday, alpha = .03))
#####

#categorical variables
cat_vars = select_if(dat_fnl, is.factor)

#continuous variables
cont_vars = select_if(dat_fnl, is.numeric)

plotHist <- function(data_in, i) {
    data <- data.frame(x = data_in[[i]])
    p <- ggplot(data = data, aes(x = factor(x))) + 
            stat_count() + 
            xlab(colnames(data_in)[i]) + 
            theme_light() + 
                theme(axis.text.x = element_text(angle = 90, hjust =1))
    return (p)
}

doPlots <- function(data_in, fun, ii, ncol = 3) {
    pp <- list()
    for (i in ii) {
        p <- fun(data_in = data_in, i=i)
        pp <- c(pp, list(p))
    }
    do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i) {
    data <- data.frame(x = data_in[[i]], waittime = data_in$waittime)
    p <- ggplot(data = data) + 
            geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) + 
            xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ', 
                        round(skewness(data_in[[i]], na.rm = TRUE), 2))) + 
            theme_light() 
    return(p)
}

doPlots(cat_vars, fun = plotHist, ii = 1:6, ncol = 3)

#continuous variables
doPlots(cont_vars, fun = plotDen, ii = 1:10, ncol = 2)

#violin plots
dat_fnl %>%
    ggplot(aes(x = season, y = temp_mean, fill = season)) + 
    geom_violin() +
    xlab("class") +
    theme(legend.position="none") +
    xlab("")

                