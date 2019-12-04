rm(list = ls()) #keep env clean

#uncomment lines below if one prefers NOT to source scripts!
# #load libraries
# library(tidyverse)
# library(data.table)

# #read in dataset - feature attributes must be specified from csv files...
# dat = read_csv("../../data/final_df.csv", 
#                col_types = cols(
#                    month = col_factor(), 
#                    season = col_factor(), 
#                    extra_morn = col_factor(), 
#                    extra_eve = col_factor(), 
#                    event = col_factor(), 
#                    timeofday = col_factor())
# )

#load feature enginnering script
source("feature-engineering_code.R")

#df structure
cat("The dataframe has", dim(dat)[1], "rows and", dim(dat)[2], "features")
str(dat)

#summary statistics of df
select_if(dat, is.numeric) %>%
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

#cap outliers - disregarded features are commented out
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

#rename df w/ capped features
dat_fnl = dat2

#keep env clean :)
rm(dat2)

#transforom data using norm function
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#normalize all numeric features
dat_norm = dat_fnl %>%
    mutate_if(is.numeric, normalize)

#### Correlation Analysis ####

shhh(GGally)
shhh(RColorBrewer)

#disregarded features: fireworks
ggcorr(select_if(dat_fnl, is.numeric)[, -10], 
       method = c("pairwise", "pearson"), 
       label = TRUE, label_size = 5,
       low = "#F21A00", high = "#3B9AB2", 
       hjust = .75, 
       size = 5, 
       layout.exp = 1,
       ) +
    ggplot2::labs(
        title = "Correlations of Selected Features", 
        subtitle = "Disney\'s Pirates of the Carribean"
        ) + 
    ggplot2::theme(
        plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 14)
    )

#####

#### EDA Viz ####
library(cowplot)
library(gridExtra)

#create dfs with categorical & continuous variables
sep_vars = list(
    cat_vars = dat_fnl[which(sapply(dat_fnl, is.factor))], 
    cont_vars = dat_fnl[which(sapply(dat_fnl, is.numeric))]
)

#create a list with freq table for each feature
freqList = lapply(sep_vars[["cat_vars"]], 
                  function(x) {
                      
                      my_lst = data.frame(table(x))
                      names(my_lst) = c("fct", "n")
                      
                      return(my_lst) 
                        }
                    )
freqList

#function to automate lolipop plots
plotLoli <- function(data_in, i, ...) {
    args = list(...)
    
    data <- data_in[[i]]  
    
    p <- ggplot(data = data, aes(x = fct, y = n, label = n)) + 
            geom_point(size = 3, color = "red", alpha = .6, shape = 20, 
                       stroke = 2) + 
            geom_segment(aes(x = fct, xend = fct, y = 0, yend = n), 
                         color = "black") + 
            coord_flip() + 
            theme_minimal() + 
            labs(title = " ", 
                 x = str_to_title(i), 
                 y = "Total Count") +
        #geom_text(nudge_x = .45) +
            theme(panel.grid.major.y = element_blank(), 
                  panel.border = element_blank(), 
                  axis.ticks.y = element_blank(), 
                  legend.position = 'none')
    
    return(p)
}

#list of lolipop plots
loliList = list(
    p_month = plotLoli(data_in = freqList, i = "month"),
    p_extmorn = plotLoli(data_in = freqList, i = "extra_morn"), 
    p_time = plotLoli(data_in = freqList, i = "timeofday"),
    p_season = plotLoli(data_in = freqList, i = "season"), 
    p_exteve = plotLoli(data_in = freqList, i = "extra_eve"),
    p_event = plotLoli(data_in = freqList, i = "event")
    )

# title of graphs
title = ggdraw() + 
    draw_label(
        "Frequency Tables of Categorical Features",
        fontface = "bold"
        )
        
#categorical plots with titles! - save both separately
plot_grid(title, plotlist = loliList[c(1, 4, 3)], ncol = 2, 
          rel_heights = c(.1, .1))
plot_grid(title, plotlist = loliList[c(2, 5, 6)], ncol = 2, 
          rel_heights = c(.1, .1))

#density plots for continuous features
plotDen <- function(data_in, i) {
    data <- data.frame(x = data_in[[i]], waittime = data_in$waittime)
    
    p <- ggplot(data = data) + 
            geom_line(aes(x = x), stat = 'density', size = 1, alpha = 1.0) + 
        xlab(paste0(
            (colnames(data_in)[i]), '\n', 'Skewness: ', 
                    round(skewness(data_in[[i]], na.rm = TRUE), 2))
            ) + 
        theme_minimal() 
    
    return(p)
}

#arrange graphs for continuous features
doPlots <- function(data_in, fun, ii, ncol = 3) {
    pp <- list()
    
    for (i in ii) {
        p <- fun(data_in = data_in, i = i)
        pp <- c(pp, list(p))
    }
    
    do.call("grid.arrange", c(pp, ncol = ncol))
}

#density plots!
doPlots(sep_vars[["cont_vars"]], fun = plotDen, ii = 2:10, ncol = 3)

##plot the waittimes for each time of day by c(month, season) possible facets?

#histogram of response variable!
ggplot(dat_fnl, aes(x = waittime)) + 
    geom_histogram(col = 'black', fill = "darkgray", bins = 40, binwidth = 5) + 
    theme_minimal() + 
    scale_x_continuous()