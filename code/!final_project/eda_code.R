rm(list = ls()) #keep env clean

#run the feature enginnering script
source("feature-engineering_code.R")

#df structure
cat("The dataframe has", dim(dat)[1], "rows and", dim(dat)[2], "features")
glimpse(dat)

#summary statistics of df
select_if(dat, is.numeric) %>%
    sapply(function(x) summary(x)) %>% 
    round(2)

#### Outlier Analysis ####

#create a df to handle updated values
dat_cap = dat

#outlier detection
cappedOutliers <- function(dt, var, gtitle = NULL) {
    gtitle = as.character(gtitle)
    var_name <- eval(substitute(var), eval(dt))
    
    tot <- sum(!is.na(var_name))
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
        
        return(x)
    }
    
    var_name <- ifelse(var_name %in% outlier, cap_func(var_name), var_name)
    
    boxplot(var_name, main = "With Capped Outliers")
    hist(var_name, main = "With Capped Outliers", xlab = NA, ylab = NA)
    
    title(paste("Outlier Check: ", gtitle), outer = TRUE)
    
    na2 <- sum(is.na(var_name))
    
    #message("Outliers Identified: ", na2 - na1, " from ", tot, " Observations")
    #message("Proportion (%) of Outliers: ", round((na2 - na1) / tot * 100), 2, "%")
    message("Mean of the Outliers: ", round(mo, 2))
    
    m2 <- mean(var_name, na.rm = T)
    
    message("Mean without Capping Outliers: ", round(m1, 2))
    message("Mean if Outliers are Capped: ", round(m2, 2))
    
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully capped", "\n")
        
    return(invisible(dt))
}

# testList = list()
# testList = lapply(dat_cap, boxplot)

#cap outliers - disregarded features are commented out
cappedOutliers(dat_cap, waittime, "Waittime")
cappedOutliers(dat_cap, holiday_prox, "Holiday Proximity")
cappedOutliers(dat_cap, temp_mean, "Mean Temperature")
cappedOutliers(dat_cap,temp_max , "Max Temperature")
cappedOutliers(dat_cap, temp_min, "Min Temperature")
cappedOutliers(dat_cap, mk_total_hrs, "MK Total Hrs")
cappedOutliers(dat_cap, ep_total_hrs, "EP Total Hrs")
cappedOutliers(dat_cap, hs_total_hrs, "HS Total Hrs")
cappedOutliers(dat_cap, ak_total_hrs, "AK Total Hrs")
cappedOutliers(dat_cap, mk_cap_lost, "MK Capacity Lost")
cappedOutliers(dat_cap, ep_cap_lost, "EP Capacity Lost")
cappedOutliers(dat_cap, hs_cap_lost, "HS Capacity Lost")
cappedOutliers(dat_cap, ak_cap_lost, "AK Capacity Lost")
#####

#summary statistics of capped df
select_if(dat_cap, is.numeric) %>%
    sapply(function(x) summary(x)) %>% 
    round(2)

#### Correlation Analysis ####

shhh(GGally)
library(corrplot)
shhh(RColorBrewer)

corrs <- cor(select_if(dat_cap, is.numeric), use="pairwise.complete.obs")

cor_sorted <- as.matrix(sort(corrs[,'waittime'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
corrs <- corrs[CorHigh, CorHigh]

#disregarded features: fireworks
corrplot(corrs, type = "lower", diag = FALSE, method = "circle", tl.pos = "ld", 
         tl.srt = 45, tl.col = "black", tl.offset = .5, tl.cex = .75
       # label = TRUE, label_size = 3,
       # low = "#F21A00", high = "#3B9AB2",
       # hjust = .65,
       # size = 3,
       # layout.exp = 2,
       # angle = 300
       )
    # works with GGally only
    # ggplot2::labs(
    #     title = "Correlations of Selected Features", 
    #     subtitle = "Disney\'s Pirates of the Carribean"
    #     ) + 
    # ggplot2::theme(
    #     plot.title = element_text(size = 20, face = "bold"), 
    #     plot.subtitle = element_text(size = 14)
    # )

#####

#### EDA Viz ####
shhh(cowplot) #masks libridate::stamp
shhh(gridExtra) #masks 'dplyr::combine

#create dfs with categorical & continuous variables
sep_vars = list(
    cat_vars = dat_cap[which(sapply(dat_cap, is.factor))], 
    cont_vars = dat_cap[which(sapply(dat_cap, is.numeric))]
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
loliPlots <- function(data_in, i, ...) {
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
    p_day = loliPlots(data_in = freqList, i = "day"),
    p_month = loliPlots(data_in = freqList, i = "month"),
    p_season = loliPlots(data_in = freqList, i = "season"),
    p_mkevent = loliPlots(data_in = freqList, i = "mk_event"),
    p_epevent = loliPlots(data_in = freqList, i = "ep_event"),
    p_hsevent = loliPlots(data_in = freqList, i = "hs_event"),
    p_akevent = loliPlots(data_in = freqList, i = "ak_event"),
    p_time = loliPlots(data_in = freqList, i = "timeofday"),
    p_event = loliPlots(data_in = freqList, i = "event")
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
plot_grid(title, plotlist = loliList[c(7, 9, 8)], ncol = 2, 
          rel_heights = c(.1, .1))

#density plots for continuous features
library(e1071)

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
doPlots(sep_vars[["cont_vars"]], fun = plotDen, ii = 2:8, ncol = 3)
doPlots(sep_vars[["cont_vars"]], fun = plotDen, ii = 9:15, ncol = 3)
doPlots(sep_vars[["cont_vars"]], fun = plotDen, ii = 16:22, ncol = 3)
doPlots(sep_vars[["cont_vars"]], fun = plotDen, ii = 23:28, ncol = 3)

##plot the waittimes for each time of day by c(month, season) possible facets?
ggplot(dat_cap, aes(x = month, y = waittime, fill = month)) + 
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(~timeofday) + 
    labs(
        title = "Monthly Count by Time of Day", 
        caption = "Good to be a night owl!"
    )

#histogram of response variable!
ggplot(dat_cap, aes(x = waittime)) + 
    geom_histogram(col = 'black', fill = "darkgray", binwidth = 4.5) + 
    theme_minimal() + 
    scale_x_continuous() + 
    labs(
        title = "Histogram of Response Variable", 
        subtitle = "Wait Times", 
        caption = "Outliers are capped!"
    )

#transforom data using norm function
normalize = function(x) {
    return(
        (x - min(x)) / (max(x) - min(x))
    )
}

#normalize all numeric features
dat_norm = dat_cap %>%
    mutate_if(is.numeric, normalize)
    # add_column("date" = dat[, 1], .before = 1)

#write out the final normalized df
write_csv(dat_norm, path = "../../data/finalnorm_df.csv")

rm(list=setdiff(ls(), "dat_norm"))

