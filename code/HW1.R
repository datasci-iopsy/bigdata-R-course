##HW3 - Sampling Distributions


#keep packages quiet!
shhh = function(...) {
  suppressPackageStartupMessages(base::library(...))
}

#create a function
corrDat = NULL

#create a function
samp_distFunc = function(n_size = NULL){
    shhh(psych)
    
    corrDat = NULL
    
    for (i in 1:1000){        
        if (is.double(n_size) == T){
            x <- rnorm(mean = 0, sd = 1, n = n_size)
            y <- rnorm(mean = 0, sd = 1, n = n_size)
        } else {
            stop("Not a number!")
        }
        
        dat = data.frame(x, y)
        r = with(dat, cor(x, y))
        
        corrDat = matrix(append(corrDat, r))
        #colnames(corrDat) <- "corrs"
    }
    return(corrDat)
}

#create separate dfs
corrDat20 = samp_distFunc(n_size = 20)
corrDat100 = samp_distFunc(n_size = 100)
corrDat500 = samp_distFunc(n_size = 500)

#combind all corr dfs
corrDatdf = cbind.data.frame(corrDat20, corrDat100, corrDat500)

#grid parameters
par(mfrow = c(3, 1))

#histograms
hist(corrDat20, main = "N = 20", breaks = seq(-.8, .8, .05), col = "red")
hist(corrDat100, main = "N = 100", breaks = seq(-.8, .8, .05), col = "yellow")
hist(corrDat500, main = "N = 500", breaks = seq(-.8, .8, .05), col = "green")

#one liner - not as pretty!
#lapply(list(corrDat20, corrDat100, corrDat500), hist)[1]["histogram"]

shhh(tidyverse)
shhh(eeptools) #used for percent symbols

table1 = data.frame(x = rep(seq(.1, .5, by = .2), 3), 
                    y = rep(c(20, 100, 500), each = 3), 
                    z = rep(NA, 9))

#fill in the z column with appropriate values
table1[1, 3] = nrow(dplyr::filter(data.frame(corrDat20), corrDat20 > .1)) / 1000 * 100
table1[2, 3] = nrow(dplyr::filter(data.frame(corrDat20), corrDat20 > .3)) / 1000 * 100
table1[3, 3] = nrow(dplyr::filter(data.frame(corrDat20), corrDat20 > .5)) / 1000 * 100
table1[4, 3] = nrow(dplyr::filter(data.frame(corrDat100), corrDat100 > .1)) / 1000 * 100
table1[5, 3] = nrow(dplyr::filter(data.frame(corrDat100), corrDat100 > .3)) / 1000 * 100
table1[6, 3] = nrow(dplyr::filter(data.frame(corrDat100), corrDat100 > .5)) / 1000 * 100
table1[7, 3] = nrow(dplyr::filter(data.frame(corrDat500), corrDat500 > .1)) / 1000 * 100
table1[8, 3] = nrow(dplyr::filter(data.frame(corrDat500), corrDat500 > .3)) / 1000 * 100
table1[9, 3] = nrow(dplyr::filter(data.frame(corrDat500), corrDat500 > .5)) / 1000 * 100

table1 %>%
    mutate(z = as.character(z), 
           z = paste(z, "%", sep = "")) %>%
    rename("Corrs >" = x, "Sample size" = y, "% of sample" = z)

#descriptive stats
desc_stats = lapply(list(corrDat20, corrDat100, corrDat500), describe)
#desc_stats

table2 = data.frame("Sample_size" = c(20, 100, 500), 
                    "Mean_1000_corrs" = c(mean(corrDat20), mean(corrDat100), 
                                          mean(corrDat500)), 
                    "SD_1000_corrs" = c(sd(corrDat20), sd(corrDat100), sd(corrDat500))
                   )
round(table2, 4)

#Note:* Everyone in class will get different (but similar) answers given that we are all using different random numbers. 