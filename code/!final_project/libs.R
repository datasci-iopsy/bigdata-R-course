#libaries needed for analysis

#function to load multiple packages
ipak = function(pkgs) {
    new_pkgs = pkgs[!(pkgs %in% installed.packages()[, "Package"])]
    if (length(new_pkgs))
        install.packages(new_pkgs, dependencies = TRUE)
    sapply(pkgs, require, character.only = TRUE)
    }

#list of packages
pkgs = c(
    # "data.table",
    "tidyverse",
    # "dtplyr",
    # "tidymodels",
    "broom",
    "lubridate",
    "GGally",
    "kableExtra",
    "psych",
    "GPArotation",
    "ggpubr",
    "RColorBrewer",
    "vip"
    )

#load packages
ipak(pkgs)