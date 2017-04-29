# Author: Ian C
# Date: Feb 11, 2017

packages <- c("dplyr", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
} else print("All packages installed")

library(dplyr); library(zoo)

n <- 100

data <- data_frame(x = 1:n) %>%
    mutate(a = x*2, 
           b = x^2, 
           c = c(NA, NA, rollapply(b, 3, sum, align = "left")), 
           c = b - lag(c), 
           d = sum(as.numeric(c), na.rm = T))

