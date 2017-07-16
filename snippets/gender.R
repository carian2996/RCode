# Author: Ian C
# Date: July 16, 2017

# Get the proportion of fe/male members in a Facebook group

# ===== Libraries =====
library(RJSONIO)
library(RCurl)
library(dplyr)
library(ggplot2)
library(gender)

# ===== Parameters =====
token <- 'token'
id_group <- 'group_id'
n_group <- 1500

# ===== Get data =====
link <- paste0("https://graph.facebook.com/v2.9/", 
               id_group, 
               "/members?pretty=0&limit=", 
               n_group, 
               "&access_token=", 
               token)
list_data <- fromJSON(getURL(link))

# ===== Transform data =====
members <- matrix(unlist(list_data$data), ncol = 3, byrow = T)
members[, 1] <- apply(X = as.matrix(members[, 1]), 
                      MARGIN = 2, 
                      FUN = function(x) sapply(strsplit(x, split = ' ', fixed = TRUE), `[`, 1))
members <- as.data.frame(members)
colnames(members) <- c('name', 'id', 'administrator')

# ===== Obtain gender from names =====
gender <- gender(as.character(members$name))
gender$gender <- factor(gender$gender)

# ===== Results =====
summary(gender)
table(gender$gender)/nrow(gender)
ggplot(gender, aes(gender, fill = gender)) + geom_bar()
