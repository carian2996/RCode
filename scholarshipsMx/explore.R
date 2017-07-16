# Ian Castillo Rosales
# Mexico City

# Conacyt is the goverment dependency responsible for 
# developing science and technology policies in Mexico.
# You can find more information about it here: 
# LINK: http://www.conacyt.mx/index.php

# Data is provided by: https://datos.gob.mx/
# The dataset contains basic information about scholarships
# given by Conacyt in 2015 and 2016 to study abroad. 

# The data contains:
# - year: 
# - name: 
# - first_surname: 
# - second_surname: 
# - gender: 
# - level_id: 
# - level: 
# - area_id: 
# - area: 
# - institution: 
# - country: 


library(tidyverse)

source('multiplot.R')
source('getting_data.R')

dataset <- get_data()

# ===== Basic data exploration =====
# We are going to work with a tibble object
# Why tibble? R: http://r4ds.had.co.nz/tibbles.html#tibbles-vs.data.frame
is_tibble(dataset) 

# Variables in data
names(dataset)

# Quick view to our variables
summary(dataset)

# ===== Null values =====
apply(dataset, 2, function(x) sum(is.na(x)))

# ===== Variables' distribution =====
multiplot(qplot(year, data = dataset, geom = 'bar'),
          qplot(gender, data = dataset, geom = 'bar'),
          qplot(level, data = dataset, geom = 'bar'),
          qplot(area_id, data = dataset, geom = 'bar'), cols = 2)

# ===== Unique values =====
table(dataset$area_id)
table(dataset$area)
unique(dataset$area)





ggplot(dataset %>% 
           group_by(año, genero, nivel_id) %>%
           summarise(total = n()), aes(x = nivel_id, y = total, fill = genero)) + 
    geom_bar(position = "stack", stat = 'identity') + 
    facet_grid(~ año)

library(scales)
p1 <- ggplot(dataset %>%
                 filter(año == '2015') %>%
                 group_by(genero, area_id) %>%
                 summarise(total = n()), 
             aes(x = area_id, y = total, fill = genero)) + 
    geom_bar(position = "fill",stat = "identity") + 
    ggtitle("Foreign Scholarships by Area and Gender in 2015") + 
    scale_y_continuous(labels = percent_format())

p2 <- ggplot(dataset %>%
                 filter(año == '2016') %>%
                 group_by(genero, area_id) %>%
                 summarise(total = n()), 
             aes(x = area_id, y = total, fill = genero)) + 
    geom_bar(position = "fill",stat = "identity") + 
    ggtitle("Foreign Scholarships by Area and Gender in 2016") + 
    scale_y_continuous(labels = percent_format())

multiplot(p1, p2)

# ===== Transform data =====
areas <- dataset %>%
    group_by(area_id, nivel_id) %>%
    summarise(total = n()) %>%
    spread(key = nivel_id, value = total, fill = 0, sep = '_')

genero <- dataset %>%
    group_by(area_id, genero) %>%
    summarise(total = n()) %>%
    spread(key = genero, value = total, fill = 0, sep = '_')

merge(areas, genero, by = 'area_id')
