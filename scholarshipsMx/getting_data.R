library(reader)
library(tidyr)

get_data <- function() {
    # ===== Retrive data =====
    schos2015 <- read_csv("http://datosabiertos.conacyt.gob.mx/publico/DATOS%20ABIERTOS/BECAS%20EXTRANJERAS/Becas%20Nuevas%20al%20Extranjero%20por%20Pais%20de%20Destino.csv", 
                          col_names = c("first_surname", "second_surname", "name", "area", 
                                        "level", "institution", "country", "gender"), 
                          locale = locale(encoding = "latin1"),
                          skip = 1)
    
    schos2016 <- read_csv("http://datosabiertos.conacyt.gob.mx/publico/DATOS%20ABIERTOS/BECAS%20EXTRANJERAS/Becas%20Nuevas%20al%20Extrajero%20por%20Pa%C3%ADs%20de%20Destino%202015.csv", 
                          col_names = c("first_surname", "second_surname", "name", "area", 
                                        "level", "institution", "country", "gender"), 
                          locale = locale(encoding = "latin1"),
                          skip = 1)
    
    # ===== Transform data =====
    schos2015['year'] <- '2015'
    schos2016['year'] <- '2016'
    
    dataset <- rbind(schos2015, schos2016)
    
    dataset <- dataset %>% 
        separate(col = area, into = c("area_id", "area"), 
                 sep = ". ", remove = T, extra = "merge") %>%
        separate(col = level, into = c("level_id", "level"), 
                 sep = ". ", remove = T, extra = "merge")
    
    dataset <- dataset %>% mutate(
        area_id = factor(area_id), 
        level_id = factor(level_id), 
        gender = factor(gender), 
        year = factor(year)) %>% 
        na.omit() %>%
        select(year, name, first_surname, second_surname, 
               gender, level_id, level, area_id, area,
               institution, country)
    
    rm(schos2015, schos2016)
    
    return(dataset)
}
