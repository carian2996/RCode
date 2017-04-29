# Author: Ian Castillo  # Date: 12/31/2016 
# Goal: Create function that calculates the statistical mode(s) of a dataset
modaIan <- function(x){
    counts <- tabulate(match(x, unique(x)))
    list(modas = unique(x)[counts == max(counts)], 
         frec.modas = max(counts), 
         num.datos = length(x))
}

# Author: Arturo Erdely  # Date: 12/31/2016 
# Goal: Create function that calculates the statistical mode(s) of a dataset
modaErdely <- function(datos) {
    modas = sort(unique(datos))[table(datos)==max(table(datos))]
    frec.modas = max(table(datos))
    num.datos = length(datos)
    list(modas = modas, 
         frec.modas = frec.modas, 
         num.datos = num.datos)
}

# Author: Yair Guzmán  # Date: 12/31/2016 
# Goal: Create function that calculates the statistical mode(s) of a dataset
modaGuzman <-function(x){
    t <- sort(table(x),decreasing = T)
    mt <- max(t)
    list(modas = as.numeric(names(t[t>=mt])),
         frecuencias = mt, 
         numero_datos = length(x))
}


# Comparation between functions
set.seed(456)
datos <- sample(1:1000, 10000000, replace = TRUE)

# Check results
sum(sort(modaErdely(datos)$modas) != sort(modaGuzman(datos)$modas))
sum(sort(modaErdely(datos)$modas) != sort(modaIan(datos)$modas))
sum(sort(modaGuzman(datos)$modas) != sort(modaIan(datos)$modas))

# Check process time
system.time(modaErdely(datos))
system.time(modaGuzman(datos))
system.time(modaIan(datos))
