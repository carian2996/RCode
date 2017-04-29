require(cumstats)

# ===== Testing functions =====

for (i in 1:5){
    
    a <- sample(1:10000000, 5)
    a[a == sample(1:5, 5)] <- NA
    a <- c(100, 0, 10000)
    a <- c(1, 2, 3)
    cat(paste(paste("Vector utilizado: (", toString(a), ")"), 
              paste("Promedio acumulativo: (", toString(cummean(a)), ")"), 
              paste("Promedio geométrico acumulativo: (", toString(cumgmean(a)), ")"), 
              paste("Promedio harmónico acumulativo: (", toString(cumhmean(a)), ")"), 
              paste("Mediana estadística acumulativa: (", toString(cummedian(a)), ")"), 
              paste("Cuantil estadístico acumulativo: (", toString(cumquant(a, 0.5)), ")"), 
              paste("Varianza estadística acumulativa: (", toString(cumvar(a)), ")"), 
              paste("Asimetría estadística acumulativa: (", toString(cumskew(a)), ")"),
              paste("Curtosis estadística acumulativa: (", toString(cumkurt(a)), ")"),
              paste("\n"),
              sep = "\n"))
}

