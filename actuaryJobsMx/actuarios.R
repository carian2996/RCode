# ===== Cargamos los paquetes necesarios =====

packages <- c("ggplot2", "dplyr", "stringr", "wordcloud", "RColorBrewer", "devtools",
              "tm", "SnowballC", "devtools", "Hmisc", "cluster", "fpc", "knitr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
} else print("Todos los paquetes instalados")

# Si no tienes instalado el wrapper para conectarte a la API de Indeed 
# necesitarás instalar jobbR con la siguiente linea:
devtools::install_github("dashee87/jobbR")
devtools::install_github('diegovalle/mxmaps')

lapply(packages, require, character.only = TRUE)
require(jobbR)
require(mxmaps)

load('actuaryJobsMx/dataset.RData')

# collecting data scientist jobs from the Indeed API
actuarios <- jobSearch(publisher = "7697745275879740",
                       query = "actuaria",
                       country = 'mx',
                       all = TRUE)

salarios <- lapply(actuarios$results.url,
                   function(x) getSalary(x, "USD"))
salarios <- do.call(rbind, salarios)

actuarios <- cbind(actuarios, salarios)


# Limpieza de datos, yuju!
actuarios <- actuarios[, -(1:10)]
colnames(actuarios) <- gsub('results.', '', colnames(actuarios))

# actuarios <- actuarios %>% 
#     mutate(jobtitle = tolower(jobtitle)) %>% 
#     mutate(jobtitle = gsub('[[:punct:]]+', ' ', jobtitle)) %>% 
#     mutate(jobtitle = gsub('[[:digit:]]+', ' ', jobtitle)) %>% 
#     mutate(jobtitle = iconv(jobtitle, to='ASCII//TRANSLIT')) %>%
#     mutate(jobtitle = gsub('[[:punct:]]+', '', jobtitle))
# 
actuarios <- actuarios %>%
    mutate(company = tolower(company)) %>%
    mutate(company = gsub('[[:punct:]]+', ' ', company)) %>%
    mutate(company = gsub('[[:digit:]]+', ' ', company)) %>%
    mutate(company = iconv(company, to='ASCII//TRANSLIT')) %>%
    mutate(company = gsub('[[:punct:]]+', '', company))

actuarios <- actuarios[-c(3, 5, 6, 11, 16)]

compañias <- sort(table(actuarios$company), decreasing = T)
dfCompañias <- data.frame(compañias[compañias / sum(compañias) > 0.0075])
colnames(dfCompañias) <- c('compañia', 'puestos')
dfCompañias$compañia <- substr(dfCompañias$compañia, 1, 15)
dfCompañias <- dfCompañias[dfCompañias$compañia != '', ]
dfCompañias$compañia <- factor(dfCompañias$compañia, 
                               levels=dfCompañias$compañia)

ggplot(dfCompañias, aes(x = compañia, y = puestos)) + 
    geom_bar(stat = "identity", fill="#1756A9") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.text.x = element_text(angle = 40)) +
    labs(title = "Empresas con Mayor Demanda de Actuarios", 
         x = NULL, y = "Total de Solicitudes") + theme(plot.title = element_text(size = 15, 
    face = "bold"))


data("df_mxstate")
mapa <- data.frame(table(actuarios$state))
colnames(mapa) <- c('estado', 'puestos')
mapa <- mapa[mapa$estado != '', ]
mapa$estado <- c('BC', 'CHIH',  'CDMX', 'DGO', 'GTO', 'JAL', 'MEX', 'MICH', 'NL', 
                 'PUE', 'QRO', 'QROO', 'SIN', 'SON', 'TAB', 'TAM', 'YUC')

mapa <- merge(x=df_mxstate,y=mapa, by.x="state_abbr", by.y="estado", all.x = TRUE)

mapa$value <- mapa$puestos
mapa$value[is.na(mapa$value)] <- 0
mxstate_choropleth(mapa, title = "Trabajos ofrecidos en México") 


# Load and prepare the corpus
reader <- readTabular(mapping = list(id = "jobkey", content = "snippet"))
corpus <- Corpus(DataframeSource(actuarios), 
                 readerControl = list(reader = reader, language = 'es'))

head(summary(corpus))
inspect(corpus[1])

# Preprocessing the text
skipWords <- function(x) removeWords(x, stopwords("spanish"))
funcs <- list(content_transformer(tolower), 
              removePunctuation, 
              removeNumbers, 
              stripWhitespace, 
              skipWords)
c_corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
for (j in seq(c_corpus)) c_corpus[[j]] <- gsub("bactuaríab", "actuaría", c_corpus[[j]])
for (j in seq(c_corpus)) c_corpus[[j]] <- gsub("bactuariab", "actuaría", c_corpus[[j]])
c_corpus <- tm_map(c_corpus, stemDocument, language = 'es')
c_corpus <- tm_map(c_corpus, PlainTextDocument) 

for (i in 1:length(c_corpus)) meta(c_corpus[[i]], 'id') <- actuarios$jobkey[i]
for (i in 1:length(c_corpus)) meta(c_corpus[[i]], 'author') <- actuarios$company[i]
for (i in 1:length(c_corpus)) meta(c_corpus[[i]], 'datetimestamp') <- actuarios$date[i]
for (i in 1:length(c_corpus)) meta(c_corpus[[i]], 'heading') <- actuarios$jobtitle[i]
for (i in 1:length(c_corpus)) meta(c_corpus[[i]], 'language') <- 'spanish'

c_corpus[[300]]['content']

# Stage the data
(dtm <- DocumentTermMatrix(c_corpus))
names(dtm)
names(dtm[['dimnames']])
dtm[['dimnames']][['Terms']][15:20]
as.matrix(dtm)[1, ][as.matrix(dtm)[1, ] > 0]

(tdm <- TermDocumentMatrix(c_corpus))
names(tdm)
names(tdm[['dimnames']])
tdm[['dimnames']][['Terms']][15:20]
as.matrix(tdm)[, 1][as.matrix(tdm)[, 1] > 0]

(dtms <- removeSparseTerms(dtm, 0.93))
inspect(dtms[1:5, ])

findFreqTerms(dtm, lowfreq = 40) 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq)
table(freq)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

p <- ggplot(subset(wf, freq > 40), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  

findAssocs(x = dtm, terms = "actuaria", corlimit = 0.2)
findAssocs(x = dtm, terms = "titulado", corlimit = 0.15)
findAssocs(x = dtm, terms = "gerente", corlimit = 0.3)
findAssocs(x = dtm, terms = "requisitos", corlimit = 0.2)

set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, 
          scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   


(dtmss <- removeSparseTerms(dtm, 0.9))

d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit 

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red") # draw dendogram with red borders around the 5 clusters   

d <- dist(t(dtmss), method = "euclidian")
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


# filtering out jobs with no advertised salary or retaining those with annual salaries
actSalarios <- actuarios[!is.na(actuarios$minSal), ]
actSalarios

# plot annual salary cumulative distributions
actSalarios$avg_salario <- mapply(function(x,y){(x+y)/2}, actSalarios$minSal, actSalarios$maxSal)
actSalarios$type <- "actuario"


source(file = "https://goo.gl/UUyEzD")
outlierKD(actSalarios, avg_salario)

summary(actSalarios$avg_salario)

ggplot(actSalarios, aes(avg_salario)) + stat_ecdf() + 
    labs(title ="Actuaria: Salario Promedio en México", 
         x = "Salario Mensual (MXN)", y = "Proporción Acumulada") + 
    theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1)) +labs(title = "Salario Promedio del Actuario en México")