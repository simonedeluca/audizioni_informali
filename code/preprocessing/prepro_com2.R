library("dplyr")
library("stringr")

c2 <- read.csv("C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione2.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c2$NOMI
nomi <- str_squish(nomi)

nomi[49] <- "Francesco Paorici, Direttore generale dell’Agenzia per l’Italia Digitale"
nomi[74] <- "UNCC (UNIONE NAZIONALE CAMERE CIVILI) ; UNIONE CAMERE PENALI"

words_to_remove <- "Audizione|- Allegato|per la Corte dei conti|Documentazione fornita da rappresentanti del|- PROPOSTE|- SCHEDA D.L. 28/20|- SCHEDA D.L. 29/2020|- ALL.1|- ALL. 2|- ALL. 3|- DOSSIER 0|- DOSSIER 2|- RELAZIONE|- DOCUMENTO CONDIVISO|DOCUMENTO|- Integrazione osservazioni|- SUPPLEMENTO|ALLEGATO 1|ALLEGATO 2|ALLEGATO 3|- analisi tecnica|- scheda riassuntiva|PROTOCOLLO INTESA|MONTELEONE|-2|RELAZIONE ALLE|TESTO DELLE|PROPOSTE|DI MODIFICA|Documento trasmesso\(Allegato\)"
nomi <- gsub(words_to_remove, "", nomi)

nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- str_trim(nomi, side = "both")
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")
nomi <- gsub("; ;", ";", nomi) %>% str_trim(side = "both")

words_to_remove <- " 1| 2| 3| 4"
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

x <- lapply(nomi, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)
x[[92]][1,1] <- "FP CGIL CISL FP UILPA"

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:127) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

n <- c(1,3:5,7:10,12,13,37:41,43:47,50:59,63:67,69,70,72:74,76:81,83:90,92,94,96:100,102:108,110:121,123:126)

for (i in n) {
  x[[i]] <- apply(x[[i]], 2, unique)
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

n <- c(2,6,11,14:36,42,48,49,60:62,68,71,75,82,91,93,95,101,109,122,127)

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

m <- matrix(nrow=127, ncol=1)
for (i in 1:127) {
  m[i,] <- length(x[[i]])
}

c2$times <- m[,1]

new_data <- c2[rep(1:nrow(c2), times = c2$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(x)
new_data[,2] <- single_name

new_data <- subset(new_data, select = -times)
new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)

x <- new_data$DATA
x <- gsub("^\\s","0", x)
x <- str_split_fixed(x, " ", 3)
x <- as.data.frame(x)

x$month <- with(x, ifelse(V2 %in% "Gennaio", 1,
                          ifelse(V2 %in% "Febbraio", 2,
                                 ifelse(V2 %in% "Marzo", 3,
                                        ifelse(V2 %in% "Aprile", 4,
                                               ifelse(V2 %in% "Maggio", 5,
                                                      ifelse(V2 %in% "Giugno", 6,
                                                             ifelse(V2 %in% "Luglio", 7,
                                                                    ifelse(V2 %in% "Agosto", 8,
                                                                           ifelse(V2 %in% "Settembre", 9,
                                                                                  ifelse(V2 %in% "Ottobre", 10,
                                                                                         ifelse(V2 %in% "Novembre", 11,
                                                                                                ifelse(V2 %in% "Dicembre", 12,
                                                                                                       "NA")))))))))))))


date <- paste(x$V1,x$month,x$V3, sep= "-")
new_data <- subset(new_data, select = -DATA)
new_data$DATA <- as.Date(date, format="%d-%m-%y")



