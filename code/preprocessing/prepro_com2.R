library("dplyr")
library("stringr")

c2 <- read.csv("C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione2.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c2$NOMI
nomi <- str_squish(nomi)

nomi[49] <- "Francesco Paorici, Direttore generale dell’Agenzia per l’Italia Digitale"
nomi[74] <- "UNCC (UNIONE NAZIONALE CAMERE CIVILI) ; UNIONE CAMERE PENALI"

words_to_remove <- "Audizione|- Allegato|per la Corte dei conti|Documentazione fornita da rappresentanti del|- PROPOSTE|- SCHEDA D.L. 28/20|- SCHEDA D.L. 29/2020|- ALL.1|- ALL. 2|- ALL. 3|- DOSSIER 0|- DOSSIER 2|- RELAZIONE|- DOCUMENTO CONDIVISO|DOCUMENTO|- Integrazione osservazioni|- SUPPLEMENTO|ALLEGATO 1|ALLEGATO 2|ALLEGATO 3|- analisi tecnica|- scheda riassuntiva|PROTOCOLLO INTESA|MONTELEONE|-2|RELAZIONE ALLE|TESTO DELLE|PROPOSTE|DI MODIFICA|Documento trasmesso"
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\([^()]*\\Allegato[^()]*\\)", "", nomi)

nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- str_trim(nomi, side = "both")
nomi <- gsub("; ;", ";", nomi) %>% str_trim(side = "both")
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

words_to_remove <- " 1| 2| 3| 4"
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

x <- lapply(nomi, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)

x[[4]][6,1] <- "Associazione Magistrati Tributari AMT"
x[[55]][4,1] <- "PROF. Paolo DE CARLI - Associazione Nonni 2.0"
x[[69]][1,1] <- "Francesco FIMMANO'"
x[[76]][3,1] <- "ASSOGOT - COGITA - UMOT"
x[[76]][4,1] <- "CONAMO Coordinamento Nazionale Onorari d'Italia"
x[[76]][7,1] <- "CONAMO Coordinamento Nazionale Onorari d'Italia"
x[[76]][8,1] <- "ANGOT - AGOT - OUMO MOU"
x[[79]][1,1] <- "PRO TEST ITALIA - PATTO TRASVERSALE PER LA SCIENZA"
x[[79]][10,1] <- "LEGAMBIENTE - LIPU - WWF"
x[[82]][1,1] <- "CONAMO Coordinamento Nazionale Onorari d'Italia"
x[[82]][3,1] <- "COGITA, Raffaele Franza"
x[[82]][5,1] <- "AVV. MARIA BARBARA CERMINARA, OUMO MOU"
x[[82]][7,1] <- "AVV. MARIA BARBARA CERMINARA, OUMO MOU"
x[[90]][1,1] <- "FP CGIL CISL FP UILPA"
x[[111]][8,1] <- "AVV. SPOLETINI - ASSOCIAZIONE \"LA TUTELA DEI DIRITTI\""

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:125) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

n <- c(1,3:5,7:10,12,13,37:41,43:47,50:59,63:67,69:79,81:88,90,92,94:98,100:106,108:119,121:124)

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

n <- c(2,6,11,14:36,42,48,49,60:62,68,80,89,91,93,99,107,120,125)

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

m <- matrix(nrow=125, ncol=1)
for (i in 1:125) {
  m[i,] <- length(x[[i]])
}

c2$times <- m[,1]

new_data <- c2[rep(1:nrow(c2), times = c2$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(x)

# correzioni manuali segni di punteggiatura
p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(1:6,8:15,23,24,26,28,37,41,42,48,54,55,61,62)
for (i in y) {
  x[i] <- gsub("-", ",", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(2,5,7,9,15:34)

for (i in y) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

single_name <- gsub("\\;$", "", single_name) %>% str_squish()

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

write.csv(new_data, "C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/C2.csv", row.names = FALSE)
