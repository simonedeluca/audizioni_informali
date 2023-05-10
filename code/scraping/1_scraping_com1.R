library(rvest)
library(dplyr)
library(stringr)

# L'obiettivo è automatizzare lo scraping. Le info da estrarre per ogni singola audizione
# sono 4: commissione/i coinvolte, audito, atto, data.
# Le audizioni della commissione 1 sono elencate in 17 pagine.

# Creiamo 4 liste (le variabili da estrarre) di lunghezza 17 (numero delle pagine).
# Idealmente, ogni elemento di ciascuna lista sarà un vettore di 10 elementi, ognuno riferito alla singola audizione.

# 1. Estrazione date delle audizioni.

data <- vector(mode = "list", length = 17)

for (page_index in 1:17) {
  link <- paste("https://www.senato.it/Leg18/3572?current_page_29825=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

link <- "https://www.senato.it/Leg18/3572?current_page_29825=17"
page <- read_html(link)
x <- page %>% html_nodes(".data_ora_inizio a") %>% html_text()
x <- gsub("del ", "", x)
data[[17]] <- x

# 2. Estrazione atti su cui vertono le audizioni.

atto <- vector(mode = "list", length = 17)

for (page_index in 1:17) {
  link <- paste("https://www.senato.it/Leg18/3572?current_page_29825=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 2.1 Integrazione dati mancanti

atto[[5]]
x <- atto[[5]]
x <- append(x, "ddl 270 (tutela dei minori e dignità della donna nella comunicazione)", after = 0)
x <- append(x, "ddl 270 (tutela dei minori e dignità della donna nella comunicazione)", after = 5)
atto[[5]] <- x

atto[[7]]
x <- atto[[7]]
x <- append(x, "Atto n. 775 - Passaporto vaccinale", after = 3)
atto[[7]] <- x

atto[[12]] <- append(atto[[12]], "A.S. n. 1144", after = 8)

# 3. Estrazione auditi

# I singoli elementi all'interno di ciascun vettore della lista contengono l'elenco dei nomi degli auditi.
# I nomi possono riferirsi a persone, a enti, o comprendere entrambi.
# Si osservano diverse descrizioni del paragrafo 'Documenti': memorie depositate dagli auditi, memorie
# richieste in forma scritta, altri documenti pervenuti, o nessun sottotitolo. Si può interpretare?

# Per ora, prendiamo il nodo senza descrizioni.

nomi <- vector(mode = "list", length = 17)

for (page_index in 1:17) {
  link <- paste("https://www.senato.it/Leg18/3572?current_page_29825=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 3.1 Pulizia nomi

x <- nomi
for (i in 1:17) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

# 3.2 Integrazione mancanti

# Pagina 1
x[[1]] <- append(x[[1]], "dottor Paolo de Rosa ; professor Michele Belletti", after = 3)
x[[1]][[5]] <- "Conferenza delle Regioni e delle Province autonome ; UPI ; ANPCI ; Confcommercio ; Confprofessioni ; ANCE ; OICE ; FINCO ; Fondazione Italia Digitale ; AGDP ; CODIRP ; ANMIL ; Comitato NO riforma Concorsi PA ; Save the Children"
x[[1]][[6]] <- "AGE ; APEI ; ANCODIS ; CNAM ; ANFIS ; SIPED ; CIDI ; prof. Eugenio Prosperetti"
x[[1]][[7]] <- "CIGIL ; CISL ; UIL ; SNALS-Confsal ; FGU-Federazione Gilda Unams"
x[[1]] <- append(x[[1]], "WWF ; ISPRA ; COMIECO", after = 7)
x[[1]][[9]] <- "ANIEF ; ANP ; AND ; UDIR ; DISAL ; ANDIS ; ADI ; INDIRE ; INVALSI ; CUN ; CNR ; CUNSF ; Fondazione Agnelli ; FIDAE ; Consulte provinciali studenti ; CNSU"

# Pagine in cui bisogna combinare più elementi del vettore. Perdiamo informazione?

# Pagina 3
a <- paste(x[[3]][[5]],x[[3]][[6]],x[[3]][[7]], sep = " ")
x[[3]] <- x[[3]][-c(5,6,7)]
x[[3]] <- append(x[[3]], a, after = 4)

# Pagina 5
a <- paste(x[[5]][[4]],x[[5]][[5]], sep = " ")
x[[5]] <- x[[5]][-c(4,5)]
x[[5]] <- append(x[[5]], a, after = 3)

a <- paste(x[[5]][[7]],x[[5]][[8]], sep = " ")
x[[5]] <- x[[5]][-c(7,8)]
x[[5]] <- append(x[[5]], a, after = 6)

# Pagina 6
a <- paste(x[[6]][[10]],x[[6]][[11]], sep = " ")
x[[6]] <- x[[6]][-c(10,11)]
x[[6]] <- append(x[[6]], a, after = 9)

# Pagina 8
a <- paste(x[[8]][[3]],x[[8]][[4]], sep = " ")
x[[8]] <- x[[8]][-c(3,4)]
x[[8]] <- append(x[[8]], a, after = 2)

a <- paste(x[[8]][[8]],x[[8]][[9]], sep = " ")
x[[8]] <- x[[8]][-c(8,9)]
x[[8]] <- append(x[[8]], a, after = 7)

# Pagina 10
a <- paste(x[[10]][[9]],x[[10]][[10]], sep = " ")
x[[10]] <- x[[10]][-c(9,10)]
x[[10]] <- append(x[[10]], a, after = 8)

# Pagina 11
x[[11]] <- append(x[[11]], "Francesco Paorici, Direttore generale dell'Agenzia per l'Italia Digitale", after = 3)

# Pagina 13
a <- paste(x[[13]][[1]],x[[13]][[2]], sep = " ")
x[[13]] <- x[[13]][-c(1,2)]
x[[13]] <- append(x[[13]], a, after = 0)

# Pagina 14
a <- paste(x[[14]][[2]],x[[14]][[3]], sep = " ")
x[[14]] <- x[[14]][-c(2,3)]
x[[14]] <- append(x[[14]], a, after = 1)

a <- paste(x[[14]][[4]],x[[14]][[5]], sep = " ")
x[[14]] <- x[[14]][-c(4,5)]
x[[14]] <- append(x[[14]], a, after = 3)

# Pagina 15
a <- paste(x[[15]][[6]],x[[15]][[7]], sep = " ")
x[[15]] <- x[[15]][-c(6,7)]
x[[15]] <- append(x[[15]], a, after = 5)

a <- paste(x[[15]][[8]],x[[15]][[9]], sep = " ")
x[[15]] <- x[[15]][-c(8,9)]
x[[15]] <- append(x[[15]], a, after = 7)

a <- paste(x[[15]][[9]],x[[15]][[10]], sep = " ")
x[[15]] <- x[[15]][-c(9,10)]
x[[15]] <- append(x[[15]], a, after = 8)

# Pagina 16
a <- paste(x[[16]][[2]],x[[16]][[3]], sep = " ")
x[[16]] <- x[[16]][-c(2,3)]
x[[16]] <- append(x[[16]], a, after = 1)

a <- paste(x[[16]][[4]],x[[16]][[5]], sep = " ")
x[[16]] <- x[[16]][-c(4,5)]
x[[16]] <- append(x[[16]], a, after = 3)

a <- paste(x[[16]][[5]],x[[16]][[6]], sep = " ")
x[[16]] <- x[[16]][-c(5,6)]
x[[16]] <- append(x[[16]], a, after = 4)

a <- paste(x[[16]][[6]],x[[16]][[7]], sep = " ")
x[[16]] <- x[[16]][-c(6,7)]
x[[16]] <- append(x[[16]], a, after = 5)

# Pagina 17
a <- paste(x[[17]][[1]],x[[17]][[2]], sep = " ")
x[[17]] <- x[[17]][-c(1,2)]
x[[17]] <- append(x[[17]], a, after = 0)

a <- paste(x[[17]][[2]],x[[17]][[3]],x[[17]][[4]], sep = " ")
x[[17]] <- x[[17]][-c(2,3,4)]
x[[17]] <- append(x[[17]], a, after = 1)

# La variabile nomi è completa
nomi <- x

# 4. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 17)

for (page_index in 1:17) {
  link <- paste("https://www.senato.it/Leg18/3572?current_page_29825=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# Integrazione dati mancanti

x <- commissione
x[[1]] <- append(x[[1]], "1 (Aff. costituzionali)", after = 0)
x[[1]] <- append(x[[1]], "1 (Aff. costituzionali)", after = 1)
x[[1]] <- append(x[[1]], "1 (Aff. costituzionali)", after = 9)

x[[2]] <- rep("1 (Aff. costituzionali)", 10)

x[[3]] <- append(x[[3]], "1 (Aff. costituzionali)", after = 0)
a <- rep("1 (Aff. costituzionali)", 7)
x[[3]] <- append(x[[3]], a, after = 3)

a <- rep("1 (Aff. costituzionali)", 6)
x[[4]] <- append(x[[4]], a, after = 0)
a <- rep("1 (Aff. costituzionali)", 2)
x[[4]] <- append(x[[4]], a, after = 8)

a <- rep("1 (Aff. costituzionali)", 9)
x[[5]] <- append(x[[5]], a, after = 0)

a <- rep("1 (Aff. costituzionali)", 8)
x[[6]] <- append(x[[6]], a, after = 2)

x[[7]] <- rep("1 (Aff. costituzionali)", 10)

a <- rep("1 (Aff. costituzionali)", 5)
x[[8]] <- append(x[[8]], a, after = 0)
a <- rep("1 (Aff. costituzionali)", 4)
x[[8]] <- append(x[[8]], a, after = 6)

x[[9]] <- rep("1 (Aff. costituzionali)", 10)

a <- rep("1 (Aff. costituzionali)", 7)
x[[10]] <- append(x[[10]], a, after = 0)

a <- rep("1 (Aff. costituzionali)", 1)
x[[11]] <- append(x[[11]], a, after = 2)
a <- rep("1 (Aff. costituzionali)", 6)
x[[11]] <- append(x[[11]], a, after = 4)

x[[12]] <- rep("1 (Aff. costituzionali)", 10)

x[[13]] <- rep("1 (Aff. costituzionali)", 10)

x[[14]] <- rep("1 (Aff. costituzionali)", 9)

a <- rep("1 (Aff. costituzionali)", 7)
x[[15]] <- append(x[[15]], a, after = 0)
a <- rep("1 (Aff. costituzionali)", 1)
x[[15]] <- append(x[[15]], a, after = 9)

x[[16]] <- rep("1 (Aff. costituzionali)", 10)

a <- rep("1 (Aff. costituzionali)", 1)
x[[17]] <- append(x[[17]], a, after = 3)
a <- rep("1 (Aff. costituzionali)", 2)
x[[17]] <- append(x[[17]], a, after = 7)

# La variabile commissione è completa
commissione <- x
rm(x)

# Abbiamo 4 liste (atto, commissione, data, nomi) di lunghezza uguale contenenti vettori della stessa lunghezza.

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 1
c1 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)
