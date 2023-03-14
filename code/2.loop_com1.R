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

data[[17]] <- rep("NA", 10) #si può interpretare il dato mancante?

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
x <- append(x, "NA", after = 3)
atto[[7]] <- x

atto[[12]] <- append(atto[[12]], "NA", after = 8)

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
x[[1]] <- append(x[[1]], "dottor Paolo de Rosa e del professor Michele Belletti", after = 3)
x[[1]][[5]] <- "Conferenza delle Regioni e delle Province autonome, UPI, ANPCI, Confcommercio, Confprofessioni, ANCE, OICE, FINCO, Fondazione Italia Digitale, AGDP, CODIRP, ANMIL, Comitato NO riforma Concorsi PA e Save the Children"
x[[1]][[6]] <- "AGE, APEI, ANCODIS, CNAM, ANFIS, SIPED, CIDI e del prof. Eugenio Prosperetti"
x[[1]][[7]] <- "CIGIL, CISL, UIL, SNALS-Confsal e FGU-Federazione Gilda Unams"
x[[1]] <- append(x[[1]], "WWF, ISPRA e COMIECO", after = 7)
x[[1]][[9]] <- "ANIEF, ANP, AND, UDIR, DISAL, ANDIS, ADI, INDIRE, INVALSI, CUN, CNR, CUNSF, Fondazione Agnelli, FIDAE, Consulte provinciali studenti e CNSU"

# Pagina 3: bisogna combinare più elementi nel vettore.

