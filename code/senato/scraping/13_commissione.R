library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/4314?current_page_42324=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[1]] <- append(x[[1]], "Audizioni AG n. 383 (fauna selvatica ed esotica)", after = 2)
x[[14]] <- append(x[[14]], "Doc. XXVII, n. 18 (Piano nazionale di ripresa e resilienza)", after = 2)
x[[16]] <- append(x[[16]], "ddl 1131 (Rigenerazione urbana)", after = 0)
x[[17]] <- append(x[[17]], "AS 1302 ; 1981 ; 1943 ; 970 ; 1131", after = 1)
x[[24]] <- append(x[[24]], "ddl	497", after = 8)
x[[25]] <- append(x[[25]], "AS 149, 497, 757 e 776 (isole minori)", after = 3)
x[[25]] <- append(x[[25]], "Ilva", after = 4)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/4314?current_page_42324=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

a <- c("22 Settembre 2021", "30 Giugno 2022")
data[[25]] <- append(data[[25]], a, after = 5)

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/4314?current_page_42324=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("13 (Ambiente)", 7)
x[[1]] <- append(x[[1]], a, after = 3)

a <- rep("13 (Ambiente)", 2)
x[[3]] <- append(x[[3]], a, after = 0)
x[[3]] <- append(x[[3]], "13 (Ambiente)", after = 3)

a <- rep("13 (Ambiente)", 2)
x[[12]] <- append(x[[12]], a, after = 8)

a <- rep("13 (Ambiente)", 9)
x[[20]] <- append(x[[20]], a, after = 0)

x[[21]] <- append(x[[21]], "13 (Ambiente)", after = 0)

a <- rep("13 (Ambiente)", 4)
x[[23]] <- append(x[[23]], a, after = 6)

a <- rep("13 (Ambiente)", 4)
x[[25]] <- append(x[[25]], a, after = 0)
x[[25]] <- append(x[[25]], "13 (Ambiente)", after = 6)

a <- rep("13 (Ambiente)", 10)
z <- c(2,13:19,24)
for (i in z) {
  x[[i]] <- a
}

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/4314?current_page_42324=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 4.1 Pulizia nomi

x <- nomi

for (i in 1:25) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- paste(x[[1]][[3]],x[[1]][[4]], sep = " ")
x[[1]] <- x[[1]][-c(3,4)]
x[[1]] <- append(x[[1]], a, after = 2)
x[[1]][2] <- "FIDC ; ASSALCO ; FOI ; LAV ; WWF-Lipu-Legambiente"

a <- paste(x[[5]][[5]],x[[5]][[6]], sep = " ")
x[[5]] <- x[[5]][-c(5,6)]
x[[5]] <- append(x[[5]], a, after = 4)

x[[5]][5] <- "Federazione ANIMA"

a <- paste(x[[9]][[5]],x[[9]][[6]], sep = " ")
x[[9]] <- x[[9]][-c(6,7)]
x[[9]] <- append(x[[9]], a, after = 5)
x[[9]][6] <- "Assobibe ; Unionplast ; Federazione carta e grafica"

x[[11]][3] <- "RSE ; Elettricità Futura ; Italia Solare ; ARERA"
x[[11]][4] <- "RSE ; Elettricità Futura ; Italia Solare ; ARERA"
a <- "Legambiente - Transport & Environment; Legambiente; Greenpeace - WWF - Legambiente - Kyoto Club - Transport & Environment"
x[[11]][5] <- a
x[[11]] <- append(x[[11]], a, after = 5)

x[[14]][4] <- "Prof. Boero, Università degli Studi di Napoli Federico II"

x[[16]][4] <- "FAI-INU-Slow Food Italia-WWF"

x[[17]][8] <- "Assoporti"
x[[17]][9] <- "Ispra"

a <- paste(x[[20]][[4]],x[[20]][[5]], sep = " ")
x[[20]] <- x[[20]][-c(4,5)]
x[[20]] <- append(x[[20]], a, after = 3)
x[[20]][8] <- "Assobioplastiche"
x[[20]][9] <- "Unionplast"

a <- paste(x[[21]][[5]],x[[21]][[6]], sep = " ")
x[[21]] <- x[[21]][-c(5,6)]
x[[21]] <- append(x[[21]], a, after = 4)
a <- paste(x[[21]][[9]],x[[21]][[10]], sep = " ")
x[[21]] <- x[[21]][-c(9,10)]
x[[21]] <- append(x[[21]], a, after = 8)
a <- paste(x[[21]][[10]],x[[21]][[11]], sep = " ")
x[[21]] <- x[[21]][-c(10,11)]
x[[21]] <- append(x[[21]], a, after = 9)

x[[21]][3] <- "Confindustria ; Istituto Nazionale Urbanistica ; Coldiretti ; Confagricoltura ; Copagri"
x[[21]][4] <- "Corte dei conti ; ANCE ; Rete delle Professioni Tecniche ; ANCI ; UPI ; Conferenza delle regioni e delle province autonome ; INAIL"
x[[21]][5] <- "Confindustria ; CNA Nazionale ; Confcommercio ; Finco ; Alleanza delle cooperative italiane ; Assoimmobiliare ; Confedilizia ; Confartigianato; FILLEA-CGIL - FILCA-CISL - FENEAL-UIL ; CGIL-CISL-UIL"
x[[21]][6] <- "FILLEA-CGIL ; prof. Paolo Maddalena, Vice Presidente emerito della Corte Costituzionale ; Forum nazionale 'salviamo il paesaggio difendiamo i territori'"
x[[21]][9] <- "Prof.ssa Adriana Galderisi, Professore associato di Tecnica e Pianificazione Urbanistica dell'Università degli Studi della Campania Vanvitelli ; Prof. Carmelo Dazzi, Professore di Scienze Agrarie dell'Università degli Studi di Palermo"
x[[21]][10] <- "Prof. Paolo Pileri, Professore ordinario di pianificazione e progettazione urbanistica del Politecnico di Milano ;  Prof. Giuseppe Corti, Professore ordinario di Pedologia Dipartimento di Scienze agrarie, alimentari e ambientali dell'Università Politecnica delle Marche ; Prof. Carlo Berizzi, Associated Professor in Architectural and Urban Design Coordinator for International Mobility Faculty of Engineering dell'Università degli Studi di Pavia ; Prof.ssa Simona Tondelli, Professore associato di tecnica e pianificazione urbanistica Dipartimento di Architettura dell'Università degli Studi di Bologna"

a <- paste(x[[22]][[4]],x[[22]][[5]], sep = " ")
x[[22]] <- x[[22]][-c(4,5)]
x[[22]] <- append(x[[22]], a, after = 3)
a <- paste(x[[22]][[8]],x[[22]][[9]], sep = " ")
x[[22]] <- x[[22]][-c(8,9)]
x[[22]] <- append(x[[22]], a, after = 7)
a <- paste(x[[22]][[10]],x[[22]][[11]], sep = " ")
x[[22]] <- x[[22]][-c(10,11)]
x[[22]] <- append(x[[22]], a, after = 9)

x[[22]][1] <- "Prof. Fabio Castelli, Presidente della Scuola di ingegneria e ordinario di idrologia e costruzioni idrauliche dell'Università degli Studi di Firenze ; Prof. Terribile, Professore di pedologia dell'Università degli Studi Federico II di Napoli"
x[[22]][3] <- "Mountain Wilderness Italia ; Federdistribuzione"
x[[22]][5] <- "Alleanza delle cooperative italiane ; UECOOP ; UNCI Agroalimentare ; Consiglio Nazionale della Green Economy"
x[[22]][6] <- "Associazione Nazionale Piccoli Comuni d'Italia ; Cisambiente"
x[[22]][10] <- "CIA ; Associazione italiana per la wilderness ; Gruppo di intervento giuridico ; Accademia italiana di Permacultura"

a <- paste(x[[23]][[2]],x[[23]][[3]], sep = " ")
x[[23]] <- x[[23]][-c(2,3)]
x[[23]] <- append(x[[23]], a, after = 1)
a <- paste(x[[23]][[3]],x[[23]][[4]], sep = " ")
x[[23]] <- x[[23]][-c(3,4)]
x[[23]] <- append(x[[23]], a, after = 2)

x[[23]] <- c("VAS Verdi Ambiente e Società", "Città amica", "Istat", "ANCE", "ISPRA", "Rete Professioni Tecniche", "Dott.ssa Gaia Checcucci, Direzione generale per la salvaguardia del territorio e delle acque del Ministero dell'ambiente e della tutela del territorio e del mare", "Dott.ssa Silvana Salvati, ISPRA", "Dott. Giovanni Balestri, consulente tecnico ecoreati", "Prof. Campanile, Università Federico II")


words_to_remove <- "Documentazione|fornita|da rappresentanti|relatore|Audizione|informale|Memoria -|Documento|trasmesso|depositato|trasmessa|Memoria depositata|Slide presentazione|audizione|- Allegato documento|Proposte modifica decreto|congiunto|- Documentazione|acquisita|DOCUMENTAZIONE|- Memoria|Memoria|Presentazione|inviata"

for (i in 1:25) {
  x[[i]] <- gsub(words_to_remove, "", x[[i]]) %>%
    str_trim(side = "left")
}

# Strategia per pulire la colonna degli auditi
x <- lapply(x, str_squish)
x <- lapply(x, function(x) gsub("\\([^()]*\\d+[^()]*\\)"," ;", x))
x <- lapply(x, function(x) gsub("\\;$", "", x))
x <- lapply(x, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:25) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

my_fun <- function(x) {
  x[!x %in% ""]
}

z <- c(1,6,9:12,20:22)

for (i in z) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) paste(x, collapse = " ; "))
}

z <- c(2:5, 7,8, 13:19, 23:25)

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

nomi <- x

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 13
c13 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c13, "[path]/commissione13.csv", row.names = FALSE)
