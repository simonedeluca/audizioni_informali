library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 19)

for (page_index in 1:19) {
  link <- paste("https://www.senato.it/Leg18/3690?current_page_40481=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[1]] <- append(x[[1]], "Audizioni AG n. 383 (fauna selvatica ed esotica)", after = 5)
x[[6]] <- append(x[[6]], "Affare assegnato sul potenziamento e riqualificazione della medicina territoriale nell'epoca post Covid (n. 569)", after = 8)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 19)

for (page_index in 1:19) {
  link <- paste("https://www.senato.it/Leg18/3690?current_page_40481=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[19]] <- append(x[[19]], "03 giugno 2021", after = 5)

data <- x

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 19)

for (page_index in 1:19) {
  link <- paste("https://www.senato.it/Leg18/3690?current_page_40481=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("12 (Sanità)", 2)
x[[1]] <- append(x[[1]], a, after = 1)
x[[1]] <- append(x[[1]], a, after = 1)
x[[1]] <- append(x[[1]], "12 (Sanità)", after = 9)

a <- rep("12 (Sanità)", 7)
x[[2]] <- append(x[[2]], a, after = 0)

a <- rep("12 (Sanità)", 5)
x[[3]] <- append(x[[3]], a, after = 5)

a <- rep("12 (Sanità)", 4)
x[[6]] <- append(x[[6]], a, after = 0)
a <- rep("12 (Sanità)", 5)
x[[6]] <- append(x[[6]], a, after = 5)

a <- rep("12 (Sanità)", 10)
z <- c(4,5,7:10,12,14:18)

for (i in z) {
  x[[i]] <- a
}

a <- rep("12 (Sanità)", 4)
x[[11]] <- append(x[[11]], a, after = 0)
a <- rep("12 (Sanità)", 5)
x[[11]] <- append(x[[11]], a, after = 5)

a <- rep("12 (Sanità)", 5)
x[[13]] <- append(x[[13]], a, after = 0)
a <- rep("12 (Sanità)", 4)
x[[13]] <- append(x[[13]], a, after = 6)

a <- rep("12 (Sanità)", 6)
x[[19]] <- append(x[[19]], a, after = 0)

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 19)

for (page_index in 1:19) {
  link <- paste("https://www.senato.it/Leg18/3690?current_page_40481=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 4.1 Pulizia nomi

x <- nomi

for (i in 1:19) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- paste(x[[1]][[6]],x[[1]][[7]], sep = " ")
x[[1]] <- x[[1]][-c(6,7)]
x[[1]] <- append(x[[1]], a, after = 5)

a <- paste(x[[6]][[4]],x[[6]][[5]],x[[6]][[6]], sep = " ")
x[[6]] <- x[[6]][-c(4,5,6)]
x[[6]] <- append(x[[6]], a, after = 3)

x[[7]] <- append(x[[7]], "NA", after = 5)

a <- paste(x[[8]][[3]],x[[8]][[4]], sep = " ")
x[[8]] <- x[[8]][-c(3,4)]
x[[8]] <- append(x[[8]], a, after = 2)

a <- paste(x[[10]][[6]],x[[10]][[7]], sep = " ")
x[[10]] <- x[[10]][-c(6,7)]
x[[10]] <- append(x[[10]], a, after = 5)
a <- paste(x[[10]][[8]],x[[10]][[9]], sep = " ")
x[[10]] <- x[[10]][-c(8,9)]
x[[10]] <- append(x[[10]], a, after = 7)

a <- paste(x[[11]][[1]],x[[11]][[2]], sep = " ")
x[[11]] <- x[[11]][-c(1,2)]
x[[11]] <- append(x[[11]], a, after = 0)

a <- paste(x[[12]][[3]],x[[12]][[4]], sep = " ")
x[[12]] <- x[[12]][-c(3,4)]
x[[12]] <- append(x[[12]], a, after = 2)

a <- paste(x[[13]][[2]],x[[13]][[3]], sep = " ")
x[[13]] <- x[[13]][-c(2,3)]
x[[13]] <- append(x[[13]], a, after = 1)

a <- paste(x[[14]][[6]],x[[14]][[7]], sep = " ")
x[[14]] <- x[[14]][-c(6,7)]
x[[14]] <- append(x[[14]], a, after = 5)

a <- paste(x[[15]][[7]],x[[15]][[8]], sep = " ")
x[[15]] <- x[[15]][-c(7,8)]
x[[15]] <- append(x[[15]], a, after = 6)
a <- paste(x[[15]][[8]],x[[15]][[9]], sep = " ")
x[[15]] <- x[[15]][-c(8,9)]
x[[15]] <- append(x[[15]], a, after = 7)

a <- paste(x[[16]][[1]],x[[16]][[2]], sep = " ")
x[[16]] <- x[[16]][-c(1,2)]
x[[16]] <- append(x[[16]], a, after = 0)
a <- paste(x[[16]][[10]],x[[16]][[11]], sep = " ")
x[[16]] <- x[[16]][-c(10,11)]
x[[16]] <- append(x[[16]], a, after = 9)

a <- paste(x[[17]][[2]],x[[17]][[3]], sep = " ")
x[[17]] <- x[[17]][-c(2,3)]
x[[17]] <- append(x[[17]], a, after = 1)

words_to_remove <- "Documentazione|Memoria - |Audizione|AUDIZIONE|Contributo fornito|DOCUMENTO|Slides|informale|fornita|inviata|depositata|pervenuta|trasmessa|consegnata|da rappresentanti|da parte|di rappresentanti|rappresentanti|Osservazioni trasmesse|Ulteriore documentazione|Documento|depositato|trasmesso|- Relazione|Memoria|Momoria|Documenti depositati|Osservazioni trasmesse|- risposte ai quesiti dei senatori|rappresentante|Documenti"
for (i in 1:19) {
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

for (i in 1:19) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

my_fun <- function(x) {
  x[!x %in% ""]
}

for (i in 1:19) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

for (i in 1:19) {
  x[[i]] <- lapply(x[[i]], unique)
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

for (i in 1:19) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in 1:19) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

for (i in 1:19) {
  x[[i]] <- lapply(x[[i]], function(x) paste(x, collapse = " ; "))
}

nomi <- x
nomi[[6]][[4]] <- "Fondazione Promozione Sociale Onlus ; Unione italiana dei ciechi e degli ipovedenti ; Dipartimento di Scienze biomediche del CNR ; SIOT ; AIFeC ; FNOPI ; Federchimica Assobiotec ; Gruppo Vita (Valore ed Innovazione delle Terapie Avanzate) ; CODIRP ; Campagna 2018 PHC Now or Never ; ALNYLAM ; FNOMCEO ; Omar Osservatorio malattie rare ; FOFI ; prof.ssa Abbracchio, Università degli Studi di Milano ; Federchimica Assogastecnici ; COSMED ; FNOVI ; dott. Boldrini, SIMFER ; Istituto Superiore di Sanita' ; SIFO"
nomi[[11]][[4]] <- "prof. Giuseppe REMUZZI, direttore dell'Istituto di ricerche farmacologhiche Mario Negri ; dott. ssa Ilaria CIANCALEONI BARTOLI, direttore dell'Osservatorio malattie rare (OMAR) ; dott.ssa Vincenza COLONNA, ricercatrice del CNR, genetista e bioinformatica ; prof. Franco LOCATELLI, professore ordinario di pediatria generale e specialistica dell'Università La Sapienza di Roma, presidente del Consiglio Superiore di Sanità ; prof. Massimo CICCOZZI, epidemiologo molecolare nell'ambito delle mutazioni del virus ; prof. Angelo DEL FAVERO, docente alla LUISS Business School e Università telematica La Sapienza, già direttore generale dell'Istituto Superiore di Sanità"
nomi[[12]][[4]] <- "Daniela CACCAMO, Associata di Biochimica Clinica e Biologia Molecolare Clinica presso l'Università di Messina"
nomi[[14]][[7]] <- "Assessore D'Amato, Regione Lazio"

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 12
c12 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c12, "C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione12.csv", row.names = FALSE)

