library(rvest)
library(dplyr)
library(stringr)

# 1. Estrazione atto

atto <- vector(mode = "list", length = 20)

for (page_index in 1:20) {
  link <- paste("https://www.senato.it/Leg18/3665?current_page_40441=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

# Integrazione mancanti

x[[2]] <- append(x[[2]], "Piano industriale 2022-2031 del gruppo", after = 3)
x[[3]] <- append(x[[3]], "Sperimentazione di nuovi sistemi di mobilità di passeggeri e merci", after = 6)
x[[4]] <- append(x[[4]], "Aumento dei costi nel settore dell'autotrasporto", after = 1)
x[[7]] <- append(x[[7]], "Nuova linea ad alta velocità Salerno-Reggio Calabria e linea Potenza-Battipaglia", after = 6)
x[[7]] <- append(x[[7]], "Predisposizione del piano industriale della società ITA", after = 9)
x[[9]] <- append(x[[9]], "Predisposizione del piano industriale della società ITA", after = 6)
x[[10]] <- append(x[[10]], "Audizioni informali nell'ambito dell'esame del disegno di legge n. 1727 (interventi funivie Savona)", after = 5)
x[[10]] <- append(x[[10]], "Audizioni informali nell'ambito dell'esame del disegno di legge n. 1727 (interventi funivie Savona)", after = 8)
x[[10]] <- append(x[[10]], "Audizioni informali nell'ambito dell'esame del disegno di legge n. 1727 (interventi funivie Savona)", after = 9)
x[[12]] <- append(x[[12]], "Nuove tecnologie e rete per contrastare l'emergenza epidemiologica da coronavirus", after = 9)
x[[13]] <- append(x[[13]], "Audizioni informali sull'incidente ferroviario avvenuto a Lodi il 6 febbraio 2020", after = 1)
x[[13]] <- append(x[[13]], "Stato di avanzamento delle opere e delle attività relativa alla realizzazione del MO.S.E.", after = 7)
x[[14]] <- append(x[[14]], "Proposta di nomina del Presidente dell'Autorità di sistema portuale dello Stretto", after = 5)
x[[17]] <- append(x[[17]], "Proposta di nomina del Presidente dell'Ente Nazionale per l'Aviazione Civile (E.N.A.C.)", after = 6)
x[[18]] <- append(x[[18]], "Attività dell'ente ACI", after = 4)
x[[18]] <- append(x[[18]], "Attività dell'ente ACI", after = 6)
x[[19]] <- append(x[[19]], "Attività della società ENAV", after = 2)
x[[19]] <- append(x[[19]], "Attività dell'Autorità per le garanzie nelle comunicazioni", after = 3)
x[[19]] <- append(x[[19]], "Situazione economica e finanziaria dell'azienda ALITALIA", after = 4)
x[[19]] <- append(x[[19]], "affare assegnato sulle attività della Guardia Costiera, con particolare riferimento al soccorso in mare (n. 88)", after = 7)
x[[20]] <- append(x[[20]], "Attività ANAC in materia di contratti pubblici", after = 2)
x[[20]] <- append(x[[20]], "Attività ANAC in materia di contratti pubblici", after = 3)
x[[20]] <- append(x[[20]], "Crollo del Ponte Morandi di Genova", after = 4)
x[[20]] <- append(x[[20]], "Fermo nazionale dei servizi di autotrasporto merci conto terzi", after = 5)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 20)

for (page_index in 1:20) {
  link <- paste("https://www.senato.it/Leg18/3665?current_page_40441=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[18]] <- append(x[[18]], "05 Dicembre 2018", after = 0)
x[[20]] <- append(x[[20]], "25 Maggio 2022", after = 8)

data <- x

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 20)

for (page_index in 1:20) {
  link <- paste("https://www.senato.it/Leg18/3665?current_page_40441=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("8 (Lavori pubblici)", 10)
x[[1]] <- a
x[[2]] <- a

a <- rep("8 (Lavori pubblici)", 5)
x[[3]] <- append(x[[3]], a, after = 0)
x[[3]] <- append(x[[3]], "8 (Lavori pubblici)", after = 6)
x[[3]] <- append(x[[3]], "8 (Lavori pubblici)", after = 9)

a <- rep("8 (Lavori pubblici)", 10)
x[[4]] <- a

a <- rep("8 (Lavori pubblici)", 8)
x[[5]] <- append(x[[5]], a, after = 0)

a <- rep("8 (Lavori pubblici)", 3)
x[[6]] <- append(x[[6]], a, after = 1)
a <- rep("8 (Lavori pubblici)", 4)
x[[6]] <- append(x[[6]], a, after = 6)

a <- rep("8 (Lavori pubblici)", 10)
x[[7]] <- a

a <- rep("8 (Lavori pubblici)", 2)
x[[8]] <- append(x[[8]], a, after = 0)
a <- rep("8 (Lavori pubblici)", 6)
x[[8]] <- append(x[[8]], a, after = 4)

a <- rep("8 (Lavori pubblici)", 10)
x[[9]] <- a

x[[10]] <- append(x[[10]], "8 (Lavori pubblici)", after = 0)
x[[10]] <- append(x[[10]], "8 (Lavori pubblici)", after = 2)
a <- rep("8 (Lavori pubblici)", 6)
x[[10]] <- append(x[[10]], a, after = 4)

x[[11]] <- append(x[[11]], "8 (Lavori pubblici)", after = 0)

a <- rep("8 (Lavori pubblici)", 10)
x[[12]] <- a
x[[13]] <- a
x[[14]] <- a

a <- rep("8 (Lavori pubblici)", 3)
x[[15]] <- append(x[[15]], a, after = 0)
a <- rep("8 (Lavori pubblici)", 5)
x[[15]] <- append(x[[15]], a, after = 5)

a <- rep("8 (Lavori pubblici)", 10)
x[[16]] <- a

a <- rep("8 (Lavori pubblici)", 5)
x[[17]] <- append(x[[17]], a, after = 0)
x[[17]] <- append(x[[17]], "8 (Lavori pubblici)", after = 6)
a <- rep("8 (Lavori pubblici)", 2)
x[[17]] <- append(x[[17]], a, after = 8)

a <- rep("8 (Lavori pubblici)", 10)
x[[18]] <- a

a <- rep("8 (Lavori pubblici)", 10)
x[[19]] <- a
x[[19]][5] <- "8 (Lavori pubblici) ; 10 (Industria)"

a <- rep("8 (Lavori pubblici)", 4)
x[[20]] <- append(x[[20]], a, after = 0)
a <- rep("8 (Lavori pubblici)", 4)
x[[20]] <- append(x[[20]], a, after = 5)

commissione <- x

# 4. Estrazione nomi 1

nomi <- vector(mode = "list", length = 20)

for (page_index in 1:20) {
  link <- paste("https://www.senato.it/Leg18/3665?current_page_40441=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 5. Estrazioni nomi 2

nomi2 <- vector(mode = "list", length = 20)

for (page_index in 1:20) {
  link <- paste("https://www.senato.it/Leg18/3665?current_page_40441=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".titolo_pubblicato strong") %>% html_text()
  nomi2[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# Copy objects

x <- nomi

for (i in 1:20) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

y <- nomi2

for (i in 1:20) {
  y[[i]] <- gsub("[\n]", "", y[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

# Integrazione nomi 1 e nomi 2

y[[8]][4] <- "Vittorio Colao, ministro per l'innovazione tecnologica e la transizione digitale"
y[[8]][7] <- "UGL, organizzazione sindacale"
y[[11]][3] <- x[[11]][2]
a <- paste(x[[11]][[3]],x[[11]][[4]], sep = " ")
y[[11]][4] <- a
y[[11]][5] <- x[[11]][5]
y[[11]][6] <- x[[11]][6]
y[[11]][7] <- x[[11]][7]
a <- paste(x[[17]][[7]],x[[17]][[8]], sep = " ")
y[[17]][6] <- a
a <- paste(x[[17]][[9]],x[[17]][[10]], sep = " ")
y[[17]][8] <- a
y[[18]] <- append(y[[18]], "ANCE", after = 0)

nomi <- y

rm(x)
rm(y)
rm(nomi2)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 7
c8 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c8, "[path]/audizioni_informali/data/raw_data/senato/commissione8.csv", row.names = FALSE)
