library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[15]] <- append(x[[15]], "Ddl 2401 - Decreto-legge 130/2021 - Contenimento degli effetti degli aumenti dei prezzi nel settore elettrico e del gas naturale", after = 4)
x[[30]] <- append(x[[30]], "Chiusura degli impianti di cracking e degli aromatici di Porto Marghera, nonché sulle conseguenze di carattere ambientale per il sito interessato e per lo sviluppo di processi di transizione ecologica", after = 1)
x[[40]] <- append(x[[40]], "NA", after = 1)
x[[43]] <- append(x[[43]], "Atto n. 572 - Proposta di Linee guida per la definizione del Piano nazionale di ripresa e resilienza", after = 0)
x[[43]] <- append(x[[43]], "Atto n. 572 - Proposta di Linee guida per la definizione del Piano nazionale di ripresa e resilienza", after = 1)
x[[43]] <- append(x[[43]], "Ricadute economiche conseguenti all'emergenza da Covid-19", after = 9)
x[[54]] <- append(x[[54]], "Affare sulle iniziative di sostegno ai comparti dell'industria, del commercio e del turismo nell'ambito della congiuntura economica conseguente all'emergenza da COVID-19 (n. 445)", after = 0)
x[[69]] <- append(x[[69]], "Vicende complesso ILVA", after = 7)
x[[69]] <- append(x[[69]], "Situazione delle imprese nelle aree di crisi complessa", after = 8)
x[[69]] <- append(x[[69]], "Comunicazione della Commissione al Parlamento europeo – Un New Deal per i consumatori [COM(2018) 183 def.] e Proposta di Direttiva sulla protezione dei consumatori [COM(2018) 185 def.]", after = 9)
x[[70]] <- append(x[[70]], "Comunicazione della Commissione al Parlamento europeo – Un New Deal per i consumatori [COM(2018) 183 def.] e Proposta di Direttiva sulla protezione dei consumatori [COM(2018) 185 def.]", after = 0)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[3]] <- x[[3]][-4]
x[[61]] <- append(x[[61]], "01 ottobre 2019", after = 8)
x[[70]] <- c("26 Luglio 2018", "20 Luglio 2022", "25 Febbraio 2021", "22 Settembre 2021", "20 Settembre 2021", "8 Febbraio 2019", "Settembre 2021")

data <- x

# Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("10 (Industria)", 10)
seq <- c(1,6:10,12,26:31,33:39,41,42,45:64,66:68)

for (i in seq) {
  x[[i]] <- a
}

a <- rep("10 (Industria)", 5)
x[[2]] <- append(x[[2]], a, after = 0)

a <- rep("10 (Industria)", 2)
x[[3]] <- append(x[[3]], a, after = 2)
a <- rep("10 (Industria)", 4)
x[[3]] <- append(x[[3]], a, after = 5)

x[[4]] <- append(x[[4]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 8)
x[[4]] <- append(x[[4]], a, after = 2)

a <- rep("10 (Industria)", 4)
x[[5]] <- append(x[[5]], a, after = 0)
a <- rep("10 (Industria)", 5)
x[[5]] <- append(x[[5]], a, after = 5)

a <- rep("10 (Industria)", 5)
x[[11]] <- append(x[[11]], a, after = 0)
a <- rep("10 (Industria)", 4)
x[[11]] <- append(x[[11]], a, after = 6)

x[[13]] <- append(x[[13]], "10ª (Industria) Senato con X (Att. produttive) Camera", after = 0)
a <- rep("10 (Industria)", 8)
x[[13]] <- append(x[[13]], a, after = 2)

a <- rep("10 (Industria)", 2)
x[[14]] <- append(x[[14]], a, after = 0)
a <- rep("10 (Industria)", 7)
x[[14]] <- append(x[[14]], a, after = 3)

a <- rep("10 (Industria)", 8)
x[[15]] <- append(x[[15]], a, after = 0)
x[[15]] <- append(x[[15]], "10 (Industria)", after = 9)

x[[23]] <- append(x[[23]], "10 (Industria)", after = 1)
x[[23]] <- append(x[[23]], "10 (Industria)", after = 5)
a <- rep("10 (Industria)", 3)
x[[23]] <- append(x[[23]], a, after = 7)

x[[25]] <- append(x[[25]], "Uffici di Presidenza Commissioni 2ª e 10ª riunite", after = 3)
x[[25]] <- append(x[[25]], "10 (Industria)", after = 4)
x[[25]] <- append(x[[25]], "10 (Industria)", after = 9)

a <- rep("10 (Industria)", 8)
x[[32]] <- append(x[[32]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[32]] <- append(x[[32]], a, after = 8)

a <- rep("10 (Industria)", 9)
x[[40]] <- append(x[[40]], a, after = 0)

a <- rep("10 (Industria)", 2)
x[[43]] <- append(x[[43]], a, after = 0)
a <- rep("10 (Industria)", 7)
x[[43]] <- append(x[[43]], a, after = 3)

x[[44]] <- append(x[[44]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 3)
x[[44]] <- append(x[[44]], a, after = 3)
a <- rep("10 (Industria)", 3)
x[[44]] <- append(x[[44]], a, after = 7)

x[[65]] <- append(x[[65]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 8)
x[[65]] <- append(x[[65]], a, after = 2)

a <- rep("10 (Industria)", 7)
x[[69]] <- append(x[[69]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[69]] <- append(x[[69]], a, after = 8)

a <- rep("10 (Industria)", 3)
x[[70]] <- append(x[[70]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[70]] <- append(x[[70]], a, after = 4)

commissione <- x




















