library("rvest")
library("dplyr")
library("stringr")

# COMMISSIONE 2 - GIUSTIZIA

link <- "https://www.senato.it/Leg18/3649?current_page_40381=1"
page <- read_html(link)

# 1. Estrazione atti.

atto <- vector(mode = "list", length = 13) #13 pagine

for (page_index in 1:13) {
  link <- paste("https://www.senato.it/Leg18/3649?current_page_40381=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

atto[[5]] <- append(atto[[5]], "AUDIZIONI INFORMALI NELL'AMBITO DELL'ESAME DEL DISEGNO DI LEGGE N. 2086 (ISTIGAZIONE ALL'AUTOLESIONISMO)", after = 6)
atto[[6]] <- append(atto[[6]], "AUDIZIONI INFORMALI NELL'AMBITO DELL'ESAME DEI DISEGNI DI LEGGE NN. 2005 E 2205 (CONTRASTO DELLA DISCCRIMINAZIONE O VIOLENZA PER SESSO, GENERE O DISABILITA')", after = 2)
atto[[8]] <- atto[[8]][-5]
atto[[9]] <- append(atto[[9]], "AUDIZIONI INFORMALI NELL'AMBITO DELL'ESAME DEI DISEGNI DI LEGGE NN. 1438, 1516, 1555, 1582 e 1714 (MAGISTRATURA ONORARIA)", after = 3)

# 2. Estrazione data.

data <- vector(mode = "list", length = 13)

for (page_index in 1:13) {
  link <- paste("https://www.senato.it/Leg18/3649?current_page_40381=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

data[[8]] <- data[[8]][-5]
data[[9]] <- append(data[[9]], "27 Novembre 2019", after = 0)

link <- "https://www.senato.it/Leg18/3649?current_page_40381=13"
page <- read_html(link)
x <- page %>% html_nodes(".data_ora_inizio a") %>% html_text()
x <- x[-1]
x <- gsub("del ", "", x)
data[[13]] <- append(data[[13]], x, after = 1)

# 3. Estrazione commissioni coinvolte.

commissione <- vector(mode = "list", length = 13)

for (page_index in 1:13) {
  link <- paste("https://www.senato.it/Leg18/3649?current_page_40381=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- rep("2 (Giustizia)", 3)
commissione[[1]] <- append(commissione[[1]], x, after = 0)
x <- rep("2 (Giustizia)", 6)
commissione[[1]] <- append(commissione[[1]], x, after = 4)

commissione[[2]] <- append(commissione[[2]], "2 (Giustizia)", after = 0)

x <- rep("2 (Giustizia)", 4)
commissione[[4]] <- append(commissione[[4]], x, after = 6)

x <- rep("2 (Giustizia)", 2)
commissione[[5]] <- append(commissione[[5]], x, after = 6)

commissione[[6]] <- append(commissione[[6]], "2 (Giustizia)", after = 0)
x <- rep("2 (Giustizia)", 7)
commissione[[6]] <- append(commissione[[6]], x, after = 2)

commissione[[7]] <- append(commissione[[7]], "2 (Giustizia)", after = 0)
x <- rep("2 (Giustizia)", 8)
commissione[[7]] <- append(commissione[[7]], x, after = 2)

x <- rep("2 (Giustizia)", 8)
commissione[[8]] <- append(commissione[[8]], x, after = 1)

commissione[[9]] <- rep("2 (Giustizia)", 9)

x <- rep("2 (Giustizia)", 2)
commissione[[10]] <- append(commissione[[10]], x, after = 0)
commissione[[10]] <- append(commissione[[10]], "2 (Giustizia)", after = 3)
x <- rep("2 (Giustizia)", 5)
commissione[[10]] <- append(commissione[[10]], x, after = 5)

x <- rep("2 (Giustizia)", 4)
commissione[[11]] <- append(commissione[[11]], x, after = 0)
x <- rep("2 (Giustizia)", 5)
commissione[[11]] <- append(commissione[[11]], x, after = 5)

commissione[[12]] <- rep("2 (Giustizia)", 10)

commissione[[13]] <- append(commissione[[13]], "2 (Giustizia)", after = 0)
x <- rep("2 (Giustizia)", 2)
commissione[[13]] <- append(commissione[[13]], x, after = 3)
commissione[[13]] <- append(commissione[[13]], "2 (Giustizia)", after = 6)

# 4. Estrazione auditi.

nomi <- vector(mode = "list", length = 13)

for (page_index in 1:13) {
  link <- paste("https://www.senato.it/Leg18/3649?current_page_40381=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi
for (i in 1:13) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- paste(x[[1]][[3]],x[[1]][[4]], sep = " ")
x[[1]] <- x[[1]][-c(3,4)]
x[[1]] <- append(x[[1]], a, after = 2)

a <- paste(x[[1]][[4]],x[[1]][[5]], sep = " ")
x[[1]] <- x[[1]][-c(4,5)]
x[[1]] <- append(x[[1]], a, after = 3)

a <- paste(x[[1]][[7]],x[[1]][[8]],x[[1]][[9]], sep = " ")
x[[1]] <- x[[1]][-c(7,8,9)]
x[[1]] <- append(x[[1]], a, after = 6)

a <- paste(x[[1]][[8]],x[[1]][[9]],x[[1]][[10]], sep = " ")
x[[1]] <- x[[1]][-c(8,9,10)]
x[[1]] <- append(x[[1]], a, after = 7)


a <- "Società Italiana per l'Ingegneria Culturale ; MEDIASET ; Capitolo Italiano Creative Commons-Wikimedia Italia ; Autorità per le garanzie nelle Comunicazioni"
x[[5]] <- append(x[[5]], a, after = 3)
a <- "Dott. Marco Giorello, Commissione europea DG CONNECT ; prof. Gustavo Ghidini, Università degli Studi di Milano ; SIAE (Società Italiana degli Autori ed Editori) ; FIEG (Federazione Italiana Editori Giornali) ; UNIVIDEO (Unione Italiana Editoria Audiovisiva Media Digitali e Online) ; FAPAV (Federazione per la tutela dei contenuti audiovisivi e multimediali) ; FIMI (Federazione Industria Musicale Italiana) ; FEM (Federazione Editori Musicali) ; USPI (Unione Stampa Periodica Italiana)"
x[[5]] <- append(x[[5]], a, after = 4)
a <- "Nuovo IMAIE ; AIE (Associazione italiana Editori) ; ITSRIGHT ; Artisti 7607 ; LEA (Liberi Editori e Autori) ; AISA (Associazione Italiana per la promozione della Scienza Aperta)"
x[[5]] <- append(x[[5]], a, after = 5)

a <- paste(x[[6]][[3]],x[[6]][[4]], sep = " ")
x[[6]] <- x[[6]][-c(3,4)]
x[[6]] <- append(x[[6]], a, after = 2)
a <- paste(x[[6]][[4]],x[[6]][[5]], sep = " ")
x[[6]] <- x[[6]][-c(4,5)]
x[[6]] <- append(x[[6]], a, after = 3)
a <- paste(x[[6]][[5]],x[[6]][[6]], sep = " ")
x[[6]] <- x[[6]][-c(5,6)]
x[[6]] <- append(x[[6]], a, after = 4)
a <- paste(x[[6]][[6]],x[[6]][[7]], sep = " ")
x[[6]] <- x[[6]][-c(6,7)]
x[[6]] <- append(x[[6]], a, after = 5)

a <- paste(x[[7]][[6]],x[[7]][[7]], sep = " ")
x[[7]] <- x[[7]][-c(6,7)]
x[[7]] <- append(x[[7]], a, after = 5)
a <- paste(x[[7]][[7]],x[[7]][[8]], sep = " ")
x[[7]] <- x[[7]][-c(7,8)]
x[[7]] <- append(x[[7]], a, after = 6)

x[[8]] <- append(x[[8]], "Dott.ssa Nunzia CIARDI, Polizia Postale Ministero Interno ; GIANNELLI-BRIANI, ANP ; Paolo RUSSO, CISMAI ; Dott. Ciro CASCONE, procuratore tribunale minori Milano ; Associazione AICS ; Vincenzo VETERE, Presidente ACBS ; Martina COLASANTE, Google ; VENERANDI-FOLLIERI, Osservatorio nazionale bullismo e disagio giovanile", after = 0)
a <- paste(x[[8]][[2]],x[[8]][[3]], sep = " ")
x[[8]] <- x[[8]][-c(2,3)]
x[[8]] <- append(x[[8]], a, after = 1)
a <- paste(x[[8]][[3]],x[[8]][[4]], sep = " ")
x[[8]] <- x[[8]][-c(3,4)]
x[[8]] <- append(x[[8]], a, after = 2)
a <- paste(x[[8]][[4]],x[[8]][[5]], sep = " ")
x[[8]] <- x[[8]][-c(4,5)]
x[[8]] <- append(x[[8]], a, after = 3)
a <- paste(x[[8]][[6]],x[[8]][[7]], sep = " ")
x[[8]] <- x[[8]][-c(6,7)]
x[[8]] <- append(x[[8]], a, after = 5)
a <- paste(x[[8]][[7]],x[[8]][[8]], sep = " ")
x[[8]] <- x[[8]][-c(7,8)]
x[[8]] <- append(x[[8]], a, after = 6)
a <- paste(x[[8]][[8]],x[[8]][[9]],x[[8]][[10]], sep = " ")
x[[8]] <- x[[8]][-c(8,9,10)]
x[[8]] <- append(x[[8]], a, after = 7)

x[[9]] <- x[[9]][-1]
a <- paste(x[[9]][[3]],x[[9]][[4]],x[[9]][[5]], sep = " ")
x[[9]] <- x[[9]][-c(3,4,5)]
x[[9]] <- append(x[[9]], a, after = 2)
a <- paste(x[[9]][[4]],x[[9]][[5]],x[[9]][[6]], sep = " ")
x[[9]] <- x[[9]][-c(4,5,6)]
x[[9]] <- append(x[[9]], a, after = 3)

x[[10]][[3]] <- "IVASS"
a <- paste(x[[10]][[4]],x[[10]][[5]], sep = " ")
x[[10]] <- x[[10]][-c(4,5)]
x[[10]] <- append(x[[10]], a, after = 3)
x[[10]][[5]] <- "Assogestioni"
a <- paste(x[[10]][[7]],x[[10]][[8]], sep = " ")
x[[10]] <- x[[10]][-c(7,8)]
x[[10]] <- append(x[[10]], a, after = 6)
a <- paste(x[[10]][[8]],x[[10]][[9]], sep = " ")
x[[10]] <- x[[10]][-c(8,9)]
x[[10]] <- append(x[[10]], a, after = 7)
a <- paste(x[[10]][[9]],x[[10]][[10]],x[[10]][[11]], sep = " ")
x[[10]] <- x[[10]][-c(9,10,11)]
x[[10]] <- append(x[[10]], a, after = 8)
a <- paste(x[[10]][[10]],x[[10]][[11]], sep = " ")
x[[10]] <- x[[10]][-c(10,11)]
x[[10]] <- append(x[[10]], a, after = 9)

a <- paste(x[[13]][[4]],x[[13]][[5]], sep = " ")
x[[13]] <- x[[13]][-c(4,5)]
x[[13]] <- append(x[[13]], a, after = 3)

nomi <- x
rm(x)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 1
c2 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)
write.csv(c2, "C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione2.csv", row.names = FALSE)
