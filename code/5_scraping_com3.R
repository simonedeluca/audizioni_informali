library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/3658?current_page_40411=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

a <- "Prima relazione annuale sull'andamento degli interventi dei Piani Sviluppo e Coesione su dati riferiti al 31/12/2021"
x[[2]] <- append(x[[2]], a, after = 2)

a <- "Rapporto economico 2021 sull'Italia"
x[[6]] <- append(x[[6]], a, after = 5)

x[[10]] <- append(x[[10]], "NA", after = 5)

a <- "Relazione al Parlamento predisposta ai sensi dell’articolo 6 della L. 243/2012"
x[[13]] <- append(x[[13]], a, after = 4)
a <- "A.S. 2040 - Relazione tecnica di passaggio"
x[[13]] <- append(x[[13]], a, after = 6)

a <- "Individuazione priorità utilizzo Recovery Fund"
x[[15]] <- append(x[[15]], a, after = 7)

a <- "Misure economiche adottate dal Governo per fronteggiare l'emergenza da COVID-19"
x[[16]] <- append(x[[16]], a, after = 8)

a <- "Misure economiche adottate dal Governo per fronteggiare l'emergenza da COVID-19"
x[[17]] <- append(x[[17]], a, after = 2)

a <- "A.S. 728 - Relazione tecnica"
x[[23]] <- append(x[[23]], a, after = 0)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/3658?current_page_40411=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[19]] <- x[[19]][-3]

x[[25]][[9]] <- "18 maggio 2022"

data <- x

# 3. Estrazione commissioni coinvolte.

commissione <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/3658?current_page_40411=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("5 (Bilancio)", 1)
x[[1]] <- append(x[[1]], a, after = 0)
a <- rep("5 (Bilancio)", 3)
x[[1]] <- append(x[[1]], a, after = 2)
x[[1]] <- append(x[[1]], a, after = 6)

a <- rep("5 (Bilancio)", 2)
x[[2]] <- append(x[[2]], a, after = 0)
a <- rep("5 (Bilancio)", 5)
x[[2]] <- append(x[[2]], a, after = 3)

a <- rep("5 (Bilancio)", 1)
x[[3]] <- append(x[[3]], a, after = 0)
x[[3]] <- append(x[[3]], a, after = 3)
x[[3]] <- append(x[[3]], a, after = 4)
x[[3]] <- append(x[[3]], a, after = 8)

a <- rep("5 (Bilancio)", 3)
x[[4]] <- append(x[[4]], a, after = 4)
a <- rep("5 (Bilancio)", 2)
x[[4]] <- append(x[[4]], a, after = 8)

a <- rep("5 (Bilancio)", 9)
x[[5]] <- append(x[[5]], a, after = 0)

a <- rep("5 (Bilancio)", 5)
x[[6]] <- append(x[[6]], a, after = 3)

a <- rep("5 (Bilancio)", 9)
x[[7]] <- append(x[[7]], a, after = 1)

x[[8]] <- rep("5 (Bilancio)", 10)

a <- rep("5 (Bilancio)", 3)
x[[9]] <- append(x[[9]], a, after = 0)
x[[9]] <- append(x[[9]], "5 (Bilancio)", after = 4)

x[[10]] <- append(x[[10]], "5 (Bilancio)", after = 7)

x[[11]] <- append(x[[11]], "5 (Bilancio)", after = 9)

a <- rep("5 (Bilancio)", 2)
x[[12]] <- append(x[[12]], a, after = 1)

a <- rep("5 (Bilancio)", 7)
x[[13]] <- append(x[[13]], a, after = 3)

a <- rep("5 (Bilancio)", 2)
x[[14]] <- append(x[[14]], a, after = 0)
a <- rep("5 (Bilancio)", 6)
x[[14]] <- append(x[[14]], a, after = 4)

a <- rep("5 (Bilancio)", 5)
x[[15]] <- append(x[[15]], a, after = 5)
x[[15]][[8]] <- "Uffici di presidenza 5 e 14 Senato e V e XIV Camera"

x[[16]] <- rep("5 (Bilancio)", 10)
x[[16]][[9]] <- "Uffici di presidenza congiunti 5 e V"

a <- rep("5 (Bilancio)", 5)
x[[17]] <- append(x[[17]], a, after = 0)
a <- rep("5 (Bilancio)", 4)
x[[17]] <- append(x[[17]], a, after = 6)
x[[17]][[3]] <- "Uffici di Presidenza integrati 5 Senato e V Camera"
   
a <- rep("5 (Bilancio)", 7)
x[[18]] <- append(x[[18]], a, after = 0)

a <- rep("5 (Bilancio)", 4)
x[[19]] <- append(x[[19]], a, after = 1)
a <- rep("5 (Bilancio)", 2)
x[[19]] <- append(x[[19]], a, after = 7)

a <- rep("5 (Bilancio)", 8)
x[[20]] <- append(x[[20]], a, after = 0)
x[[20]] <- append(x[[20]], "5 (Bilancio)", after = 9)

a <- rep("5 (Bilancio)", 7)
x[[21]] <- append(x[[21]], a, after = 0)
x[[21]] <- append(x[[21]], "5 (Bilancio)", after = 8)

a <- rep("5 (Bilancio)", 8)
x[[22]] <- append(x[[22]], a, after = 2)

a <- rep("5 (Bilancio)", 7)
x[[23]] <- append(x[[23]], a, after = 0)
x[[23]] <- append(x[[23]], "5 (Bilancio)", after = 9)

a <- rep("5 (Bilancio)", 3)
x[[24]] <- append(x[[24]], a, after = 0)
a <- rep("5 (Bilancio)", 4)
x[[24]] <- append(x[[24]], a, after = 6)

x[[25]] <- rep("5 (Bilancio)", 9)
x[[25]][[7]] <- "Uffici Presidenza integrati 5 e V"
x[[25]][[8]] <- "Uffici Presidenza integrati 5 e V"

commissione <- x

# 4. Estrazione auditi.

nomi <- vector(mode = "list", length = 25)

for (page_index in 1:25) {
  link <- paste("https://www.senato.it/Leg18/3658?current_page_40411=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

for (i in 1:25) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- paste(x[[2]][[3]],x[[2]][[4]], sep = " ")
x[[2]] <- x[[2]][-c(3,4)]
x[[2]] <- append(x[[2]], a, after = 2)

x[[3]] <- append(x[[3]], "Maria Rosaria Carfagna, Ministro per il Sud e la Coesione territoriale", after = 2)

x[[4]] <- append(x[[4]], "Andrea Orlando, Ministro del lavoro e delle politiche sociali", after = 2)

a <- paste(x[[5]][[5]],x[[5]][[6]], sep = " ")
x[[5]] <- x[[5]][-c(5,6)]
x[[5]] <- append(x[[5]], a, after = 4)

a <- paste(x[[5]][[8]],x[[5]][[9]], sep = " ")
x[[5]] <- x[[5]][-c(8,9)]
x[[5]] <- append(x[[5]], a, after = 7)

x[[6]] <- append(x[[6]], "NA", after = 8)

x[[8]] <- append(x[[8]], "Laura Castelli, vice ministro dell'economia e delle finanze; dottor Carmine Di Nuzzo, dirigente della Ragioneria generale dello Stato; dottoressa Nunzia Vecchione, dirigente della Ragioneria generale dello Stato", after = 7)

a <- paste(x[[9]][[1]],x[[9]][[2]], sep = " ")
x[[9]] <- x[[9]][-c(1,2)]
x[[9]] <- append(x[[9]], a, after = 0)
a <- paste(x[[9]][[9]],x[[9]][[10]],x[[9]][[11]], sep = " ")
x[[9]] <- x[[9]][-c(9,10,11)]
x[[9]] <- append(x[[9]], a, after = 8)

a <- paste(x[[10]][[4]],x[[10]][[5]], sep = " ")
x[[10]] <- x[[10]][-c(4,5)]
x[[10]] <- append(x[[10]], a, after = 3)

a <- paste(x[[11]][[9]],x[[11]][[10]], sep = " ")
x[[11]] <- x[[11]][-c(9,10)]
x[[11]] <- append(x[[11]], a, after = 8)

a <- paste(x[[13]][[3]],x[[13]][[4]], sep = " ")
x[[13]] <- x[[13]][-c(3,4)]
x[[13]] <- append(x[[13]], a, after = 2)

a <- paste(x[[14]][[3]],x[[14]][[4]], sep = " ")
x[[14]] <- x[[14]][-c(3,4)]
x[[14]] <- append(x[[14]], a, after = 2)

a <- paste(x[[15]][[9]],x[[15]][[10]], sep = " ")
x[[15]] <- x[[15]][-c(9,10)]
x[[15]] <- append(x[[15]], a, after = 8)
a <- paste(x[[15]][[10]],x[[15]][[11]], x[[15]][[12]], sep = " ")
x[[15]] <- x[[15]][-c(10,11,12)]
x[[15]] <- append(x[[15]], a, after = 9)
x[[15]][[3]] <- "Banca d'Italia, ASSAEROPORTI, Assonime, Ance, CONFAPI, CNA (Confederazione nazionale dell'artigianato e della piccola e media impresa), Confartigianato Imprese, Confersercenti, Confcommercio Imprese per l'Italia, Casartigiani, CONFPROFESSIONI, Alleanza delle Cooperative italiane, FEDERDISTRIBUZIONE, Confagricoltura, CIA-Agricoltori italiani, Coldiretti e Filiera Italia, Copagri, CONFETRA e ANIA"
x[[15]][[4]] <- "ABI e Confindustria"

x[[16]] <- append(x[[16]], "Roberto Gualtieri, Ministro dell'economia e delle finanze", after = 8)

a <- paste(x[[17]][[1]],x[[17]][[2]], sep = " ")
x[[17]] <- x[[17]][-c(1,2)]
x[[17]] <- append(x[[17]], a, after = 0)
x[[17]] <- append(x[[17]], "Roberto Gualtieri, Ministro dell'economia e delle finanze", after = 2)

a <- paste(x[[18]][[8]],x[[18]][[9]], sep = " ")
x[[18]] <- x[[18]][-c(8,9)]
x[[18]] <- append(x[[18]], a, after = 7)
a <- paste(x[[18]][[9]],x[[18]][[10]], sep = " ")
x[[18]] <- x[[18]][-c(9,10)]
x[[18]] <- append(x[[18]], a, after = 8)
a <- paste(x[[18]][[10]],x[[18]][[11]], sep = " ")
x[[18]] <- x[[18]][-c(10,11)]
x[[18]] <- append(x[[18]], a, after = 9)

a <- paste(x[[19]][[1]],x[[19]][[2]], sep = " ")
x[[19]] <- x[[19]][-c(1,2)]
x[[19]] <- append(x[[19]], a, after = 0)
a <- paste(x[[19]][[6]],x[[19]][[7]], sep = " ")
x[[19]] <- x[[19]][-c(6,7)]
x[[19]] <- append(x[[19]], a, after = 5)

a <- paste(x[[22]][[2]],x[[22]][[3]], sep = " ")
x[[22]] <- x[[22]][-c(2,3)]
x[[22]] <- append(x[[22]], a, after = 1)

a <- paste(x[[23]][[8]],x[[23]][[9]], sep = " ")
x[[23]] <- x[[23]][-c(8,9)]
x[[23]] <- append(x[[23]], a, after = 7)

nomi <- x
rm(x)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 5
c5 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)