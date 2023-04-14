library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 16)

for (page_index in 1:16) {
  link <- paste("https://www.senato.it/Leg18/3680?current_page_40471=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 2. Estrazione data

data <- vector(mode = "list", length = 16)

for (page_index in 1:16) {
  link <- paste("https://www.senato.it/Leg18/3680?current_page_40471=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data
x[[9]] <- append(x[[9]], "01 Ottobre 2019", after = 1)

a <- c("22 Febbraio 2022","14 Gennaio 2021", "31 Ottobre - 26 Novembre 2018", "19 Luglio 2018", "18 Luglio 2018", "23 Giugno 2018", "25 Luglio 2018", "27 Aprile 2021") 
x[[14]] <- append(x[[14]], a, after = 2)

a <- c("06 settembre 2018", "06 maggio 2021", "18 settembre 2018", "marzo 2021", "20 gennaio 2021", "NA", "NA", "02 marzo 2022", "NA", "09 marzo 2022")
x[[15]] <- a

x[[16]] <- "10 Marzo 2022"

data <- x

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 16)

for (page_index in 1:16) {
  link <- paste("https://www.senato.it/Leg18/3680?current_page_40471=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("11 (Lavoro)", 6)
x[[1]] <- append(x[[1]], a, after = 0)
a <- rep("11 (Lavoro)", 2)
x[[1]] <- append(x[[1]], a, after = 8)

a <- rep("11 (Lavoro)", 3)
x[[2]] <- append(x[[2]], a, after = 0)

a <- rep("11 (Lavoro)", 5)
x[[5]] <- append(x[[5]], a, after = 4)

x[[6]] <- append(x[[6]], "11 (Lavoro)", after = 0)
a <- rep("11 (Lavoro)", 3)
x[[6]] <- append(x[[6]], a, after = 3)

n <- c(7,9:13)
a <- rep("11 (Lavoro)", 10)

for (i in n) {
  x[[i]] <- a
}

a <- rep("11 (Lavoro)", 8)
x[[8]] <- append(x[[8]], a, after = 0)
x[[8]] <- append(x[[8]], "11 (Lavoro)", after = 9)

a <- rep("11 (Lavoro)", 9)
x[[14]] <- append(x[[14]], a, after = 0)

a <- rep("11 (Lavoro)", 8)
x[[15]] <- append(x[[15]], a, after = 0)
x[[15]] <- append(x[[15]], "11 (Lavoro)", after = 9)

x[[16]] <- "11 (Lavoro)"

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 16)

for (page_index in 1:16) {
  link <- paste("https://www.senato.it/Leg18/3680?current_page_40471=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".titolo_pubblicato strong") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

# Pre-cleaning

words_to_remove <- "Audizione|Audizioni|informale|informali|Memoria|, in videoconferenza,|in videoconferenza|di esperti e|di rappresentanti|Trasmissione|memorie|osservazioni|Documentazione|trasmessa"

for (i in 1:16) {
  x[[i]] <- gsub(words_to_remove, "", x[[i]]) %>%
    str_trim(side = "left")
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da"

for (i in 1:16) {
  x[[i]] <- gsub(words2, "", x[[i]]) %>%
    str_trim(side = "left")
}

x[[1]][1] <- "Presidente del Consiglio di indirizzo e vigilanza dell'INPS, Presidente del Consiglio nazionale dell'Ordine dei consulenti del lavoro, Segretario generale del CNEL"
x[[1]][7] <- "Elena Bonetti - Ministro per le pari opportunità e la famiglia"
x[[1]][8] <- "Andrea Orlando - Ministro del lavoro e delle politiche sociali"

x[[5]][2] <- "CNA, Alberto Liguori, ANMIL"
x[[5]][5] <- "Prof. Rosina, Prof. Bonifazi, Prof.ssa De Rose"
x[[5]][6] <- "CGIL, CISL, UIL, CONFSAL, CUB, INPS, ISTAT, Conflavoro, Unilavoro"
x[[5]][7] <- "Prof. Dalla Zuanna"
x[[5]][8] <- "Ordine Nazionale Consulenti del Lavoro, Prof.ssa Saraceno"

x[[7]][2] <- "Uber Eats"

x[[8]][5] <- "Fidaldo, Federdistribuzione"
x[[8]][9] <- "SIML, ANCE, Dott. Guariniello, UGL, IGESAN, UIL"
x[[8]][10] <- "FNOPI, CARER, CGIL, Forum Ex Articolo 26, Associazione Bambini Cerebrolesi, FISH-FAND, UIL, CONFAD, Parkinson Italia, Co.Fa.As., FIRST, Cittadinanzattiva, ANFFAS, CISL, Ass. ALICE"

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=9") %>%
  html_nodes("#container_40476_1 a") %>% html_text()

a <- gsub("Documento|depositato|Memoria|depositata|trasmesso|Testo dell'intervento", "", a) %>%
  str_trim(side = "left")

a <- gsub("^di|^del|^della|^dell'|dal|dall'|dalla|^delle|^dei|^da","", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ",")

x[[9]] <- append(x[[9]], a, after = 1)
x[[9]][6] <- "Dott. Porcellana"

x[[10]][10] <- "Prof. Natalini, Avv. Cardarello, Avv. Bigot, Prof.ssa Tullini"

x[[11]][6] <- "Poste Italiane"

x[[12]][9] <- "Confcommercio, AIMA, Fondazione promozione sociale, COMIP, ProRETT, UFHA, FIRST"

x[[13]][5] <- "AIDLASS, Consiglio Nazionale Ordine dei Consulenti del Lavoro"

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=14") %>%
  html_nodes("#list_40478_4 a") %>% html_text()

a <- gsub("Memorie", "", a)
a <- gsub("\\s*\\([^\\)]+\\)","", a) %>%
  str_trim(side = "both")
a <- paste(a, collapse = ",")

x[[14]] <- append(x[[14]], a, after = 4)
x[[14]][8] <- "Assindatcolf"
x[[14]][10] <- "AWI - Art Workers Italia, CUB - Confederazione unitaria di base"

x[[15]][2] <- "CONFETRA"

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=15") %>%
  html_nodes("#list_40479_3 a") %>% html_text()
a <- a[-c(3,9)]
a <- gsub("Memorie", "", a)
a <- gsub("\\s*\\([^\\)]+\\)","", a) %>%
  str_trim(side = "both")
a <- paste(a, collapse = ",")

x[[15]][4] <- a

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=15") %>%
  html_nodes("#container_40476_4 a") %>% html_text()
a <- a[-5]
a <- gsub("Memorie", "", a)
a <- gsub("\\s*\\([^\\)]+\\)","", a) %>%
  str_trim(side = "both")
a <- paste(a, collapse = ",")

x[[15]][5] <- a

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=15") %>%
  html_nodes("#container_40476_6 a") %>% html_text()
a <- a[-6]
a <- gsub("Memorie", "", a)
a <- gsub("\\s*\\([^\\)]+\\)","", a) %>%
  str_trim(side = "both")
a <- paste(a, collapse = ",")

x[[15]][7] <- a

a <- read_html("https://www.senato.it/Leg18/3680?current_page_40471=15") %>%
  html_nodes("#list_40480_8 a") %>% html_text()
a <- gsub("\\s*\\([^\\)]+\\)","", a) %>%
  str_trim(side = "both")
a <- paste(a, collapse = ",")

x[[15]][9] <- a

x[[15]][10] <- "Università Cattolica di Milano, Avv. Gabriele Fava, ADAPT"

nomi <- x

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 7
c11 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c11, "C:/Users/pc/Desktop/Progetto Audizioni/raw_data/commissione11.csv", row.names = FALSE)





