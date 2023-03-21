library("rvest")
library("dplyr")
library("stringr")

# COMMISSIONE 3 - AFFARI ESTERI, EMIGRAZIONE

link <- "https://www.senato.it/Leg18/3652"
page <- read_html(link)

# 1. Estrazione atto

atto <- vector(mode = "list", length = 8)

for (page_index in 1:8) {
  link <- paste("https://www.senato.it/Leg18/3652?current_page_40501=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[4]][[2]] <- "NA"

x[[5]][[3]] <- "NA"
x[[5]][[4]] <- "Expo 2020 Dubai"
a <- rep("NA", 2)
x[[5]] <- append(x[[5]], a, after = 7)

x[[6]] <- append(x[[6]], "NA", after = 2)
x[[6]] <- append(x[[6]], "NA", after = 6)

x[[7]] <- append(x[[7]], "NA", after = 1)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 8)

for (page_index in 1:8) {
  link <- paste("https://www.senato.it/Leg18/3652?current_page_40501=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

a <- page %>% html_nodes(".data_ricezione_documento em") %>% html_text()
x[[8]] <- append(x[[8]], a, after = 1)

data <- x

# 3. Estrazione commissioni coinvolte

x <- vector(mode = "list", length = 8)

x[[1]] <- rep("3 (Aff. esteri, emigrazione)", 10)
x[[1]][[10]] <- "Uffici di Presidenza Commissioni 3 e 4 riunite"

x[[2]] <- rep("3 (Aff. esteri, emigrazione)", 10)
x[[2]][[1]] <- "Uffici di Presidenza Commissioni 3 e 4 riunite"
x[[2]][[4]] <- "Uffici di Presidenza Commissioni 3 e 4 riunite"
x[[2]][[3]] <- "Uffici di Presidenza Commissioni congiunte 3 e 4 Senato e III e IV Camera"
x[[2]][[5]] <- "Uffici di Presidenza Commissioni congiunte 3 e 4 Senato e III e IV Camera"
x[[2]][[6]] <- "Uffici di Presidenza Commissioni congiunte 3 e 4 Senato e III e IV Camera"
x[[2]][[7]] <- "Uffici di Presidenza Commissioni congiunte 3 e 4 Senato e III e IV Camera"

x[[3]] <- rep("3 (Aff. esteri, emigrazione)", 10)
x[[3]][[5]] <- "Uffici di Presidenza integrati Commissioni riunite 3 e 14"

x[[4]] <- rep("3 (Aff. esteri, emigrazione)", 10)

x[[5]] <- rep("3 (Aff. esteri, emigrazione)", 10)

x[[6]] <- rep("3 (Aff. esteri, emigrazione)", 10)

x[[7]] <- rep("3 (Aff. esteri, emigrazione)", 9)

x[[8]] <- rep("3 (Aff. esteri, emigrazione)", 4)

commissione <- x

# 4. Estrazione auditi.

nomi <- vector(mode = "list", length = 8)

for (page_index in 1:8) {
  link <- paste("https://www.senato.it/Leg18/3652?current_page_40501=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

for (i in 1:8) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- "Conferenza delle Regioni e delle Province autonome,"
x[[1]] <- append(x[[1]], a, after = 3)
a <- paste(x[[1]][[4]],x[[1]][[5]],x[[1]][[6]], sep = " ")
x[[1]] <- x[[1]][-c(4,5,6)]
x[[1]] <- append(x[[1]], a, after = 3)

a <- "Conferenza dei Presidenti delle Assemblee legislative delle Regioni e delle Province autonome, Comitato europeo delle Regioni, UPI," 
x[[1]] <- append(x[[1]], a, after = 8)
a <- paste(x[[1]][[9]],x[[1]][[10]],x[[1]][[11]], sep = " ")
x[[1]] <- x[[1]][-c(9,10,11)]
x[[1]] <- append(x[[1]], a, after = 8)

x[[1]][[10]] <- "giornalista Alberto Negri e Ambasciatore Luciano Pezzotti"

x[[2]][[4]] <- "Lucio Caracciolo (Direttore della rivista Limes), Rossella Miccio (Presidente di Emergency), Fausto Biloslavo (giornalista), Stefano Di Carlo (direttore Medici Senza Frontiere Italia) e Francesco Segoni (responsabile delle comunicazioni Medici Senza Frontiere Francia)"

x[[7]][[8]] <- "Direttore delle relazioni internazionali dell'ENI, Lapo Pistelli"
x[[7]][[9]] <- "Presidente dell'Istituto per gli Studi di Politica Internazionale (ISPI), ambasciatore Giampiero Massolo"

x[[8]][[2]] <- "Direttore di Porte Aperte Onlus, Cristian Nani"

nomi <- x
rm(x)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 3
c3 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)
