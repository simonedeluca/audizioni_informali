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

x[[2]][[3]] <- "Audizioni informali sulla crisi in Afghanistan e sui possibili scenari successivi"
x[[2]][[5]] <- "Audizioni informali sulla crisi in Afghanistan e sui possibili scenari successivi"
x[[2]][[6]] <- "Audizioni informali sulla crisi in Afghanistan e sui possibili scenari successivi"
x[[2]][[7]] <- "Audizioni informali sulla crisi in Afghanistan e sui possibili scenari successivi"

x[[3]][5] <- "Audizioni informali sull'Accordo quadro per gli investimenti UE-Cina"

x[[4]][[2]] <- "La posizione dell'Azerbaigian sulla questione armena"

x[[5]][[3]] <- "Il ruolo del Canton Ticino nell’ambito delle relazioni transfrontaliere e bilaterali tra Svizzera e Italia"
x[[5]][[4]] <- "Expo 2020 Dubai"

x[[5]] <- append(x[[5]], "Il futuro delle relazioni tra l'Italia e la Federazione russa", after = 7)
x[[5]] <- append(x[[5]], "L'Italia nel contesto geopolitico", after = 8)

x[[6]] <- append(x[[6]], "Il futuro delle relazioni tra l'Italia e la Federazione russa", after = 2)
x[[6]] <- append(x[[6]], "Il futuro delle relazioni tra l'Italia e la Federazione russa", after = 6)

x[[7]] <- append(x[[7]], "Il futuro delle relazioni tra l'Italia e la Federazione russa", after = 1)

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

x[[8]] <- append(x[[8]], c("15 Giugno 2021", "9 Giugno 2021", "8 Giugno 2021"), after = 1)

data <- x

# 3. Estrazione commissioni coinvolte

x <- vector(mode = "list", length = 8)

x[[1]] <- rep("3 (Aff. esteri)", 10)
x[[1]][[10]] <- "3 (Aff. esteri) ; 4 (Difesa)"

x[[2]] <- rep("3 (Aff. esteri)", 10)
x[[2]][[1]] <- "3 (Aff. esteri) ; 4 (Difesa)"
x[[2]][[4]] <- "3 (Aff. esteri) ; 4 (Difesa)"
x[[2]][[3]] <- "3 (Aff. esteri) ; 4 (Difesa) ; III (Aff. esteri) ; IV (Difesa)"
x[[2]][[5]] <- "3 (Aff. esteri) ; 4 (Difesa) ; III (Aff. esteri) ; IV (Difesa)"
x[[2]][[6]] <- "3 (Aff. esteri) ; 4 (Difesa) ; III (Aff. esteri) ; IV (Difesa)"
x[[2]][[7]] <- "3 (Aff. esteri) ; 4 (Difesa) ; III (Aff. esteri) ; IV (Difesa)"

x[[3]] <- rep("3 (Aff. esteri)", 10)
x[[3]][[5]] <- "3 (Aff. esteri) ; 14 (Politiche dell'UE)"

x[[4]] <- rep("3 (Aff. esteri)", 10)

x[[5]] <- rep("3 (Aff. esteri)", 10)

x[[6]] <- rep("3 (Aff. esteri)", 10)

x[[7]] <- rep("3 (Aff. esteri)", 9)

x[[8]] <- rep("3 (Aff. esteri)", 4)

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

a <- "Conferenza delle Regioni e delle Province autonome ;"
x[[1]] <- append(x[[1]], a, after = 3)
a <- paste(x[[1]][[4]],x[[1]][[5]],x[[1]][[6]], sep = " ")
x[[1]] <- x[[1]][-c(4,5,6)]
x[[1]] <- append(x[[1]], a, after = 3)

a <- "Conferenza dei Presidenti delle Assemblee legislative delle Regioni e delle Province autonome ; Comitato europeo delle Regioni ; UPI ;" 
x[[1]] <- append(x[[1]], a, after = 8)
a <- paste(x[[1]][[9]],x[[1]][[10]],x[[1]][[11]], sep = " ")
x[[1]] <- x[[1]][-c(9,10,11)]
x[[1]] <- append(x[[1]], a, after = 8)

x[[1]][[10]] <- "Alberto Negri, giornalista ; Luciano Pezzotti, ambasciatore"

x[[2]][[4]] <- "Lucio Caracciolo, direttore della rivista Limes ; Rossella Miccio, presidente di Emergency ; Fausto Biloslavo, giornalista ; Stefano Di Carlo, direttore Medici Senza Frontiere Italia ; Francesco Segoni, responsabile delle comunicazioni Medici Senza Frontiere Francia"

x[[7]][[8]] <- "Lapo Pistelli, direttore delle relazioni internazionali dell'ENI"
x[[7]][[9]] <- "Giampiero Massolo, ambasciatore e presidente dell'Istituto per gli Studi di Politica Internazionale (ISPI)"

x[[8]][[2]] <- "Cristian Nani, direttore di Porte Aperte Onlus"

nomi <- x
rm(x)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 3
c3 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c3, "[path]/audizioni_informali/data/raw_data/senato/commissione3.csv", row.names = FALSE)
