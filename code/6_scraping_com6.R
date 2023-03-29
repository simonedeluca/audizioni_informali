library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 21)

for (page_index in 1:21) {
  link <- paste("https://www.senato.it/Leg18/3677?current_page_40421=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[9]] <- append(x[[9]], "NA", after = 8)
x[[12]] <- append(x[[12]], "NA", after = 7)

a <- c("NA", "NA")
x[[17]] <- append(x[[17]], a, after = 5)
x[[17]] <- append(x[[17]], "NA", after = 9)

a <- c("NA", "NA")
x[[18]] <- append(x[[18]], a, after = 0)
x[[18]] <- append(x[[18]], "NA", after = 3)

a <- rep("NA", 5)
x[[19]] <- append(x[[19]], a, after = 4)

x[[20]] <- append(x[[20]], "NA", after = 2)
x[[20]] <- append(x[[20]], "NA", after = 6)
x[[20]] <- append(x[[20]], "sull'attività svolta", after = 7)
a <- rep("NA", 2)
x[[20]] <- append(x[[20]], a, after = 8)

x[[21]] <- append(x[[21]], "NA", after = 0)
x[[21]] <- append(x[[21]], "NA", after = 3)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 21)

for (page_index in 1:21) {
  link <- paste("https://www.senato.it/Leg18/3677?current_page_40421=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[16]] <- x[[16]][-1]

a <- c("28 ottobre 2020", "08 maggio 2019", "26 settembre 2019", "26 settembre 2019", "26 settembre 2019")
x[[21]] <- append(x[[21]], a, after = 2)

data <- x

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 21)

for (page_index in 1:21) {
  link <- paste("https://www.senato.it/Leg18/3677?current_page_40421=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("6 (Finanze)", 2)
x[[1]] <- append(x[[1]], a, after = 2)

x[[2]] <- append(x[[2]], "6 (Finanze)", after = 8)

x[[3]] <- append(x[[3]], "6 (Finanze)", after = 3)

x[[7]] <- append(x[[7]], "6 (Finanze)", after = 9)

a <- rep("6 (Finanze)", 4)
x[[8]] <- append(x[[8]], a, after = 0)
x[[8]] <- append(x[[8]], a, after = 6)

x[[9]] <- append(x[[9]], "6 (Finanze)", after = 0)

a <- rep("6 (Finanze)", 8)
x[[12]] <- append(x[[12]], a, after = 2)

x[[13]] <- rep("6 (Finanze)", 10)

x[[14]] <- rep("6 (Finanze)", 10)

x[[15]] <- rep("6 (Finanze)", 10)

x[[16]] <- rep("6 (Finanze)", 9)

x[[17]] <- rep("6 (Finanze)", 10)

x[[18]] <- rep("6 (Finanze)", 10)

a <- rep("6 (Finanze)", 2)
x[[19]] <- append(x[[19]], a, after = 0)
a <- rep("6 (Finanze)", 6)
x[[19]] <- append(x[[19]], a, after = 4)

x[[20]] <- rep("6 (Finanze)", 10)

x[[21]] <- rep("6 (Finanze)", 7)

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 21)

for (page_index in 1:21) {
  link <- paste("https://www.senato.it/Leg18/3677?current_page_40421=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

for (i in 1:21) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

a <- paste(x[[1]][[2]],x[[1]][[3]], sep = " ")
x[[1]] <- x[[1]][-c(2,3)]
x[[1]] <- append(x[[1]], a, after = 1)

a <- paste(x[[1]][[4]],x[[1]][[5]], sep = " ")
x[[1]] <- x[[1]][-c(4,5)]
x[[1]] <- append(x[[1]], a, after = 3)

x[[7]][[1]] <- "COBTI"

a <- paste(x[[9]][[7]],x[[9]][[8]], sep = " ")
x[[9]] <- x[[9]][-c(7,8)]
x[[9]] <- append(x[[9]], a, after = 6)

a <- paste(x[[10]][[1]],x[[10]][[2]], sep = " ")
x[[10]] <- x[[10]][-c(1,2)]
x[[10]] <- append(x[[10]], a, after = 0)
a <- paste(x[[10]][[7]],x[[10]][[8]], sep = " ")
x[[10]] <- x[[10]][-c(7,8)]
x[[10]] <- append(x[[10]], a, after = 6)

a <- paste(x[[11]][[3]],x[[11]][[4]], sep = " ")
x[[11]] <- x[[11]][-c(3,4)]
x[[11]] <- append(x[[11]], a, after = 2)
a <- paste(x[[11]][[4]],x[[11]][[5]], sep = " ")
x[[11]] <- x[[11]][-c(4,5)]
x[[11]] <- append(x[[11]], a, after = 3)
a <- paste(x[[11]][[9]],x[[11]][[10]], sep = " ")
x[[11]] <- x[[11]][-c(9,10)]
x[[11]] <- append(x[[11]], a, after = 8)

a <- paste(x[[12]][[1]],x[[12]][[2]], sep = " ")
x[[12]] <- x[[12]][-c(1,2)]
x[[12]] <- append(x[[12]], a, after = 0)

x[[17]][9] <- "Presidente Fondazione Bellisario"

x[[18]][1] <- "Direttore Agenzia delle Entrate"
x[[18]][3] <- "Assogestioni"
x[[18]][6] <- "CONSOB"

x[[19]][1] <- "Stefano De Polis, IVASS"
x[[19]][3] <- "IVASS"
x[[19]][4] <- "Assogestioni"
x[[19]][5] <- "R.ETE. Imprese Italia"
x[[19]][6] <- "Alleanza delle cooperative italiane"
x[[19]][7] <- "CIA - Agricoltori Italiani"

x[[21]][2] <- "avv. Ernesto Maria Ruffini, direttore dell'Agenzia delle Entrate"

nomi <- x

rm(x)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 5
c6 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

