library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 33)

for (page_index in 1:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[3]] <- append(x[[3]], "Impatto della pandemia sugli studenti delle scuole secondarie", after = 4)

x[[4]] <- append(x[[4]], "Affare assegnato n. 808 (Impatto cambiamenti climatici sui beni culturali e sul paesaggio)", after = 0)

x[[9]] <- append(x[[9]], "Atto n. 621 - Impatto della didattica digitale integrata (DDI) sui processi di apprendimento e sul benessere psicofisico degli studenti", after = 4)

x[[17]] <- append(x[[17]], "Affare assegnato n. 245 (Volontariato e professioni nei beni culturali)", after = 4)

x[[20]] <- append(x[[20]], "AG N. 79 (DPR reclutamento AFAM)", after = 6)
x[[20]] <- append(x[[20]], "NA", after = 9)

x[[30]] <- x[[30]][-2]

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 33)

for (page_index in 1:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

a <- data

data_fill <- vector(mode = "list", length = 8)

for (page_index in 26:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data_ora_inizio a") %>% html_text()
  data_fill[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

for (i in 26:33) {
  data_fill[[i]] <- gsub("del", "", data_fill[[i]]) %>% #togli specific pattern
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

for (i in 26:33) {
  a[[i]] <- data_fill[[i]]
}

a[[26]][[1]] <- "26 settembre 2018"

data <- a
rm(data_fill)
rm(a)

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 33)

for (page_index in 1:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- "7 (Pubbl. istruzione)"
n <- c(1,4,5,6,7,9,13:19,21:25,32)

for (i in n) {
  x[[i]] <- rep(a, 10)
}

x[[2]] <- append(x[[2]], a, after = 0)
x[[2]] <- append(x[[2]], a, after = 8)
x[[2]] <- append(x[[2]], a, after = 9)

a <- rep("7 (Pubbl. istruzione)", 7)
x[[3]] <- append(x[[3]], a, after = 0)
x[[3]] <- append(x[[3]], "7 (Pubbl. istruzione)", after = 9)

a <- rep("7 (Pubbl. istruzione)", 4)
x[[8]] <- append(x[[8]], a, after = 0)
x[[8]] <- append(x[[8]], "7 (Pubbl. istruzione)", after = 8)

a <- rep("7 (Pubbl. istruzione)", 4)
x[[10]] <- append(x[[10]], a, after = 0)
a <- rep("7 (Pubbl. istruzione)", 5)
x[[10]] <- append(x[[10]], a, after = 5)

a <- rep("7 (Pubbl. istruzione)", 6)
x[[11]] <- append(x[[11]], a, after = 0)
a <- rep("7 (Pubbl. istruzione)", 2)
x[[11]] <- append(x[[11]], a, after = 7)

a <- rep("7 (Pubbl. istruzione)", 8)
x[[12]] <- append(x[[12]], a, after = 2)

a <- rep("7 (Pubbl. istruzione)", 5)
x[[20]] <- append(x[[20]], a, after = 0)
a <- rep("7 (Pubbl. istruzione)", 4)
x[[20]] <- append(x[[20]], a, after = 6)

a <- rep("7 (Pubbl. istruzione)", 8)
x[[26]] <- append(x[[26]], a, after = 0)
x[[26]] <- append(x[[26]], "7 (Pubbl. istruzione)", after = 9)

x[[27]] <- append(x[[27]], "7 (Pubbl. istruzione)", after = 3)
x[[27]] <- append(x[[27]], "7 (Pubbl. istruzione)", after = 5)
x[[27]] <- append(x[[27]], "7 (Pubbl. istruzione)", after = 7)
x[[27]] <- append(x[[27]], "7 (Pubbl. istruzione)", after = 8)

x[[28]] <- append(x[[28]], "7 (Pubbl. istruzione)", after = 1)
a <- rep("7 (Pubbl. istruzione)", 4)
x[[28]] <- append(x[[28]], a, after = 4)

a <- rep("7 (Pubbl. istruzione)", 2)
x[[29]] <- append(x[[29]], a, after = 0)
a <- rep("7 (Pubbl. istruzione)", 4)
x[[29]] <- append(x[[29]], a, after = 3)
x[[29]] <- append(x[[29]], "7 (Pubbl. istruzione)", after = 8)

a <- rep("7 (Pubbl. istruzione)", 3)
x[[30]] <- append(x[[30]], a, after = 0)
a <- rep("7 (Pubbl. istruzione)", 5)
x[[30]] <- append(x[[30]], a, after = 5)

x[[31]] <- append(x[[31]], "7 (Pubbl. istruzione)", after = 1)
a <- rep("7 (Pubbl. istruzione)", 3)
x[[31]] <- append(x[[31]], a, after = 4)

x[[33]][[1]] <- "7 (Pubbl. istruzione)"

commissione <- x

# 4. Estrazione nomi 1

nomi <- vector(mode = "list", length = 33)

for (page_index in 1:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

for (i in 1:33) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

# 5. Estrazioni nomi 2

nomi2 <- vector(mode = "list", length = 33)

for (page_index in 1:33) {
  link <- paste("https://www.senato.it/Leg18/3661?current_page_40431=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".titolo_pubblicato strong") %>% html_text()
  nomi2[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

y <- nomi2

for (i in 1:33) {
  y[[i]] <- gsub("[\n]", "", y[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

# Integrazione nomi1 e nomi2

y[[1]][10] <- x[[1]][6]

y[[2]][2] <- x[[2]][1]

y[[3]][8] <- x[[3]][5]
y[[3]][9] <- x[[3]][6]

y[[9]][6] <- x[[9]][6]

y[[14]][9] <- x[[14]][9]

y[[15]][6] <- x[[15]][5]

y[[26]][8] <- x[[26]][7]
y[[26]][10] <- x[[26]][8]

a <- paste(x[[27]][[1]],x[[27]][[2]], sep = " ")
y[[27]][2] <- a
a <- paste(x[[27]][[3]],x[[27]][[4]], x[[27]][[5]], sep = " ")
y[[27]][3] <- a
y[[27]][9] <- x[[27]][8]

y[[28]][8] <- x[[28]][5]
y[[28]][7] <- x[[28]][4]

y[[29]][1] <- x[[29]][1]
y[[29]][3] <- x[[29]][3]
a <- x[[29]][3]
y[[29]] <- append(y[[29]], a, after = 3)
y[[29]][7] <- x[[29]][5]

a <- paste(x[[30]][[1]],x[[30]][[2]], sep = " ")
y[[30]][6] <- a
y[[30]][9] <- x[[30]][4]

y[[31]][2] <- x[[31]][1]
a <- paste(x[[31]][[2]],x[[31]][[3]], sep = " ")
y[[31]][6] <- a
y[[31]][7] <- x[[31]][4]
y[[31]][8] <- x[[31]][5]

a <- paste(x[[32]][[1]],x[[32]][[2]], sep = " ")
y[[32]][1] <- a
y[[32]][2] <- x[[32]][3]
y[[32]][5] <- x[[32]][5]
y[[32]][8] <- x[[32]][6]
y[[32]][9] <- x[[32]][7]
y[[32]][10] <- x[[32]][8]

y[[33]][1] <- x[[33]][1]

nomi <- y

rm(x)
rm(y)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 7
c7 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

