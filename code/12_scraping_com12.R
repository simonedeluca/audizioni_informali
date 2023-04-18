library("rvest")
library("dplyr")
library("stringr")

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


words_to_remove <- "Documentazione|Memoria - |Audizione|AUDIZIONE|Contributo fornito|DOCUMENTO|Slides|informale|fornita|inviata|depositata|pervenuta|trasmessa|consegnata|da rappresentanti|da parte|di rappresentanti|rappresentanti|Osservazioni trasmesse|Ulteriore documentazione|Documento|depositato|trasmesso|Relazione|Documenti depositati|Osservazioni trasmesse"
for (i in 1:19) {
  x[[i]] <- gsub(words_to_remove, "", x[[i]]) %>%
    str_trim(side = "left")
}

# Strategia per pulire la colonna degli auditi

a <- x[[15]]
a <- str_squish(a) #remove more than one whitespace
a <- gsub("\\s*\\([^\\)]+\\)"," -", a)
a <- gsub("\\-$", "", a)

a <- str_split(a, "-", simplify = TRUE)

my_fun <- function(x) {
  str_trim(side = "both", x)
}
a <- apply(a, c(1, 2), my_fun)

a <- t(a)
a <- apply(a, 2, unique)

a <- lapply(a, function(x) x[!x %in% ""])

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

a <- lapply(a, function(x) gsub(words2, "", x))

a <- lapply(a, function(x) str_trim(x, side = "left"))

a <- lapply(a, function(x) paste(x, collapse = " - "))

