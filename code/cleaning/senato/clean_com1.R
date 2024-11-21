library("dplyr")
library("stringr")

# Carichiamo commissione1.csv in c1
c1 <- read.csv("[path]/audizioni_informali/data/raw_data/senato/commissione1.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c1$NOMI
nomi <- str_squish(nomi)

words_to_remove <- "DOCUMENTO|Memoria -|Memoria|slides|Dati minori stranieri non accompagnati -|Proposte normative|emd|SLIDES|UNSCP-2"
nomi <- gsub(words_to_remove, "", nomi)

nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

str_extract(nomi, ",")
x <- str_subset(nomi, ",")

p <- str_extract(nomi, " e ")
p <- which(p == " e ") #posizione elementi filtrati
x <- str_subset(nomi, " e ") #check manuale elementi da correggere: 7
nomi[117] <- gsub(" e ", " ; ", nomi[117]) #corrispondenza di x in p


p <- str_extract(nomi, "-")
x <- str_subset(nomi, "-")

#Dopo aver controllato la punteggiatura, possiamo procedere con la separazione dei singoli nomi.

nomi_splitted <- str_split(nomi, " ; ", simplify = FALSE)

m <- matrix(nrow=169, ncol=1)
for (i in 1:169) {
  m[i,] <- length(nomi_splitted[[i]])
}

single_name <- unlist(nomi_splitted)
p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(3,4,7,8,12,13,14,16,17,18,20,21,23,24,25,33,40,43,46)
for (i in y) {
  x[i] <- gsub("-", ",", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(2,4,6:8,10:14,16:21,24,25,27)
for (i in y) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

single_name <- gsub("_",",", single_name)
single_name[108] <- "Corte dei Conti"
single_name[164] <- "Francesco Paorici, Direttore generale AGID Agenzia per l'Italia Digitale"
single_name <- gsub("\\;$", "", single_name) %>% str_squish()

c1$times <- m[,1]

new_data <- c1[rep(1:nrow(c1), times = c1$times), ]
rownames(new_data) <- 1:nrow(new_data)

new_data[,2] <- single_name

new_data <- subset(new_data, select = -times)
new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)

x <- new_data$DATA
x <- gsub("^\\s","0", x)
x <- str_split_fixed(x, " ", 3)
x <- as.data.frame(x)

x$month <- with(x, ifelse(V2 %in% "Gennaio", 1,
                          ifelse(V2 %in% "Febbraio", 2,
                                 ifelse(V2 %in% "Marzo", 3,
                                        ifelse(V2 %in% "Aprile", 4,
                                               ifelse(V2 %in% "Maggio", 5,
                                                      ifelse(V2 %in% "Giugno", 6,
                                                             ifelse(V2 %in% "Luglio", 7,
                                                                    ifelse(V2 %in% "Agosto", 8,
                                                                           ifelse(V2 %in% "Settembre", 9,
                                                                                  ifelse(V2 %in% "Ottobre", 10,
                                                                                         ifelse(V2 %in% "Novembre", 11,
                                                                                                ifelse(V2 %in% "Dicembre", 12,
                                                                                                       "NA")))))))))))))


date <- paste(x$V1,x$month,x$V3, sep= "-")
new_data <- subset(new_data, select = -DATA)
new_data$DATA <- as.Date(date, format="%d-%m-%Y")

# salva new_data in C1.csv
write.csv(new_data, "[path]/audizioni_informali/data/clean_data/senato/C1.csv", row.names = FALSE)
