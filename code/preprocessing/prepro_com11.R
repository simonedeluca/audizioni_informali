library("dplyr")
library("stringr")

c11 <- read.csv("C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione11.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c11$NOMI
nomi <- str_squish(nomi)

nomi[57] <- "Presidente di Sport e Salute S.p.A. ; Presidente dell'INPS ; Presidente del Consiglio direttivo della divisione calcio paralimpico e sperimentale (DCPS) ; Associazione giocatori italiani basket associati (GIBA) ; Associazione nazionale atlete (ASSIST) ; Associazione italiana calciatori (AIC) ; Associazione italiana pallavolisti (AIP)"
nomi[83] <- "Consiglio nazionale dei dottori commercialisti dei dottori commercialisti e degli esperti contabili ; Consiglio nazionale dell'Ordine consulenti del lavoro ; Associazione nazionale commercialisti"
nomi[108] <- "Associazione nazionale vittime civili di guerra ; Associazione nazionale mutilati e invalidi civili ; Federazione tra le associazioni nazionali delle persone con disabilità"
nomi[111] <- "Consiglio nazionale Ordine degli assistenti sociali ; Conferenza delle Regioni e delle Province autonome ; ANCI ; UPI ; INPS ; ADAPT ; ISTAT ; ADEPP ; Lottomatica ; ACADI ; ANIEF ; UDIR ; Corte dei Conti"
nomi[114] <- "Conferenza delle Regioni e delle Province autonome ; ANCI ; CIDA ; CONFEDIR ; CODIRP ; ANSEB ; FIPE ; Corte dei Conti ; ADAPT"
nomi[133] <- "Amnesty International ; prof. Cesare Damiano"

nomi <- gsub("sul ddl.*", "", nomi)

x <- c(2,13,24,39,51,61,67,68,70,74,87,93,104,105,117,124,129)
for (i in x) {
  nomi[i] <- gsub(" e ", " ; ", nomi[i])
}

x <- c(6,10,11,12,43,44,52,53,55,58,63,77,78,84,85,92,96,101,102,103,105,109,110,112,115,120,122,132,134,146,148,151)
for (i in x) {
  nomi[i] <- gsub(",", " ;", nomi[i])
}
for (i in x) {
  nomi[i] <- gsub(" e ", " ; ", nomi[i])
}

words <- "rappresentanti|professori|Memorie"
nomi <- gsub(words, "", nomi) %>% str_trim("left")
nomi <- gsub("\\.$", "", nomi) %>% str_trim(side = "right")

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=151, ncol=1)
for (i in 1:151) {
  m[i,] <- length(nomi_splitted[[i]])
}

c11$times <- m[,1]

new_data <- c11[rep(1:nrow(c11), times = c11$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

single_name[525] <- "AIAT Associazione italiana agenzie teatrale e spettacolo"
single_name[529] <- "AWI Art Workers Italia"
single_name[532] <- "UNAMS Unione nazionale arte musica e spettacolo"

new_data[,2] <- single_name

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
new_data <- subset(new_data, select = -c(DATA,times))
new_data$DATA <- as.Date(date, format="%d-%m-%y")

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

write.csv(new_data, "C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/C11.csv", row.names = FALSE)
