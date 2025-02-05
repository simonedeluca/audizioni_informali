library("dplyr")
library("stringr")

# Carichiamo commissione3.csv in c3
c3 <- read.csv("[path]/commissione3.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c3$NOMI
nomi <- str_squish(nomi)

nomi[12] <- "Benedetto Della Vedova, sottosegretario di Stato per gli affari esteri e la cooperazione internazionale"
nomi[31] <- "Mauro Battocchi, ambasciatore d'Italia in Cile"
nomi[39] <- "Ambasciatore d'Israele in Italia, S.E. Dror Eydar"
nomi[41] <- "Pasquale Tridico, presidente INPS"
nomi[48] <- "Triageduepuntozero"
nomi[53] <- "Nodo di Gordio"
nomi[55] <- "Partecipazione e Sviluppo"
nomi[44] <- "Paolo Glisenti, commissario generale di sezione per EXPO 2020 Dubai"
nomi[59] <- "IAI Istituto Affari Internazionali"
nomi[67] <- "ICE"

words_to_remove <- "Relazione|Intervento|\\([^()]*\\Documento unitario[^()]*\\)|\\([^()]*\\Memoria[^()]*\\)|audizione Senato|- Intervento|di rappresentanti di|Audizione informale|in videoconferenza|Documentazione depositata dal|- Memoria|- Memorie|Memoria|Memorie|dell'"

nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

nomi_splitted <- str_split(nomi, " ; ", simplify = FALSE)

m <- matrix(nrow=73, ncol=1)
for (i in 1:73) {
  m[i,] <- length(nomi_splitted[[i]])
}

c3$times <- m[,1]

new_data <- c3[rep(1:nrow(c3), times = c3$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

str_subset(single_name, ",") #controllo punteggiatura

single_name[3] <- "OCST-UNIA, sindacati svizzeri"
single_name[16] <- "Movimento Federalista europeo-Associazione Gioventù Federalista europea"
single_name[7] <- "Pier Virgilio Dastoli, CIME Consiglio Italiano Movimento Europeo"
single_name[47] <- "CICM Campagna Italiana contro le Mine ONLUS"

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"
single_name <- gsub(words2, "", single_name) %>% str_squish()

single_name[52] <- "Marco Petacco, console Generale a Buenos Aires"
single_name[53] <- "Antonio Petrarulo, console Generale a Bahia Blanca"

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
new_data$DATA <- as.Date(date, format="%d-%m-%Y")

# salva new_data in C3.csv
write.csv(new_data, "[path]/C3.csv", row.names = FALSE)
