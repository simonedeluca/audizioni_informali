library("dplyr")
library("stringr")

# Carichiamo commissione6.csv in c6
c6 <- read.csv("[path]/commissione6.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c6$NOMI
nomi <- str_squish(nomi)

x <- c("Daniele Franco, ministro dell'economia e delle finanze","Confcommercio","Alleanza Cooperative Italiane","ANCE","Casartigiani","CGIL","CISL","CNA","CNDCED","CODIRP")
y <- c(1:10)
z <- c(41:50)
for (i in y) {
  nomi[z] <- x[y]
}

x <- c("Confartigianato","CONFEDIR-CISAL","Confimi Industria","PMI","Consulenti del lavoro","Ordine dei revisori legali","Forum Terzo Settore","Confindustria","Associazione Nazionale Commercialisti ANC","Unione Nazionale Revisori Legali")
y <- c(1:10)
z <- c(51:60)
for (i in y) {
  nomi[z] <- x[y]
}

x <- c("LAPET","Assoprofessioni","Assosoftware","Federterziario","FNO TSRM PSTRP","INT-Confassociazioni","UDIR","UCID")
y <- c(1:8)
z <- c(62:69)
for (i in y) {
  nomi[z] <- x[y]
}

x <- c("ANCE","ABI","A.N.T.I.C.O.","Banca d'Italia","SAP","Guardia di Finanza","ANCE","Istituto Nazionale Tributaristi","Ufficio parlamentare di bilancio")
y <- c(1:9)
z <- c(132:140)
for (i in y) {
  nomi[z] <- x[y]
}

x <- c("Banca d'Italia UIF","Dogane","DNA","World Customs Organization","Claudio Clemente, direttore UIF Banca d'Italia", "Gen. C.A. Giuseppe Zafarana, Comandante Generale della Guardia di Finanza", "Federico Cafiero de Raho, Procuratore nazionale antimafia e antiterrorismo DNA","Consiglio Nazionale dei Dottori Commercialisti e degli Esperti Contabili","ABI")
y <- c(1:9)
z <- c(151:159)
for (i in y) {
  nomi[z] <- x[y]
}

x <- c("Banca d'Italia","Assofiduciaria","DIA","OAM","A.N.T.I.C.O.","SOSE","SOGEI","Valore D","Lella Golfo, presidente Fondazione Bellisario","Guardia di Finanza")
y <- c(1:10)
z <- c(160:169)
for (i in y) {
  nomi[z] <- x[y]
}

nomi[18] <- "Daniele Franco, ministro dell'economia e delle finanze"
nomi[24] <- "Alessandra Dal Verme, Direttore dell'Agenzia del Demanio"
nomi[90] <- "Dott.ssa Giorgia Maffini ; Pascal Saint-Amans, director centre for tax policy and administration OCSE"
nomi[91] <- "Giulio Tremonti ; Prof. Fabrizia Lapecorella, direttore Dipartimento delle finanze"
nomi[92] <- "ASSOFONDOPENSIONE ; ANASF ; Avv. Massimo Basilavecchia ; Prof. Paola Profeta, Università Bocconi"
nomi[95] <- "Prof. Corasaniti, Università degli Studi di Brescia ; Prof. Giuseppe Melis, LUISS ; Prof. Carlo Fiorio, Università degli Studi di Milano e IRVAPP-FBK"
nomi[101] <- "Prof. Baldini, Università di Modena e Reggio Emilia"
nomi[106] <- "dottor Guido CARLINO, presidente della Corte dei Conti ; prof. Massimo BORDIGNON, Università Cattolica & European Fiscal Board ; prof. Giuseppe VEGAS"
nomi[116] <- "Avv. Ernesto Maria Ruffini, Direttore dell'Agenzia delle entrate"
nomi[117] <- "Banca d'Italia"
nomi[120] <- "Dott.ssa Savigni, Ordine dei Dottori Commercialisti e degli Esperti Contabili Sardegna"
nomi[148] <- "Consiglio Nazionale Dei Dottori Commercialisti e Degli Esperti Contabili"
nomi[149] <- "Antonino Maggiore, Direttore dell'Agenzia delle entrate"
nomi[150] <- "Ministero dell'economia e delle finanze"
nomi[187] <- "Coldiretti"
nomi[190] <- "Antonino Maggiore, Direttore dell'Agenzia delle entrate ; Consiglio Nazionale Ordine Commercialisti ed Esperti Contabili ; Prof. Maurizio LEO ; Prof. Fabrizia Lapecorella, direttore Dipartimento delle finanze ; Corte dei Conti"
nomi[197] <- "G. Pitruzzella, Presidente dell'Autorità Garante della concorrenza e del mercato ; Dott. Claudio Clemente, Direttore dell'Unità di informazione finanziaria per l'Italia UIF"
nomi[204] <- "Commissione Europea"
nomi[205] <- "Dogane"
nomi[206] <- "IVASS"

words_to_remove <- "- Memoria integrativa|Memoria|Audizione|- Presentazione|- Allegato|- Integrazione|e chiarimenti|- Proposte emendative| 1| 2a| 2b| 2c| 2d| 2| 3| 4| n.|- Documento|Documento|Documentazione|depositato|depositata|trasmesso|- ALLEGATO|- allegato|Relazione|Proposte riforma fiscale -|di rappresentanti|dai rappresentanti|rappresentanti|- Emendamenti|Emendamenti|Emendamento|- Intervento|Intervento|del Presidente -|del Presidente|- Presidente|Terme|- emendamenti|Ordine del giorno|Circolare 10 2021|- LAVORO|- TURISTICO RICETTIVE|- FARMACISTI|- ACCESSO AL CREDITO|- EMENDAMENTI|Intervento -|Intervento|- Traduzione in italiano|: Allegato|: allegato|Allegato|alla memoria|depositata|Piattaforma|sui temi fiscali|Sintesi dell'intervento -|- Ipotesi per un nuovo modello fiscale -|Nota esplicativa -|PROPOSTE EMENDAMENTI|- Osservazioni|- Punta Cana Resolution|-Proposte emendative|- Slide|audizione|Audiizione"
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

x <- lapply(nomi, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:206) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

n <- c(1:13,35,61,75,76,83:95,97:100,102:112,128,188:190,192:194,197,199,200)

for (i in n) {
  x[[i]] <- apply(x[[i]], 2, unique)
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in n) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

z <- c(1:206)
z <- z[!(z %in% n)]

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}



m <- matrix(nrow=206, ncol=1)

for (i in 1:206) {
  m[i,] <- length(x[[i]])
}

c6$times <- m[,1]

new_data <- c6[rep(1:nrow(c6), times = c6$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(x)

single_name[226] <- "Associazione nazionale fra le banche popolari"
single_name[337] <- "ASSONIME"
single_name[356] <- "Consiglio Nazionale dei Dottori Commercialisti e degli Esperti Contabili"
single_name[510] <- "Federico Cafiero de Raho, Procuratore Nazionale Antimafia e Antiterrorismo DNA"

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(2,3,26,31,33)
for (i in y) {
  x[i] <- gsub("-", ",", x[i])
}
x[34] <- "Prof. Mauro MASI, CONSAP"
x[35] <- "Giuseppe MARESCA, MEF"
x[31] <- "Gen. C.A. Giuseppe Zafarana, Comandante Generale della Guardia di Finanza"
x[32] <- "Prof. Fabrizia Lapecorella, direttore Dipartimento delle finanze"
x[38] <- "Prof. Fabrizia Lapecorella, direttore Dipartimento delle finanze"

y <- c(5,6,7,9,12,13,14,17,18,19,20,21,22,36,37)
for (i in y) {
  x[i] <- gsub("-", " ", x[i])
}

y <- c(2,3,26,31,33,5,6,7,9,12,13,14,17,18,19,20,21,22,36,37)
output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

p <- str_extract(single_name, " e ")
p <- which(p == " e ")
x <- str_subset(single_name, " e ")

x[11] <- "OCST-UNIA, sindacati svizzeri"
x[14] <- "Col. Naz. Maestri Sci - AMSI"
x[20] <- "CGIL-CISL-UIL"
x[23] <- "Agenzia delle dogane e dei monopoli"
y <- c(11,14,20,23)
output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

p <- str_extract(single_name, "CGIL")
p <- which(p == "CGIL")
x <- str_subset(single_name, "CGIL")
single_name[270] <- "CGIL-CISL-UIL"

single_name <- str_squish(single_name)
new_data[,2] <- single_name
new_data <- new_data[-28,]

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

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

# salva new_data in C6.csv
write.csv(new_data, "[path]/C6.csv", row.names = FALSE)
