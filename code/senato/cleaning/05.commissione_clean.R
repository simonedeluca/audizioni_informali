library("dplyr")
library("stringr")

# Carichiamo commissione5.csv in c5
c5 <- read.csv("[path]/commissione5.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c5$NOMI
nomi <- str_squish(nomi)

p <- str_extract(nomi, "Ragioneria")
p <- which(p == "Ragioneria")

x <- "Ministero dell'Economia e delle Finanze Ragioneria generale dello Stato"
for (i in p) {
  nomi[i] <- x
}

p <- str_extract(nomi, "Relazione tecnica")
p <- which(p == "Relazione tecnica")

for (i in p) {
  nomi[i] <- x
}

nomi[6] <- "Lilia Cavallari, presidente Ufficio parlamentare di bilancio UPB"
nomi[13] <- "Mara Carfagna, ministro per il Sud e la coesione territoriale"
nomi[49] <- x
nomi[51] <- "ASVIS ; Salvatore Scalia, commissario straordinario sisma area etnea ; UPI ; UNCEM ; CONFERENZA DELLE REGIONI E PROVINCE AUTONOME ; ANCI ; Gaetano Armao, vicepresidente Regione Sicilia ; CISL ; UGL ; ABI ; CGIL ; ANIA ; ISTAT ; CONFINDUSTRIA ; UIL ; Giovanni Legnini, commissario straordianario sisma 2016 ; SPORT E SALUTE ; UPI"
nomi[56] <- "OCSE"
nomi[58] <- x
nomi[72] <- x
nomi[74] <- "Ministero dell'Economia e delle Finanze"
nomi[75] <- "Pierpaolo Sileri, sottosegretario di Stato alla salute"
nomi[77] <- "Ministero dell'Economia e delle Finanze Ufficio del coordinamento legislativo"
nomi[78] <- "Laura Castelli, vice ministro dell'economia e delle finanze ; Carmine Di Nuzzo, dirigente della Ragioneria generale dello Stato ; Nunzia Vecchione, dirigente della Ragioneria generale dello Stato"
nomi[79] <- "Ministro delle infrastrutture e della mobilit? sostenibile"
nomi[148] <- "Dario Scannapieco, vicepresidente della Banca europea per gli investimenti"
nomi[160] <- "Ministero dell'Economia e delle Finanze"
nomi[161] <- "UPI ; CONFERENZA REGIONI E PROVINCE AUTONOME ; ISTAT ; CGIL ; UIL ; UGL ; CONFINDUSTRIA ; CONFESERCENTI ; CONFARTIGIANATO IMPRESE ; ALLEANZA DELLE COOPERATIVE ITALIANE ; CIA ; COPAGRI ; CNA ; CONFAGRICOLTURA ; BANCA D'ITALIA ; CNEL ; UPB ; CONFCOMMERCIO ; CORTE DEI CONTI"
nomi[166] <- "Giuseppe Conte, Presidente del Consiglio ; Roberto Gualtieri, Ministro dell'Economia e delle Finanze"
nomi[178] <- "Banca d'Italia ; Ufficio parlamentare di bilancio ; Confetra ; Agrinsieme ; Nesl? Italiana ; Assoimmobiliare ; Sistema Gioco Italia ; Udir ; Bormioli pharma ; Assolatte ; Assica ; Assobibe ; Lapet ; Federalimentare ; Federagenti ; Assindatcolf ; Assiterminal ; Garante per la protezione dei dati personali ; Assarmatori"
nomi[179] <- "ABI ; Alleanza Cooperative Italiane ; Coldiretti ; CIA ; Confagricoltura ; Consiglio Nazionale dei Dottori Commercialisti e degli Esperti Contabili ; CGIL ; CISL ; UIL ; UGL ; Confindustria ; Rete Imprese Italia ; ANCI ; UPI ; Conferenza delle Regioni e delle Province Autonome ; Corte dei Conti ; ISTAT ; CNEL ; Associazione Nazionale Famiglie Numerose ANFN ; Associazione Nazionale Industria Autonoleggio e Servizi Automobilistici ANIASA ; Confederazione Italiana Dirigenti e Alte Professionalit? CIDA ; Unione Nazionale rappresentanti autoveicoli esteri UNRAE ; Associazione Aziende Pubblicitarie Italiane AAPI"
nomi[180] <- "Forum Terzo Settore ; ANIA ; ANPCI ; ANCIM ; Sbilanciamoci! ; WWF ; Legambiente ; Carlo Cottarelli, Osservatorio sui conti pubblici italiani ; Unaitalia ; Energia Nazionale"
nomi[181] <- "ANCE ; Confedilizia ; Confapi ; Confimi Industria ; Confprofessioni ; Federdistribuzione ; Anaao Assomed ; Finco ; Federpropriet? ; Associazione Family Day ; Filiera Immobiliare Re Mind ; Federfardis"
nomi[184] <- "Ministero dell'Economia e delle Finanze Ufficio del coordinamento legislativo"
nomi[185] <- "Ministro della difesa ; Ministro dell'Economia e delle Finanze"
nomi[186] <- "Corte dei Conti ; Ufficio parlamentare di bilancio ; Banca d'Italia ; ISTAT"
nomi[187] <- "CNEL"
nomi[196] <- "Inps ; Fabrizia Lapecorella, direttore generale delle finanze ; Ufficio parlamentare di bilancio"
nomi[198] <- "Giovanni Tria, Ministro dell'Economia e delle Finanze"
nomi[207] <- "CNEL ; ISTAT ; Banca d'Italia ; Ufficio parlamentare di bilancio"
nomi[209] <- "Corte dei Conti"
nomi[210] <- "Giovanni Tria, Ministro dell'Economia e delle Finanze"
nomi[211] <- "Confapi ; Alleanza delle Cooperative Italiane ; Copagri ; SVIMEZ ; CGIL ; CISL ; UIL ; UGL ; Confindustria ; Rete Imprese Italia ; ANCI ; Conferenza delle Regioni e delle Province Autonome ; Sbilanciamoci! ; UPI"
nomi[212] <- "ISTAT ; SVIMEZ ; Ministero dell'Economia e delle Finanze"
nomi[233] <- "Giovanni Tria, Ministro dell'Economia e delle Finanze"
nomi[234] <- "Giovanni Tria, Ministro dell'Economia e delle Finanze ; Banca d'Italia ; ISTAT ; Corte dei Conti"
nomi[235] <- "Ufficio parlamentare di bilancio"
nomi[246] <- "Ufficio parlamentare di bilancio"
nomi[247] <- "Corte dei Conti"

words_to_remove <- "Memoria - |Osservazioni|- PROPOSTE EMENDATIVE|- MEMORIA|- 2|- Allegato statistico|Audizione|Emendamenti|Emendamento|- Intervento del presidente|Terme|- emendamenti|Documento|Emendamenti|- Emendamenti|- Integrazione|Ordine del giorno|- LAVORO|- TURISTICO RICETTIVE|- FARMACISTI|- ACCESSO AL CREDITO| - EMENDAMENTI|integrazione|Memoria|- ALLEGATO|- Documento|audizione|- Allegato|- ALLEGATO STATISTICO|- Relazione illustrativa|dei rappresentanti|rappresentanti|Circolare 10 2021|- Intervento del Presidente|unitaria"
nomi <- gsub(words_to_remove, "", nomi)
nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

words1 <- " 1 | 2 "
nomi <- gsub(words1, "", nomi)

x <- lapply(nomi, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:248) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

n <- c(2,10,19,20,22,41:45,50:53,60,61,78,81,86:97,100,104:107,109,111,114:118,121,128,130,131,133,134,137,141:145,149,150,152,161,164,166,167,178:181,185,186,196,207,211,212,227,228,234)

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

z <- c(1:248)
z <- z[!(z %in% n)]

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in z) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

m <- matrix(nrow=248, ncol=1)

for (i in 1:248) {
  m[i,] <- length(x[[i]])
}

c5$times <- m[,1]

new_data <- c5[rep(1:nrow(c5), times = c5$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(x)

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(2,3,11,17)

for (i in y) {
  x[i] <- gsub("-", "", x[i])
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

y <- c(2,3,4,6)

for (i in y) {
  x[i] <- gsub(" - ", ", ", x[i])
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

single_name[457] <- "Prof. Mauro Cappello, docente Master AIGEP Universit? degli Studi della Tuscia"
single_name[721] <- gsub("-", ".", single_name[721])

p <- str_extract(single_name, " e ")
p <- which(p == " e ")
x <- str_subset(single_name, " e ")

y <- c(67,107,121)

for (i in y) {
  x[i] <- gsub(" e ", "-", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}

single_name[917] <- "Confederazione Italiana Dirigenti e Alte Professionalità CIDA"
single_name[1029] <- "CGIL-CISL-UIL"

single_name <- gsub("\\.$", "", single_name)
single_name <- gsub("\\,$", "", single_name) %>% str_squish()

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

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

# salva new_data in C5.csv
write.csv(new_data, "[path]/C5.csv", row.names = FALSE)
