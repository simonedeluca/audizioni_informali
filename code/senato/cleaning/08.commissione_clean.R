library("dplyr")
library("stringr")

# Carichiamo commissione8.csv in c8
c8 <- read.csv("[path]/commissione8.csv", header = TRUE, stringsAsFactors = FALSE)


nomi <- c8$NOMI
nomi <- str_squish(nomi)

nomi <- gsub(", con particolare riferimento.*", "", nomi)
nomi <- gsub("sul piano.*", "", nomi)
nomi <- gsub("nell'ambito.*", "", nomi)
nomi <- gsub("sulla sperimentazione.*", "", nomi)
nomi <- gsub("sull'aumento.*", "", nomi)
nomi <- gsub("sulla nuova.*", "", nomi)
nomi <- gsub("sulla predisposizione.*", "", nomi)
nomi <- gsub("sull'impatto.*", "", nomi)
nomi <- gsub("sull'uso.*", "", nomi)
nomi <- gsub("sull'incidente.*", "", nomi)
nomi <- gsub("sullo stato.*", "", nomi)
nomi <- gsub("sulle attività.*", "", nomi)
nomi <- gsub("sull'attività.*", "", nomi)
nomi <- gsub("sulla situazione.*", "", nomi)
nomi <- gsub(", in relazione.*", "", nomi)
nomi <- gsub(", sul crollo.*", "", nomi)
nomi <- gsub("sul prospettato.*", "", nomi)


words_to_remove <- "Audizioni|informali|Audizione|informale|, in videoconferenza,|, in videoconferenza|di:|in videoconferenza:|in videoconferenza|di rappresentanti|Contributo|Trasmissione|contributo|dei vertici|, intervenuto|, intervenuti|SLIDES|PROPOSTE|Memoria|Proposte normative|emd|Seguito dell'audizione|pervenuto"
nomi <- gsub(words_to_remove, "", nomi)

nomi <- str_squish(nomi)
nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\,$", "", nomi) %>% str_trim(side = "right")
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "right")

x <- nomi
a <- c(7,8,9,15,19,36,38,40,41,42,46,47,55,56,71,72,73,95,97,98,102,121,125,130,133,145,147,148,150,163,164,182,192)
for (i in a) {
  x[i] <- gsub(",", ";", x[i])
}
for (i in a) {
  x[i] <- gsub(" e ", ";", x[i])
}

a <- c(22,25,29,37,101,110,111,112,116,119,135,137,143,149,154,160,162,173,176)
for (i in a) {
  x[i] <- gsub(" e ", ";", x[i])
}

a <- c(32,43,152,153,178)
for (i in a) {
  x[i] <- gsub(",", ";", x[i])
}

a <- c(44,45)
for (i in a) {
  x[i] <- gsub(",", ";", x[i])
}

for (i in a) {
  x[i] <- gsub(" E ", ";", x[i])
}

nomi <- x

nomi[2] <- "Corrado Gisonni, Commissario Straordinario per la sicurezza del sistema idrico del Gran Sasso ; Marco Corsini, Commissario per la messa in sicurezza delle autostrade A24 e A25 ; Sergio Fiorentino, Commissario ad acta per l'aggiornamento del Piano Economico Finanziario Autostrada A24/A25"
nomi[4] <- "Comitato Sindaci e Amministratori di Lazio e Abruzzo per la sicurezza e contro il caro-pedaggi A24-A25 ; Strada dei Parchi S.p.A."
nomi[28] <- "UICI Unione Italiana dei Ciechi e degli Ipovedenti ; ANITEC-ASSINFORM ; AIE Associazione Italiana Editori ; AgID Agenzia per l'Italia Digitale"
nomi[34] <- "Presidente RAI ; Amministratore Delegato RAI"
nomi[35] <- "CNA Cinema e Audiovisivo ; BANIJAY Group"
nomi[70] <- "ing. Francesco Caio, presidente di Italia TrasportoAereo Spa ; dott. Fabio Lazzerini, amministratore delegato di Italia TrasportoAereo Spa"
nomi[87] <- "ing. Francesco Caio, presidente di Italia TrasportoAereo Spa ; dott. Fabio Lazzerini, amministratore delegato di Italia TrasportoAereo Spa"
nomi[91] <- "TELT S.A.S. ; Regione Piemonte ; Piero Franco Nurisso, Sindaco di Gravere e Presidente dell'Unione Montana Alta Valle Susa ; Pier Giuseppe Genovese, Sindaco di Susa"
nomi[92] <- "Zeno D'Agostino ; Pino Musolino"
nomi[94] <- "professoressa Giovanna De Minico, Università degli Studi di Napoli Federico II ; professor Andrea Renda, CEPS Centro per gli Studi Politici Europei ; dottor Pietro Guindani, Presidente ASSTEL ; dottor Marco Gay, Presidente Anitec-Assinform"
nomi[96] <- "Angelo Mautone, Direttore generale per i sistemi di trasporto ad impianti fissi e il trasporto pubblico locale Ministero delle infrastrutture e dei trasporti"
nomi[100] <- "Rappresentazioni sindacali"
nomi[114] <- "Corte dei conti ; Autorità garante della concorrenza e del mercato"
nomi[128] <- "Presidente della Regione Veneto ; Presidente dell'Autorità di sistema portuale del Mare Adriatico Settentrionale ; Provveditorato Interregionale per le Opere Pubbliche per il Veneto, Trentino Alto Adige e Friuli Venezia Giulia ; ANCE"
nomi[131] <- "ENAV SpA ; ADR Aeroporti di Roma SpA ; dottoressa Olga SIMEON, Esperto Nazionale Commissione Europea"
nomi[140] <- "VODAFONE ; WIND TRE ; ASSTEL Assotelecomunicazioni ; CNCU Consiglio Nazionale dei Consumatori e degli Utenti"
nomi[141] <- "ANCAI ; SEA Aeroporti di Milano ; SAVE Aeroporti di Venezia e Treviso ; Legambiente"
nomi[144] <- "Corte dei conti ; ANCE ; Rete delle Professioni Tecniche ; ANCI ; UPI ; Conferenza delle regioni e delle province autonome ; INAIL"
nomi[146] <- "Confindustria servizi innovativi e tecnologici ; Consiglio Nazionale del Notariato ; Consiglio Nazionale Forense ; HP Italy"
nomi[157] <- "AISCAT ; FIPE Federazione Italiana Pubblici Esercizi ; ANCI ; Consiglio Superiore dei Lavori Pubblici ; INVITALIA ; CONFIMI INDUSTRIA Confederazione dell'Industria Manifatturiera Italiana e dell'Impresa Privata"
nomi[168] <- "ANCE ; ALLEANZA DELLE COOPERATIVE ITALIANE ; ASSOCIAZIONE NAZIONALE CONSULENTI DEL LAVORO ; UDIR ; ANIEF ; Anief ; COSMED ; ANAAO ASSOMED ; OPENFIBER ; FINCO ; GENERAL SOA"
nomi[198] <- "Direzione Generale della Motorizzazione civile ; Direzione Generale per la vigilanza sulle autorità portuali, le infrastrutture portuali ed il trasporto marittimo e per vie d'acqua interne"

p <- str_extract(nomi, "\\.$")
p <- which(!is.na(p))
x <- str_subset(nomi, "\\.$")

a <- c(2,3,6,13,15)
for (i in a) {
  x[i] <- gsub("\\.$", "", x[i])
}

output <- c()

for (i in a) {
  h <- p[i]
  output <- c(output, h)
}

for (i in a) {
  nomi[output] <- x[a] #sostituiti
}

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=199, ncol=1)
for (i in 1:199) {
  m[i,] <- length(nomi_splitted[[i]])
}

c8$times <- m[,1]

new_data <- c8[rep(1:nrow(c8), times = c8$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle|^de della"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

j <- c(1,2,3,4,9,11,12,13,14,15,17,18,19,20,22,23,24,25,29,31,32,33,34,35,36,37,38,39,40,41,42)
for (i in j) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in j) {
  h <- p[i]
  output <- c(output, h)
}

for (i in j) {
  single_name[output] <- x[j]
}

single_name <- gsub("//,$", "", single_name) %>% str_squish()
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

# salva new_data in C8.csv
write.csv(new_data, "[path]/C8.csv", row.names = FALSE)
