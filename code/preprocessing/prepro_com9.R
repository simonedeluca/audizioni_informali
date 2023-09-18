library("dplyr")
library("stringr")

c9 <- read.csv("C:/Users/pc/Desktop/Progetto Audizioni/data/raw_data/commissione9.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c9$NOMI
nomi <- str_squish(nomi)

nomi[40] <- "Istituto superiore per la protezione e la ricerca ambientale (ISPRA) ; Assessore all'ambiente e all'energia e al settore tutela delle acque della regione Piemonte"
nomi[43] <- "Slow Food Italia ; professoressa Alessandra Corrado, associata di sociologia dell'ambiente e del territorio presso l'Università della Calabria"
nomi[44] <- "Autorità di bacino distrettuali delle Alpi orientali ; Autorità di bacino distrettuali del Fiume Po ; Autorità di bacino distrettuali dell'Appennino settentrionale ; Autorità di bacino distrettuali dell'Appennino meridionale ; Autorità di bacino distrettuali dell'Appennino centrale"
nomi[50] <- "Consiglio dell'Ordine nazionale dei dottori agronomi e dei dottori forestali (CONAF) ; Consiglio dell'ordine nazionale dei tecnologi alimentari (OTAN) ; Collegio nazionale dei periti agrari e periti agrari laureati (CNPAPAL)"
nomi[54] <- "Agrinsieme ; UNCI Agroalimentare ; Consorzio Tutela Vini Asolo Montello ; Consorzio Tutela del vino Conegliano Valdobbiadene Prosecco Superiore ; Consorzio di Tutela della DOC Prosecco ; Assessore all'agricoltura del Veneto ; Assessore all'agricoltura del Friuli Venezia Giulia ; Assessore all'agricoltura del Piemonte"
nomi[59] <- "Comune di Cetara ; Comune di Marsala ; Alleanza delle Cooperative italiane, Coordinamento pesca (AGCI Agrital Pesca-FedAgri Pesca-Federcoopesca-Legacoop agroalimentare Dipartimento pesca) ; UNCI Agroalimentare"
nomi[69] <- "prof. Giovanni Dinelli, professore ordinario Dipartimento di scienze e tecnologie agro alimentari Università di Bologna ; prof. Angelo Frascarelli, professore associato Dip. Scienze agrarie alimentari e ambientali Università di Perugia ; prof. Michele Pisante, professore ordinario di Agronomia e coltivazioni erbacee Università di Teramo"
nomi[70] <- "prof.ssa Daniela Romano, Professore ordinario di Orticoltura e floricoltura all'Università degli Studi di Catania ; prof. Francesco Ferrini, Professore ordinario di Arboricoltura Generale e Coltivazioni Arboree Full Professor of Arboriculture Membro del Senato Accademico dell'Università di Firenze Dipartimento di Scienze e Tecnologie Agrarie Alimentari Ambientali e Forestali (DAGRI)"
nomi[72] <- "Associazione italiana professionisti cinofili ; Associazione centro cinofilo FourDogs ; Associazione culturale ICCS Il Cane: cultura e sport"
nomi[73] <- "Istituto di Servizi per Mercato Agricolo Alimentare (ISMEA) ; Consiglio per la ricerca in agricoltura e l'analisi dell'economia agraria (CREA)"
nomi[85] <- "dott.ssa Lucia Piana, biologa esperta della qualità e dell'origine del miele ; Unione nazionale associazioni apicoltori italiani UNAAPI ; dott. Giancarlo Quaglia, direttore tecnico del laboratorio Floramo parte di Life Analytics"
nomi[98] <- "Mercato dei fiori della Toscana Pescia ; Mercato dei fiori di Sanremo ; Mercato dei fiori di Vittoria ; Mercato dei fiori di Pompei ; Mercato dei fiori di Taviano ; Mercato dei fiori di Viareggio ; Mercato dei fiori di Leverano ; Mercato dei fiori di Ercolano"
nomi[111] <- "Associazione Florovivaisti Veneti ; Associazione Florveneto ; Filiera Florovivaistica del Lazio ; Associazione Floricoltori e Fioristi italiani AFFI ; Asproflor ; Distretto florovivaistico del Ponente ligure ; Distretto rurale vivaistico ornamentale di Pistoia ; Distretto florovivaistico interprovinciale Lucca Pistoia"
nomi[112] <- "Origin Italia Associazione Italiana Consorzi Indicazioni Geografiche ; Federazione Italiana Strade del vino, dell'olio e dei sapori"
nomi[152] <- "Collegio nazionale degli agrotecnici e degli agrotecnici laureati ; Consiglio dell'Ordine nazionale dei dottori agronomi e dei dottori forestali (CONAF) ; Collegio nazionale dei periti agrari e periti agrari laureati (CNPAPAL)"
nomi[186] <- "Consiglio per la ricerca in agricoltura e l'analisi dell'economia agraria (CREA) ; Istituto Superiore di Sanità (ISS)"
nomi[189] <- "ANBI Friuli-Venezia Giulia ; ANBI Liguria ; ANBI Umbria ; ANBI Molise ; ANBI Sardegna"
nomi[190] <- "Dipartimento delle politiche competitive, della qualità agroalimentare, ippiche e della pesca del MIPAAFT ; Agrofarma-Federchimica ; Assoferilizzanti-Federchimic ; Federazione Italiana Dottori in scienze Agrarie e Forestali (FIDAF)"
nomi[197] <- "ANBI Valle d'Aosta ; ANBI Trentino Alto-Adige ; ANBI Marche ; ANBI Abruzzo ; ANBI Puglia ; ANBI Calabria"
nomi[200] <- "FILLEA CGIL ; prof. Paolo Maddalena, Vice Presidente emerito della Corte Costituzionale ; Forum nazionale salviamo il paesaggio - difendiamo i territori"
nomi[201] <- "Sindaco Santa Maria La Fossa ; Sindaco Castel Volturno ; Sindaco Grazzanise ; Sindaco Unione comuni Caserta Sud-Ovest ; Sindaco Capaccio Paestum ; Sindaco Albanella ; Sindaco Altavilla Silentina"
nomi[202] <- "Assoimmobiliare ; R.E.TE. Imprese Italia"
nomi[204] <- "Olimpolli Montagnani"
nomi[221] <- "CIA ; Associazione italiana per la wilderness A.I.W. ; Gruppo di intervento giuridico ; Accademia italiana di Permacultura" 
nomi[229] <- "Rete Professioni Tecniche"
nomi[230] <- "Agrinsieme (Confagricoltura, CIA, Copagri, Alleanza delle cooperative italiane-agroalimentare) ; Coldiretti ; UNCI Agroalimentare ; UECOOP"
nomi[232] <- "Autorità garante della concorrenza e del mercato"
nomi[234] <- "Confcommercio e Federalimentare"
nomi[235] <- "FIPSAS Federazione italiana pesca sportiva ed attività subacquee ; ARCI PESCA FISA Federazione italiana sport e ambiente ; Unione nazionale Enalcaccia pesca e tiro ; FIOPS Federazione italiana operatori pesca sportiva"
nomi[240] <- "DOTTOR ALBERTO SPAGNOLLI, SENIOR POLICY ADVISER EFSA (AUTORITÁ EUROPEA PER LA SICUREZZA ALIMENTARE)"
nomi[242] <- "FIPE (Federazione Italiana Pubblici Esercizi) ; Futuro Agricoltura"
nomi[243] <- "Agrinsieme (Confagricoltura, CIA, Copagri, Alleanza delle cooperative italiane-agroalimentare) ; Coldiretti ; UNCI Agroalimentare ; UECOOP"
nomi[245] <- "A.M.A. (Associazione Mediterranea Acquacoltori) ; FEDER OP.IT (Federazione delle Organizzazioni di Produttori della Pesca e dell'Acquacoltura Italiane) ; Federalimentare (Federazione italiana dell'industria alimentare)"
nomi[246] <- "UNCI Agroalimentare ; UECOOP ; API (Associazione Piscicoltori Italiani) ; UNICOOP PESCA ; Associazione Marinerie d'Italia e d'Europa ; Associazione Pescatori Marittimi Professionali (A.P.M.P.) ; Impresa Pesca Coldiretti"

nomi <- gsub("\\^:", "", nomi)
nomi <- gsub("\\^,", "", nomi)
nomi[218] <- gsub("\\)$", "", nomi[218])

words <- "in videoconferenza,|Documentazione|pervenuta|trasmessa|Rappresentanti|rappresentanti"
nomi <- gsub(words, "", nomi)

x <- c(2,9,15,17,21,35,39,46,48,56,58,60,62,63,65,75,76,78,100,108,113,114,115,119,143,148,149,150,166,170,171,172,173,176,196,214,215,218,222,236,247,248)
for (i in x){
  nomi[i] <- gsub(",", " ;", nomi[i])
}
for (i in x){
  nomi[i] <- gsub(" e ", " ; ", nomi[i])
}

x <- c(68,77,79,96,129,142,147,158,160,161,168,174,177,193,194,203,213,216,219,223)
for (i in x){
  nomi[i] <- gsub(" e ", " ; ", nomi[i])
}

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=254, ncol=1)
for (i in 1:254) {
  m[i,] <- length(nomi_splitted[[i]])
}

c9$times <- m[,1]

new_data <- c9[rep(1:nrow(c9), times = c9$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle|^de della"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")
single_name

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

j <- c(3,4,5,8,9,10,13,15,16,17,18,19,20,21,22,23,24,25,27,28,29,34,35,37,39,42,43,44,45,46,47,48,49,50,51)
for (i in j) {
  x[i] <- gsub("-", " ", x[i])
}

x[32] <- "Dott. Lorenzo Tosi, Società AGREA Verona"
j <- c(3,4,5,8,9,10,13,15,16,17,18,19,20,21,22,23,24,25,27,28,29,32,34,35,37,39,42,43,44,45,46,47,48,49,50,51)

output <- c()
for (i in j) {
  h <- p[i]
  output <- c(output, h)
}

for (i in j) {
  single_name[output] <- x[j]
}

single_name[311] <- gsub("\\)$", "", single_name[311])
single_name[334] <- gsub("\\:", "", single_name[334])
single_name[457] <- gsub("\\:", "", single_name[457])
single_name <- str_squish(single_name)
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
                                                                                  ifelse(V2 %in% "settembre", 9,
                                                                                         ifelse(V2 %in% "Ottobre", 10,
                                                                                                ifelse(V2 %in% "Novembre", 11,
                                                                                                       ifelse(V2 %in% "Dicembre", 12,
                                                                                                              "NA"))))))))))))))


date <- paste(x$V1,x$month,x$V3, sep= "-")
new_data <- subset(new_data, select = -c(DATA,times))
new_data$DATA <- as.Date(date, format="%d-%m-%y")

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

new_data <- new_data[-c(72,245),]
rownames(new_data) <- 1:nrow(new_data)

write.csv(new_data, "C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/C9.csv", row.names = FALSE)

