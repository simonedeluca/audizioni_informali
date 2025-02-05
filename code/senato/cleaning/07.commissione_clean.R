library("dplyr")
library("stringr")

# Carichiamo commissione7.csv in c7
c7 <- read.csv("[path]/commissione7.csv", header = TRUE, stringsAsFactors = FALSE)


nomi <- c7$NOMI
nomi <- str_squish(nomi)

words_to_remove <- "Documento|trasmesso|pervenuto|Audizione|Audizioni|informale|informali|di rappresentanti|i rappresentanti|rappresentanti|di:|Memoria -|esperti:|di:|\\([^()]*\\in videoconferenza[^()]*\\)|Documentazione|trasmessa|audizione|Seguito|23.07.2020|delle comunicazioni|dell'audizione|Comunicazioni"
nomi <- gsub(words_to_remove, "", nomi)

nomi <- gsub("in merito.*", "", nomi)
nomi <- gsub("sull'impatto.*", "", nomi)
nomi <- gsub("sullo stato.*", "", nomi)
nomi <- gsub("sulle linee.*", "", nomi)
nomi <- gsub("sui contenuti.*", "", nomi)
nomi <- gsub("congiuntamente.*", "", nomi)
nomi <- gsub("sulle iniziative.*", "", nomi)
nomi <- gsub("sul disegno.*", "", nomi)
nomi <- gsub("sull'avvio.*", "", nomi)
nomi <- gsub("sull'utilizzo.*", "", nomi)
nomi <- gsub("sull'attuazione.*", "", nomi)

nomi <- gsub("\\([^()]*\\d+[^()]*\\)",";", nomi)
nomi <- gsub("\\;$", "", nomi) %>% str_trim(side = "both")

nomi[38] <- "Associazione SOS Archivi ; Associazione nazionale archivistica italiana (ANAI) ; Direttrice della Scuola di Specializzazione in \"Beni archivistici e librari\" ; Prof. Giovanni Paoloni, esperto"
nomi[243] <- "Corte dei Conti"
nomi[295] <- "IPSEOA Cornaro di Jesolo (Verona) ; ITG Belzoni Padova ; ITIS A. Rossi Vicenza ; MOTUS-E"
nomi[298] <- "Fondazione Homo Viator ; Cammino dei Monti e dei Santi ; Alleanza Cooperative italiane turismo e beni culturali ; Comitato promotore San Michele Cammino dei Cammini ; Associazione I Cammini di Francesco"

p <- str_extract(nomi, ",")
p <- which(p == ",")
x <- str_subset(nomi, ",")

a <- c(3,5,8,9,10,11,12,13,14,15,17,27,28,29,31,35,36,37,43,46,52,54,56,63,64,68,70,74,77,78,81,84,85,86,91,98,100,101,102,104,105,113,115,116,117,119,120,121,122,123,125,127,128,129,130,132,137,144) #,+e
for (i in a) {
  x[i] <- gsub(",", ";", x[i])
}
for (i in a) {
  x[i] <- gsub(" e ", ";", x[i])
}

b <- c(1,4,7,16,22,24,30,39,42,44,45,55,94,112,131) #e
for (i in b) {
  x[i] <- gsub(" e ", ";", x[i])
}

c <- c(41,83,88,97,108) #,
for (i in c) {
  x[i] <- gsub(",", ";", x[i])
}

z <- c(3,5,8,9,10,11,12,13,14,15,17,25,27,28,29,31,35,36,37,43,46,52,54,56,63,64,68,70,74,77,78,81,84,85,86,91,98,100,101,102,104,105,113,115,116,117,119,120,121,122,123,125,127,128,129,130,132,137,144,1,4,7,16,22,24,30,39,42,44,45,55,94,112,131,41,83,88,97,108)
z <- sort(z)

output <- c()
for (i in z) {
  h <- p[i]
  output <- c(output, h)
}

for (i in z) {
  nomi[output] <- x[z] #sostituiti
}

n <- c(1:145)
d <- n[!(n %in% z)] #indici in x da controllare

x_check <- x[d]

tot <- c(1:66)
ok <- c(3,4,6,7,8,12,13,15,37,40,42,43,45,49,50,56,57,58,59,60,61,62,63,64,65,66)
mod <- tot[!tot %in% ok] #indici in x_check da modificare

u <- x_check[mod]

u[1] <- "Istituto per la promozione e valorizzazione della dieta mediterranea (IDIMED) ; Federazione italiana cuochi ; prof. Petrillo, esperto"
u[2] <- "Academia Barilla ; ALMA S.r.l. Scuola internazionale di cucina italiana ; Associazione italiana di dietetica e nutrizione clinica Sez. Sicilia"
u[3] <- "Associazione bancaria italiana (ABI) ; Commissione nazionale per la società e la borsa (CONSOB) ; Federazione italiana delle banche di credito cooperativo casse rurali ed artigiane (FEDERCASSE)"
u[4] <- "Istituto Restauro Roma (IRR) ; Labirinto della Masone di Fontanellato ; Associazione nazionale musei di enti locali e istituzionali (ANMLI)"
u[5] <- "Consiglio nazionale degli Attuari ; Federazione degli Ordini dei farmacisti italiani (FOFI) ; Associazione medici dirigenti (ANAAO ASSOMED) ; Consiglio nazionale ordine assistenti sociali (CNOAS) ; Consiglio nazionale dei geologi ; Collegio nazionale degli agrotecnici e degli agrotecnici laureati"
u[6] <- "Associazione dottorandi e dottori di ricerca (ADI) ; Associazione nazionale docenti universitari (ANDU) ; Associazione ricercatori tempo determinato (ARTeD) ; Università manifesta ; Comitato nazionale universitario (CNU) ; Rete 29 Aprile"
u[7] <- "Direttore Generale della Direzione generale dei rapporti lavoro delle relazioni industriali del Ministero del lavoro e delle politiche sociali ; Ispettorato nazionale del lavoro ; Associazione Comma 2"
u[8] <- "prof. Pileri ; dott.ssa Zanni ; dottor Attolico ; Federtrek"
u[9] <- "Associazione italiana Via Romea Germanica ; Sviluppumbria ; Rete Cammini del Sud ; Associazione movimento lento ; Amici del Cammino di Sant'Agostino ; dott. Bambi, esperto"
u[10] <- "scrittore Paolo Rumiz ; dottor Federico Ignesti, assessore al turismo dell'Unione montana dei comuni del Mugello ; Associazione cammino di San Francesco di Paola ; Federcammini"

u[11] <- "Forum delle Associazioni professionali di docenti e dirigenti (FONADDS) ; Associazione nazionale dirigenti pubblici e alte professionalità della scuola (ANP) ; Associazione nazionale dei dirigenti scolastici, delle professionalità dell'area dell'istruzione e della ricerca (UDIR) ; Federazione nazionale dei dirigenti e delle alte professionalità della funzione pubblica (FP CIDA) ; Associazione per la Scuola della Repubblica ; Associazione nazionale insegnanti e formatori (ANIEF)"
u[12] <- "Associazione generale italiana dello spettacolo (AGIS) ; Federvivo ; Associazione nazionale fondazioni lirico sinfoniche (ANFOLS) ; Istituto nazionale per la valutazione del sistema educativo di istruzione e di formazione (INVALSI) ; Istituto nazionale di documentazione innovazione e ricerca educativa (INDIRE) ; Associazione rete fondazioni ITS Italia ; Forum nazionale delle associazioni studentesche ; Fondazione Agnelli"
u[13] <- "Assessore alla cultura Trieste ; Assessore alla cultura Bologna ; Assessore alla cultura Firenze ; Assessore alla cultura Roma ; Assessore alla cultura Napoli ; Assessore alla cultura Cagliari"
u[14] <- "Assessore alla cultura Torino ; Assessore alla cultura Genova ; Assessore alla cultura Milano ; Assessore alla cultura Venezia"
u[15] <- "professor Massimo Margottini, ordinario di didattica generale Università degli Studi Roma Tre ; professoressa Anna Maria Giannini, ordinario di psicologia Università degli Studi di Roma \"La Sapienza\" ; dottor Marco De Rossi, amministratore delegato di WeSchool piattaforma di classe digitale"
u[16] <- "Dott.ssa Cinzia Zincone, Provveditore del Provveditorato Interregionale per le Opere Pubbliche Veneto Trentino Alto Adige Friuli Venezia Giulia ; arch. Elisabetta Spitz, Commissario Straordinario per il completamento del Mo.SE. ; arch. Giotta, collaboratrice del Commissario ; avv. Giuseppe Fiengo, Amministratore Straordinario del Consorzio Venezia Nuova ; ing. Daniele Rinaldo, progettista delle barriere in vetro Basilica San Marco ; dott. Devis Rizzo, Presidente di KOSTRUTTIVA ; prof. Stefano Boato ; dott.ssa Lidia Fersuoch, membro del direttivo di Italia Nostra"
u[17] <- "Sport per tutti (UISP) ; Centro sportivo italiano (CSI) ; Associazione italiana Cultura e Sport (AICS) ; CGIL ; CISL ; UIL"
u[18] <- "Direttore di Euromedia Research ; Vice direttore di RAi Scuola e Rai Cultura, responsabile del progetto scuola ; Forum nazionale delle associazioni studentesche"
u[19] <- "Primo Procuratore di San Marco ; Proto di San Marco ; Soprintendente per il Comune di Venezia e la laguna ; provveditorato interregionale per le OO.PP. per il Veneto Trentino Alto Adige e Fruli Venezia Giulia"
u[20] <- "Presidente di Sport e Salute S.p.A. ; Presidente dell'INPS ; Presidente del Consiglio direttivo della divisione calcio paralimpico e sperimentale (DCPS) ; Associazione giocatori italiani basket associati (GIBA) ; Associazione nazionale atlete (ASSIST) ; Associazione italiana calciatori (AIC) ; Associazione italiana pallavolisti (AIP)"

u[21] <- "IAFA (Italian Association of Football Agents) ; AIACS (Associazione Italiana Agenti Calciatori e Società) ; Federfuni ; ANEF (Associazione Nazionale Esercenti Funiviari) ; FISI (Federazione italiana sport invernali) ; FISIP (Federazione italiana sport invernali paralimpici) ; AMSI (Associazione Maestri di Sci Italiani) ; Collegio Nazionale Maestri di sci"
u[22] <- "dottor Agostino Miozzo, Coordinatore del CtS ; dottor Patrizio Bianchi, coordinatore del Comitato di esperti presso il Ministero dell'istruzione ; prof.ssa Simona Argentieri, psicanalista"
u[23] <- "Associazione unione teatri di Roma (UTR) ; Comitato nazionale danza arte e spettacolo (Co.N.D.A.S.) ; Associazione italiana danza attività di formazione (AIDAF) ; AssoDanza Italia (ADI) ; Forum arte e spettacolo (FAS) ; Sindacato arte cultura e spettacolo (Cisal Sacs) ; Federazione per la tutela dei contenuti audiovisivi e multimediali (FAPAV) ; Associazione fonografici italiani (AFI) ; SOUNDREEF ; AssoArtisti-Confesercenti"
u[24] <- "Associazione teatri italiani privati (ATIP) ; StaGe! Coordinamento musica e spettacolo indipendente ed emergente ; Movimento spettacolo dal vivo"
u[25] <- "Associazione dottorandi e dottori di ricerca italiani (ADI) ; Segretariato italiano giovani medici (SIGM) ; Comitato per la valorizzazione del dottorato"
u[26] <- "Associazione nazionale archivistica italiana (ANAI) ; Associazione italiana biblioteche (AIB) ; ALES Arte Lavoro e Servizi Spa ; Electa Spa ; Coopulture ; Zetema progetto cultura"
u[27] <- "Direttore del Parco archeologico di Pompei ; Direttore del Grande Progetto Pompei ; Commissario straordinario del Governo per le Fondazioni lirico sinfoniche ; Fima ; Acri ; Anmli ; Women in film ; Galleria degli Uffizi ; Museo e Real Bosco di Capodimonte ; Confindustria radio tv ; Mediaset ; La7 ; Sky ; Discovery ; Viacom ; Tim ; Vodafone"
u[28] <- "Rai cultura (Rai storia e Rai scuola); Rai cinema ; Rai radio tre ; Salone internazionale del libro di Torino ; Fondazione De Sanctis ; Ali ; Sil ; Fieg ; Aie ; Soundreef ; Afi ; Nuovo Imaie ; Siae ; Aicc ; Arci ; Civita ; Confcooperative cultura ; Mediacoop ; Alleanza delle cooperative italiane ; Confcultura ; Cultura Italiae ; Associazione per l'economia della cultura ; Culturmedia ; Tichetswap ; Slc Cgil ; Uilcom ; Fistel Cisl ; Attrici attori uniti ; Coordinamento lavoratori e lavoratrici dello spettacolo ; Fedítart ; Aps ; Professionisti dello spettacolo ; Assolirica ; Asc ; Associazione nazionale coreografi italiani ; Wgi Writers Guil italia ; Fas ; Coordinamento delle associazioni dei musicisti ; Lavorator_della danza ; Angt ; Agta ; Confartigianato imprese ; Cna ; Aesvi ; Parco archeologico del Colosseo ; Galleria degli uffizi ; Museo Real Bosco di Capodimonte ; Adsi ; Fondazione centro studi doc ; Siedas ; Stati generali della musica indipendente ed emergente"
u[29] <- "ANINSEI Associazione nazionale istituti non statali di educazione e di istruzione ; AIMC Associazione Italiana Maestri Cattolici ; UCIIM Unione cattolica italiana insegnanti dirigenti e formatori ; FISM Federazione Italiana Scuole Materne ; FORMA Associazione Enti Nazionali di Formazione Professionale ; FIDAE Federazione istituti di attività educative ; CDO Compagnia delle Opere Educative-FOE ; AGeSC Associazione genitori scuole cattoliche ; AGE Associazione italiana genitori ; FONAGS Forum nazionale delle associazioni dei genitori della scuola ; INVALSI ; CUNSF Conferenza universitaria nazionale di scienze della formazione ; COORDINAMENTO Presidenti dei Corsi di laurea in Scienze della Formazione primaria ; ASSOCIAZIONE Rete Fondazioni ITS ITALIA ; AGIA Autorità Garante per l'Infanzia e l'Adolescenza ; FORUM del Terzo Settore ; FONDAZIONE Agnelli ; SAVE the Children ; MCE Movimento di Cooperazione Educativa ; CIDI Centro iniziativa democratica insegnanti ; \"TUTTOSCUOLA\" Rivista ; ASSOCIAZIONE Proteo Faresapere ; DIESSE Didattica e innovazione scolastica ; OPERA nazionale Montessori ; RETE Saltamuri educazione sconfinata per l'infanzia, i diritti, l'umanità ; ASSOCIAZIONE Libera Scuola ; UPI Unione province d'Italia ; ANCI associazione nazionale comuni d'Italia ; AIE Associazione Italiana Editori ; ANCE - Associazione nazionale costruttori edili"
u[30] <- "FLC CGIL ; CISL scuola ; UIL scuola ; FGU - Federazione Gilda Unams ; S.N.A.L.S. Sindacato Nazionale Autonomo Lavoratori Scuola ; ANIEF - Associazione professionale sindacale ; UGL scuola ; AND - Associazione nazionale docenti ; UDIR - Associazione nazionale dei dirigenti scolastici, delle professionalità dell'area dell'istruzione e della ricerca ; ANP - Associazione nazionale dirigenti pubblici e alte professionalità della scuola ; ANDIS - Associazione nazionale dirigenti scolastici ; ADI - Associazione docenti e dirigenti scolastici italiani ; FP CIDA Federazione nazionale dei dirigenti e delle alte professionalità della funzione pubblica ; DISAL Dirigenti scuole autonome e libere"

u[31] <- "Cultura Cattolica ; dell'Associazione nazionanle IRC (ANIRC) ; del\"Comitato Trasparenza è partecipazione concorso Dirigenti scolastici 2017\" ; del Coordinamento nazionale docenti abilitati (CNDA) ; dell'Associazione nazionale autonoma professionisti della scuola (Confsal-Anaps) ; del Comitato nazionale \"Giustizia per l'orale\" ; dell'Associazione nazionale imprese di pulizia e servizi integrati (ANIP) ; del Movimento nazionale dei facenti funzione ; Associazione nazionale docenti per i diritti dei lavoratori (AnDDL)"
u[32] <- "Associazione italiana dislessia (AID) ; dell'Istituto tecnico per il turismo \"Livia Bottardi\" ; dell'Istituto professionale statale per i servizi dell'enogastronomia e dell'ospitalità alberghiera \"Amerigo Vespucci\" ; Istituto professionale statale Stendhal"
u[33] <- "Associazione nazionale dirigenti pubblici e alte professionalità della scuola (ANP) ; dell'Associazione professionale Proteo Fare Sapere ; della Società italiana di ricerca didattica (SIRD) ; professor Giancarlo Onger, esperto in politiche sull'inclusione scolastica"
u[34] <- "Associazione nazionale dei dirigenti scolastici, delle professionalità dell'area dell'istruzione e della ricerca (UDIR)"
u[35] <- "FAND Federazione tra le associazioni nazionali delle persone con disabilità ; di FIRST Federazione italiana rete sostegno e tutele diritti delle persone con disabilità ; di FISH Federazione italiana superamento handicap ; SUL Sindacato unitario lavoratori ; di FCG CGIL ; di CISL Scuola ; di UIL Scuola"
u[36] <- "Conferenza dei Direttori delle Accademie di Belle Arti ; della Conferenza dei Direttori dei Conservatori di Musica ; della Conferenza dei Presidenti dei Conservatori di Musica ; della Conferenza nazionale dei Presidenti delle consulte degli studenti delle accademie di belle arti e degli istituti superiori per le industrie artistiche ISIA (CPCSAI) ; della Conferenza dei presidenti delle consulte degli studenti degli Istituti superiori di studi musicali ISSM (CNSI)"
u[37] <- "Direttore del Centro nazionale dipendenze e doping dell'Istituto superiore di sanità ; del Presidente della Sezione per la vigilanza e il controllo sul doping presso il Ministero della Salute ; dell'ex Procuratore Capo della Procura nazionale Antidoping di NADO Italia"
u[38] <- "Associazione italiana danza attività di formazione (AIDAF) ; della Federazione della danza (AIDAP) ; dell'Associazione danza esercizio e promozione (ADEP)"
u[39] <- "Associazione dirigenti scolastici (DiSAL) ; Associazione nazionale dirigenti scolastici (ANDiS) ; Associazione nazionale dirigenti pubblici e alte professionalità della scuola (ANP)"
u[40] <- "Associazione delle imprese stabili di produzione (ISP) ; dell'Associazione nazionale delle compagnie e delle residenze di innovazione teatrale (ANCRIT) ; dell'Associazione nazionale teatri stabili d'arte contemporanea (ANTAC) ; dell'Associazione teatro ragazzi (ASTRA)"

# u back in x_check ; x_check back in x

for (i in 1:40) {
  x_check[mod] <- u[1:40]
  }

for (i in 1:66) {
  x[d] <- x_check[1:66]
}

output <- c()
for (i in d) {
  h <- p[i]
  output <- c(output, h)
}

for (i in d) {
  nomi[output] <- x[d] #sostituiti
}

p <- str_extract(nomi, " e ")
p <- which(p == " e ")
x <- str_subset(nomi, " e ")

n <- c(2,6,14,17,18,20,41,50,51,52,56,57,61,62,63,65,67,72,76,78,79,81,82,84,93,96,97,118)

for (i in n) {
  x[i] <- gsub(" e ", ";", x[i])
}

output <- c()
for (i in n) {
  h <- p[i]
  output <- c(output, h)
}

for (i in n) {
  nomi[output] <- x[n]
}

x[5] <- "Unione nazionale di interpreti teatro e audiovisivo (UNITA) ; Writers Guild Italia"
x[19] <- "Istituto superiore per la protezione e la ricerca ambientale (ISPRA); ICOMOS Italia"
x[20] <- "Conferenza nazionale studenti degli ISSM (CNSI) ; Rete nazionale dei licei musicali e coreutici"
x[21] <- "Comitato per l'apprendimento pratico della musica e per gli studenti (CNAPM) ; Conferenza dei Direttori dei conservatori di musica"
x[30] <- "CONI ; Sport e Salute SPA"
x[38] <- "Confederazione nazionale dell'artigianato e della piccola e media impresa (CNA) ; Lega Coop"
x[70] <- "Associazione nazionale docenti (AND) ; Associazione docenti e dirigenti scolastici (ADI)"
x[82] <- "Associazione nazionale esercenti multiplex (ANEM) ; Federazione Carta e Grafica"
x[93] <- "Associazione italiana organizzatori e produttori spettacoli di musica dal vivo (ASSOMUSICA) ; Produttori musicali indipendenti (PMI)"
x[114] <- "Osservatorio regionale del Piemonte per l'università e per il diritto allo studio universitario ; FIR (Federazione Innovazione e Ricerca) Cisl"

n <- c(5,19,20,21,30,38,70,82,93,114)
output <- c()
for (i in n) {
  h <- p[i]
  output <- c(output, h)
}
for (i in n) {
  nomi[output] <- x[n]
}

nomi[10] <- "UIL ; UIL SCUOLA"
nomi <- gsub("\\.$", "", nomi) %>% str_trim(side = "right")
nomi <- gsub("\\,$", "", nomi) %>% str_trim(side = "right")

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=320, ncol=1)
for (i in 1:320) {
  m[i,] <- length(nomi_splitted[[i]])
}

c7$times <- m[,1]

new_data <- c7[rep(1:nrow(c7), times = c7$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle|^de della"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

j <- c(1,6)
for (i in j) {
  x[i] <- gsub("-", ",", x[i])
}

output <- c()
for (i in j) {
  h <- p[i]
  output <- c(output, h)
}

for (i in j) {
  single_name[output] <- x[j]
}

k <- c(3,4,5,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,37,38,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69)
for (i in k) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in k) {
  h <- p[i]
  output <- c(output, h)
}

for (i in k) {
  single_name[output] <- x[k]
}

x[65] <- "Lorenzo Baldrighi, Artists Management ; Baldrighi Bertoni, Music Productions ; Raffaella Coletti ; InArt ; Music Center Oldani ; RESIA ; Stage Door"
x[67] <- "Docenti di didattica della musica - Gruppo operativo DDM GO"

single_name[1061] <- x[65]
single_name[1065] <- x[67]

remove <- "^'|^de"
single_name <- gsub(remove, "", single_name) %>% str_squish()
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

# Expand the dataset by separating names and creating multiple rows
new_data <- new_data %>%
  separate_rows(NOMI, sep = ";") %>%
  mutate(NOMI = str_trim(NOMI))  # Removes leading/trailing spaces

# salva new_data in C7.csv
write.csv(new_data, "[path]/C7.csv", row.names = FALSE)
