library("dplyr")
library("stringr")

# Carichiamo commissione10.csv in c10
c10 <- read.csv("[path]/commissione10.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c10$NOMI
nomi <- str_squish(nomi)

nomi[13] <- "AIGET (Associazione italiana di grossisti di energia e trader) ; ARTE (Associazione di reseller e trader dell'energia) ; CNA (Confederazione nazionale dell'artigianato e della piccola e media impresa) ; Confartigianato Imprese ; CNCU"
nomi[16] <- "Assocarta ; Assogasmetano-Federmetano ; Federacciai ; Unionplast ; UNEM"
nomi[17] <- "Confindustria Alberghi ; AGCM Autorità Garante Concorrenza e Mercato ; ANCI ; Confimi Industria ; ART Autorità Regolazione dei Trasporti ; ASSTEL ; ARERA Autorità di Regolazione per energia reti e ambiente ; Confindustria ; Garante per la sorveglianza dei prezzi ; Conferenza Regioni e Province autonome ; Conferenza Stato Regioni"
nomi[18] <- "Assocostieri ; Assogasliquidi-Federchimica ; Assoutenti ; Caritas ; CIB Consorzio Italiano Biogas ; CNCU-Altroconsumo ; Movimento Consumatori ; ANCE"
nomi[19] <- "Assopetroli-Assoenergia ; Federalberghi ; Confesercenti ; UIL ; Federterme ; Confagricoltura ; CISL ; CGIL ; Federazione ANIMA ; FILT CGIL ; Federturismo ; Confcommercio ; UGL ; FINCO"
nomi[20] <- "Elettricità futura ; Italia Solare ; Utilitalia ; ABI Associazione Bancaria Italiana ; Confetra ; Confartigianato Imprese ; Confapi ; CNA"
nomi[21] <- "Energia Libera ; FEDERDISTRIBUZIONE ; NGV-Italy ; AISCAT ; ANITA ; ASSITERMINAL ; CONFEDILIZIA ; CONFLAVORO PMI ; CNDCEC ; SACE ; UNAITALIA ; ANIF - Eurowellness ; ASSOTURISMO ; CONFTURISMO - Confcommercio ; ASSOSISTEMA ; CONFINDUSTRIA SERVIZI HCFS ; FAIB Confesercenti - FEGICA - FIGISC Confcommercio ; Ferrovie dello Stato ; Federazione Carta e grafica ; Assofloro ; AGENS ; ALIS ; AssoESCo ; ACEA ; ANAC ; Casartigiani ; AIRU ; FNOPI ; AIGET ; CONSIP ; ENEA ; ILVA ; Assistal ; A.R.T.E. - Associazione Reseller e Trader Energia ; a2a Life Company ; ANEV ; ANIGAS ; Bluenergy ; ANPE ; ASSOEBIOS ; EBS -BIOMASSE ; EDISON ; Trend-Micro Italia ; del Prof. Stefano Sylos Labini ; AN.BTI - Associazione Nazionale Bus Turistici Italiani ; ANAV - Associazione Nazionale Autotrasporto Viaggiatori ; ANIEF - Associazione professionale sindacale ; Assonime"
nomi[25] <- "UICI Unione Italiana dei Ciechi e degli Ipovedenti ; ANITEC-ASSINFORM ; AIE Associazione Italiana Editori ; AgID Agenzia per l'Italia Digitale"
nomi[36] <- "Coordinamento Nazionale Mare Libero (CoNaMaL) ; CNA Balneari ; Lega navale italiana ; Sindacato italiano balneari (SIB) ; La base balneare con Donnedamare ; Federazione italiana imprese balneari (FIBA Confesercenti) ; Federbalneari Italia ; Conferenza delle Regioni e delle Province autonome"
nomi[43] <- "Poste Italiane ; IAIC (Italian Academy of the Internet Code) ; ASSINTEL (Associazione nazionale imprese ICT) ; CISPE Cloud ; Anitec-Assinform Associazione Italiana per l'Information and Communication Technology (ICT) ; Assofranchising ; IVASS (Istituto per la vigilanza sulle assicurazioni) ; Associazioni dei consumatori ; Federcarrozzieri"
nomi[67] <- "A.C.N.C.C. Associazione Campana Noleggio Con Conducente ; A.N.C. Associazione Nazionale di Categoria trasporto persone e mobilità ; A.N.C.C.I. Aggregazione Noleggiatori con Conducente Italiana ; A.N.I.Tra.V. Associazione Nazionale Imprese Trasporto Viaggiatori ; ASINCC Associazione Siciliana Noleggio Con Conducente ; Associazione NCC Italia Noleggiatori per i noleggiatori ; Comitato A.I.R. Comitato Autonoleggiatori Italiani Riuniti ; Comitato Sindacale NCC Fiumicino ; FEDERNOLEGGIO-Confesercenti Federazione italiana delle imprese di noleggio auto e autobus con conducente ; F.I.A.-Confindustria Federazione Imprese Autonoleggio con conducente ; F.I.N.C.C. Federazione Imprese Noleggio Con Conducente ; O.R.A. NCC Osservatori Regionali Autoservizi Pubblici Non di Linea - NCC ; 8PUNTOZERO ; PROFESSIONE TRAVEL NCC DONNE 2030 ; Sindacato Nazionale LLP Lavoratori Liberi Professionisti ; Sistema Trasporti-Sistema Impresa"
nomi[76] <- "Movimento Nazionale Liberi Farmacisti (MNLF) ; Federazione Nazionale Parafarmacie Italiane (FNPI) ; Confederazione Unitaria Libere Parafarmacie Italiane (CULPI) ; Federazione Farmacisti e Disabilità Onlus (FEDERFARDIS)"
nomi[80] <- "Federmoto ; AON (consulenza dei rischi, intermediazione assicurativa e riassicurativa) ; Agenzia italiana del farmaco ; Farmindustria ; Associazione medici dirigenti (ANAAO-ASSOMED) ; AVIS Nazionale OdV ; EGUALIA (Industrie farmaci accessibili) ; SIHTA (Società italiana di Health Technology Assessment) ; #VITA (Valore e innovazione delle terapie avanzate)"
nomi[92] <- "Autorità di regolazione dei trasporti (ART) ; Consorzio nazionale per la raccolta, il riciclo e il recupero degli imballaggi in plastica (COREPLA) ; Consorzio nazionale imballaggi (CONAI) ; Consorzio volontario per il riciclo del PET (CORIPET) ; dell'Associazione importatori medicinali Italia (AIM) ; dell'Associazione italiana ospedalità privata (AIOP) ; dell'Associazione immunodeficienze primitive (AIP-OdV)"
nomi[96] <- "Autorità di regolazione per energia reti e ambiente (ARERA) ; Stefano Grassi, Capo di Gabinetto della Commissaria europea per l'energia"
nomi[97] <- "Elettricità futura ; Compagnia valdostana delle acque (CVA) ; Dolomiti energia Holding SpA ; Edison ; Confederazione generale dei sindacati autonomi dei lavoratori (CONFSAL) ; Confederazione generale italiana dei trasporti e della logistica (CONFETRA) ; Legambiente-UNI ; WWF Italia ; Greenpeace Italia ; Italia Nostra"
nomi[108] <- "ANCI ; UPI ; Assoprofessioni ; CNA ; Confartigianato Imprese ; Confcommercio ; Confesercenti ; Confindustria ; Conferenza delle Regioni e delle Province autonome"
nomi[123] <- "Corpo nazionale dei Vigili del Fuoco Direzione centrale per la prevenzione e la sicurezza tecnica ; Consiglio nazionale periti industriali e periti industriali laureati"
nomi[125] <- "Consiglio nazionale ingegneri ; Consiglio nazionale geometri e geometri laureati"
nomi[133] <- "Gestore dei servizi energetici (GSE) ; Cassa dei servizi energetici e ambientali (CSEA) ; Confcommercio-Imprese per l'Italia"
nomi[143] <- "ANIGAS (Associazione nazionale industriali gas) ; AIRU (Associazione italiana riscaldamento urbano) ; AIGET (Associazione italiana grossisti di energia e trader) ; Energia libera ; Federmetano"
nomi[209] <- "Federazione carta e grafica ; Federazione gomma plastica (UNIONPLAST)"
nomi[220] <- "Associazione italiana polistirene espanso (AIPE) ; Associazione nazionale riciclatori e rigeneratori di materie plastiche (ASSORIMAP) ; Consorzio Biorepack ; Consorzio italiano compostatori ; Consorzio Coripet"
nomi[231] <- "Autorità di regolazione per energia reti e ambiente (ARERA) ; Italia Solare ; RSE - Ricerca sul sistema energetico ; Elettricità futura"
nomi[232] <- "Autorità di regolazione per energia reti e ambiente (ARERA) ; Italia Solare ; RSE - Ricerca sul sistema energetico ; Elettricità futura"
nomi[243] <- "Associazione nazionale costruttori edili (ANCE) ; Assoprofessioni ; Unioncamere ; Consiglio nazionale consulenti del lavoro ; Consiglio nazionale dottori commercialisti ed esperti contabili ; Confederazione nazionale dell'artigianato e della piccola e media impresa (CNA)"
nomi[253] <- "ANIA (Associazione nazionale fra le imprese assicuratrici) ; EPPI (Ente di previdenza dei periti industriali e dei periti industriali laureati) ; INPS"
nomi[263] <- "associazione \"La strada della ceramica in Umbria\" ; Artex (Centro per l'artigianato artistico e tradizionale della Toscana) ; AiCC (Associazione italiana Città della ceramica)"
nomi[264] <- "CNA Nazionale ; Confguide (Federazione nazionale guide turistiche) ; CGIL ; CISL ; CONFSAL ; CUB-FLAICA ; UGL ; UIL"
nomi[265] <- "Federperiti (Federazione italiana tra le associazioni dei periti assicurativi e danni) ; APAID (Associazione periti auto ispettori danni)"
nomi[270] <- "AGTA (Associazione guide turistiche abilitate) ; Federagit (Federazione italiana guide turistiche, accompagnatori e interpreti) ; SNGT (Sindacato nazionale guide turistiche Roma) ; FAI (Fondo ambiente italiano)"
nomi[277] <- "Direttore generale per la crescita sostenibile e la qualità dello sviluppo del Ministero della transizione ecologica ; Direttore generale per il risanamento ambientale del Ministero della transizione ecologica ; Capo Dipartimento per i trasporti e la navigazione del Ministero delle infrastrutture e della mobilità sostenibili"
nomi[291] <- "ENI"
nomi[311] <- "Invitalia ; Direttore generale per la politica industriale, la competitività e le piccole e medie imprese del MISE"
nomi[313] <- "RSE (Ricerca sul sitema energetico) ; ARERA (Autorità di regolazione per energia, reti e ambiente) ; ENEA (Agenzia nazionale per le nuove tecnologie, l'energia e lo sviluppo economico sostenibile)"
nomi[347] <- "Federazione Carta e Grafica (FCG) ; Consorzio nazionale recupero e riciclo degli imballaggi a base cellulosica (COMIECO)"
nomi[372] <- "Transport & Environment ; Kyoto Club ; Legambiente ; Cittadini per l'Aria ; Greenpeace Italia ; WWF Italia"
nomi[373] <- "Kyoto Club-Transport and Environment"
nomi[393] <- "Alleanza delle cooperative italiane - Turismo e beni culturali ; Slow Tourism Italia ; AGTI (Associazione grotte turistiche italiane) ; Federterme"
nomi[399] <- "professoressa Giovanna De Minico, Università degli Studi di Napoli Federico II ; professor Andrea Renda, CEPS (Centro per gli Studi Politici Europei) ; dottor Pietro Guindani, Presidente ASSTEL ; dottor Marco Gay, Presidente Anitec-Assinform"
nomi[405] <- "Conferenza delle Regioni e delle Province autonome ; Agenzia per l'Italia digitale ; AIDiT (Associazione italiana distribuzione turistica)"
nomi[407] <- "CNA Turismo e commercio ; Associazione italiana turismo responsabile (AITR) ; Associazione guide turistiche abilitate (AGTA)"
nomi[410] <- "Conferenza delle Regioni e delle Province autonome ; Confesercenti-Federagit Federazione italiana guide turistiche, accompagnatori e interpreti"
nomi[414] <- "FIAVET (Federazione italiana associazioni imprese viaggi e turismo) ; Unioncamere"
nomi[417] <- "Federalberghi ; ENIT (Agenzia nazionale del turismo) ; CNEL (Consiglio nazionale dell'economia e del lavoro)"
nomi[467] <- "Confederazione AEPI Ass. europea professionisti e imprese ; Alleanza delle cooperative italiane ; FIPE Federazione italiana pubblici esercizi"
nomi[615] <- "Anitec-Assinform ; Confapi (Confederazione italiana piccola e media industria privata) ; rivista \"Il Salvagente\""
nomi[620] <- "Consorzio nazionale raccolta e riciclo (Cobat) ; Consiglio nazionale dei consumatori e degli utenti (CNCU)"
nomi[621] <- "Istituto superiore per la protezione e la ricerca ambientale (ISPRA)"
nomi[636] <- "Presidente della Provincia di Savona ; Comune di Vado Ligure ; Comune di Cairo Montenotte"
nomi[637] <- "Agenzia nazionale per l'attrazione degli investimenti e lo sviluppo d'impresa (Invitalia) ; CGIL-CISL-UIL Savona ; UGL Savona ; Direttore generale dell'Unione industriali di Savona ; Assessore allo sviluppo economico della Regione Liguria"
nomi[650] <- "Associazione italiana di scienza e tecnologia dei cereali (AISTEC) ; Confederazione nazionale dell'artigianato e della piccola e media impresa (CNA)"
nomi[656] <- "Federazione italiana panificatori, pasticceri e affini (FIPPA) ; Associazione italiana panificatori e affini (Assipan)"
nomi[670] <- "Regione Piemonte ; Regione Sardegna ; Regione Emilia-Romagna ; Regione Lombardia"
nomi[671] <- "Greenpeace ; Legambiente ; WWF"
nomi[674] <- "CGIL ; CISL ; UIL ; UGL"
nomi[684] <- "Enea ; Consiglio nazionale consumatori e utenti (CNCU) ; Associazione nazionale costruttori edili (ANCE)"


words <- "del 22 luglio 2021|sul della Conferenza delle Regioni|documentazione|Trasmissioni|Sindacati|osservazione"
nomi <- gsub(words, "", nomi)

nomi <- gsub("sul testo.*", "", nomi)
nomi <- gsub("nell'ambito.*", "", nomi)
nomi <- gsub("in merito.*", "", nomi)
nomi <- gsub("sui prezzi.*", "", nomi)
nomi <- gsub("sui meccanismi.*", "", nomi)
nomi <- gsub("sulla Proposta.*", "", nomi)
nomi <- gsub("sul comparto.*", "", nomi)
nomi <- gsub("sull'area.*", "", nomi)
nomi <- gsub("sulla situazione.*", "", nomi)
nomi <- gsub("sul recente.*", "", nomi)
nomi <- gsub("sulle vicende.*", "", nomi)

x <- c(2,11,14,15,106,122,178,233,234,247,261,267,268,281,290,317,385,403,406,415,419,422,428,430,434,437,439,446,449,481,485,486,594,598,605,611,612,631,632,638,647,653,662,673,685)
for (i in x){
  nomi[i] <- gsub(",", " ;", nomi[i])
}
for (i in x){
  nomi[i] <- gsub(" e ", ";", nomi[i])
}

x <- c(4,9,10,69,119,127,134,176,177,210,211,240,256,269,274,286,396,416,436,442,443,528,603,604,623,648,657,666,686)
for (i in x){
  nomi[i] <- gsub(" e ", ";", nomi[i])
}

x <- c(37,79,95,102,104,107,142,273,394,454,508,683)
for (i in x){
  nomi[i] <- gsub(",", " ;", nomi[i])
}

nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=696, ncol=1)
for (i in 1:696) {
  m[i,] <- length(nomi_splitted[[i]])
}

c10$times <- m[,1]

new_data <- c10[rep(1:nrow(c10), times = c10$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle|^de della|^de"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

j <- c(1,6,10,11,12,13,14,15,17,18,19,20,23,24,26,31,32,33,34,35,36,39,43,47,49,51,52,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,87,88,90,92,93,94,98,99,100,105,106,107,110,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,158,164,165,166)
for (i in j) {
  x[i] <- gsub("-", " ", x[i])
}

x[51] <- "Dr. Carlo Zaghi, Direzione Generale per il Mare e le Coste del Ministero della transizione ecologica Ufficio del Direttore Generale"

j <- c(1,6,10,11,12,13,14,15,17,18,19,20,23,24,26,31,32,33,34,35,36,39,43,47,49,50,51,52,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,87,88,90,92,93,94,98,99,100,105,106,107,110,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,158,164,165,166)

output <- c()
for (i in j) {
  h <- p[i]
  output <- c(output, h)
}

for (i in j) {
  single_name[output] <- x[j]
}

p <- str_extract(single_name, "/")
p <- which(p == "/")
x <- str_subset(single_name, "/")

single_name[745] <- "FIBA-Confesercenti (Federazione italiana imprese balneari)"
single_name[845] <- "Federazione Moda Italia-Confcommercio"

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

# salva new_data in C10
write.csv(new_data, "[path]/C10.csv", row.names = FALSE)
