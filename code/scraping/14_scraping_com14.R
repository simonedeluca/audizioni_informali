library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 10)

for (page_index in 1:10) {
  link <- paste("https://www.senato.it/Leg18/3712?current_page_40491=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[9]] <- append(x[[9]], "Disegno di legge europea 2018 - AS 822", after = 8)

a <- rep("Disegno di legge europea 2018 - AS 822", 2)
x[[10]] <- append(x[[10]], a, after = 1)
x[[10]][5] <- "Gestione della migrazione; bilancio UE per il futuro"

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 10)

for (page_index in 1:10) {
  link <- paste("https://www.senato.it/Leg18/3712?current_page_40491=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 10)

for (page_index in 1:10) {
  link <- paste("https://www.senato.it/Leg18/3712?current_page_40491=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

x[[1]] <- append(x[[1]], "14 (Unione Europea)", after = 1)
a <- rep("14 (Unione Europea)", 2)
x[[1]] <- append(x[[1]], a, after = 3)

x[[2]] <- append(x[[2]], "14 (Unione Europea)", after = 1)
x[[2]] <- append(x[[2]], "14 (Unione Europea)", after = 9)

a <- rep("14 (Unione Europea)", 8)
x[[3]] <- append(x[[3]], a, after = 0)

x[[5]][7] <- "5 (Bilancio), 14 (Unione europea), V (Bilancio), XIV (Unione Europea)"
x[[5]][8] <- "5 (Bilancio), 14 (Unione europea), V (Bilancio), XIV (Unione Europea)"
x[[5]][9] <- "5 (Bilancio), 14 (Unione europea), V (Bilancio)"
x[[5]][10] <- "5 (Bilancio), 14 (Unione europea), VI (Finanze)"

a <- rep("14 (Unione Europea)", 7)
x[[6]] <- append(x[[6]], a, after = 1)

x[[7]] <- append(x[[7]], a, after = 3)

a <- rep("14 (Unione Europea)", 10)
x[[8]] <- a
x[[9]] <- a

a <- rep("14 (Unione Europea)", 3)
x[[10]] <- append(x[[10]], a, after = 0)
x[[10]][5] <- "14 (Unione Europea) e XIV (Pol. Unione Europea)"

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 10)

for (page_index in 1:10) {
  link <- paste("https://www.senato.it/Leg18/3712?current_page_40491=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".sublista_docs_ul") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi

for (i in 1:10) {
  x[[i]] <- gsub("[\n]", "", x[[i]]) %>% #togli new line
    str_trim(side = "both") # togli white space all'inizio e alla fine!
}

words_to_remove <- "Memoria| - |integrazione"

for (i in 1:10) {
  x[[i]] <- gsub(words_to_remove, "", x[[i]]) %>%
    str_trim(side = "left")
}

x[[1]] <- append(x[[1]], "Maria Rosaria Carfagna, Ministro per il Sud e la Coesione territoriale", after = 2)
x[[1]][5] <- "Pasquale Tridico, Presidente dell'INPS ; Prof. Marco Scialdone ; Prof. Gianluca Sgueo ; Prof. Nicola Ferrigni ; Dott. Roberto Falcone, Segretario generale ASSOPROFESSIONI ; Prof.ssa Annamaria Donini ; Prof. Giuseppe Antonio Recchia ; Prof. Maurizio del Conte ; Dott.ssa Lucilla Deleo, responsabile Rapporti istituzionali di Search on Media Group"

x[[2]] <- append(x[[2]], "Andrea Orlando, Ministro del lavoro e delle politiche sociali", after = 2)
x[[2]][5] <- "Daniele Franco, Ministro dell'economia e delle finanze"

a <- paste(x[[2]][[7]],x[[2]][[8]], sep = " ")
x[[2]] <- x[[2]][-c(7,8)]
x[[2]] <- append(x[[2]], a, after = 6)
a <- paste(x[[2]][[9]],x[[2]][[10]], sep = " ")
x[[2]] <- x[[2]][-c(9,10)]
x[[2]] <- append(x[[2]], a, after = 8)

x[[2]][7] <- "Conferenza delle Regioni e delle Province autonome ; Pier Virgilio Dastoli, Presidente del Consiglio italiano del Movimento Europeo ; ASSONIME ; ABI ; CGIL-CISL-UIL ; CISL ; UGL ; CNEL ; Banca d'Italia ; ANCI"
x[[2]][8] <- "Ferdinando Nelli Feroci, co-Presidente del Comitato scientifico per il futuro dell'Europa ; Movimento Federalista europeo-Associazione Giovent? Federalista europea"
x[[2]][9] <- "Conferenza dei Presidenti delle Assemblee legislative delle Regioni e delle Province autonome ; Comitato europeo delle Regioni ; UPI ; UNCEM ; Corte dei Conti"

a <- paste(x[[3]][[2]],x[[3]][[3]], sep = " ")
x[[3]] <- x[[3]][-c(2,3)]
x[[3]] <- append(x[[3]], a, after = 1)
a <- paste(x[[3]][[3]],x[[3]][[4]], sep = " ")
x[[3]] <- x[[3]][-c(3,4)]
x[[3]] <- append(x[[3]], a, after = 2)

x[[3]][1] <- "CONFETRA Friuli-Venezia Giulia ; Presidente del Consiglio regionale del Friuli-Venezia Giulia ; Autorit? di Sistema Portuale del Mare Adriatico Orientale"
x[[3]][3] <- "Pierpaolo Sileri, Sottosegretario di Stato per la salute ; Massimo Casciello, Direttore generale della Direzione generale per l'igiene e la sicurezza degli alimenti e la nutrizione del Ministero della salute ; FEI - Federazione Erboristi Italiani ; FederSalus ; ABOCA"
x[[3]][4] <- "Roberto Rustichelli, Presidente dell'Autorit? Garante della Concorrenza e del Mercato ; Giovanni Salvi, Procuratore Generale della Suprema Corte di Cassazione ; CONSOB ; IGI - Istituto Grandi Infrastrutture"


a <- paste(x[[4]][[10]],x[[4]][[11]], sep = " ")
x[[4]] <- x[[4]][-c(10,11)]
x[[4]] <- append(x[[4]], a, after = 9)

a <- paste(x[[6]][[1]],x[[6]][[2]], sep = " ")
x[[6]] <- x[[6]][-c(1,2)]
x[[6]] <- append(x[[6]], a, after = 0)
a <- paste(x[[6]][[6]],x[[6]][[7]], sep = " ")
x[[6]] <- x[[6]][-c(6,7)]
x[[6]] <- append(x[[6]], a, after = 5)

x[[6]][5] <- "INPS ; INAPP (Istituto Nazionale per l'Analisi delle Politiche Pubbliche) ; COMMA 2 ; Cobas ; Federdistribuzione"

x[[7]][1] <- "Banca d'Italia ; ASSAEROPORTI ; Assonime ; Ance ; CONFAPI ; CNA (Confederazione nazionale dell'artigianato e della piccola e media impresa) ; Confartigianato Imprese ; Confersercenti ; Confcommercio Imprese per l'Italia ; Casartigiani ; CONFPROFESSIONI ; Alleanza delle Cooperative italiane ; FEDERDISTRIBUZIONE ; Confagricoltura ; CIA-Agricoltori italiani ; Coldiretti-Filiera Italia ; Copagri ; CONFETRA ; ANIA"
x[[7]][2] <- "ABI ; Confindustria"
x[[7]][7] <- "ASSOBIBE ; Confindustria Dispositivi Medici ; CONSOB ; Federazione gomma plastica ; Assobioplastiche ; ANGEM ; AIPE ; Marevivo Onlus ; Alleanza Cooperative italiane"
x[[7]][8] <- "Assorassegne Stampa ; ANAI ; ICOM Italia ; CIC ; ANIE ; RSE ; Italia Solare ; Cogen Europe ; SNAM ; Consiglio Nazionale del Notariato ; MFSD"
x[[7]][9] <- "CIA ; Coldiretti ; Confagricoltura ; COPAGRI ; Federalimentare ; Unaitalia ; Assalzoo ; Assica ; Prof. Ferdinando Albisinni, Universit? degli Studi della Tuscia ; Federdistribuzione ; Centromarca"
x[[7]][10] <- "LAPET ; BCC ; ABI ; Assonime ; Assogestioni ; Prof. Massimiliano Marzo, Universit? degli Studi di Bologna"

x[[8]][2] <- "Slc-Cgil ; Fistel-Cisl ; Uilcom-Uil ; FPA Fotografi ; Utilitalia ; Motus-E ; Neste"
x[[8]][3] <- "FNSI ; CRUI ; SIAE ; Wikimedia Italia ; Google ; Nuovo IMAIE ; FIMI ; MPA ; AISA ; GOIPE ; Creative Commons ; Hermes ; Movimento consumatori ; ANAC Autori ; Altroconsumo"
x[[8]][4] <- "ANICA - Associazione Nazionale Industrie Cinematografiche Audiovisive e Multimediali ; APA - Associazione Produttori Audiovisivi ; Confindustria digitale ; FIEG - Federazione Italiana Editori Giornali ; Confindustria Cultura Italia ; FAPAV - Federazione per la tutela dei contenuti Audiovisivi e Multimediali ; AIB"
x[[8]][5] <- "ANSO ; EMusa ; 100autori ; ITSRIGHT ; AIE"
x[[8]][9] <- "Ministero dell'Ambiente direzione generale rifiuti ; FISE-Assoambiente-FISE-Unicircular ; Fondazione per lo sviluppo sostenibile ; Ecogeo ; Future Power ; ASSOBIOPLASTICHE ; CONAI ; COREPLA ; Federchimica Plastiscseurope Italia ; ANIE ; Centro coordinamento RAEE ; Ecodom ; PV Cycle-ECO-PV"
x[[8]][10] <- "Riccardo Fuzio, Procuratore generale della Repubblica presso la Corte di cassazione"

x[[9]][3] <- "ANIA - Associazione nazionale fra le imprese assicuratrici ; ASSOFONDIPENSIONE ; ASSOPREVIDENZA - Associazione italiana per la previdenza complementare ; ADEPP - Associazione degli enti previdenziali privati ; CNCU - Consiglio nazionale dei consumatori e degli utenti ; Altroconsumo ; U.DI.CON"
x[[9]][5] <- "CONSOB"
x[[9]][6] <- "Federpesca ; Alleanza delle Cooperative italiane - Settore pesca ; Assarmatori ; Confitarma"
x[[9]][9] <- "SIAE - Societ? Italiana degli Autori ed Editori ; ANIE - Federazione Nazionale Imprese Elettrotecniche ed Elettroniche ; AIFA - Agenzia Italiana del Farmaco ; Agenzia delle Dogane e dei Monopoli"
x[[9]][10] <- "ISTAT"

x[[10]][1] <- "Banca d'Italia"
x[[10]][4] <- "Paolo Savona, Ministro per gli affari europei"
x[[10]][5] <- "Beatrice Covassi, Capo della Rappresentanza in Italia della Commissione europea"



# Strategia per pulire la colonna degli auditi
x <- lapply(x, str_squish)
x <- lapply(x, function(x) gsub("\\s*\\([^\\)]+\\)"," ;", x))
x <- lapply(x, function(x) gsub("\\;$", "", x))
x <- lapply(x, function(x) str_split(x, ";", simplify = TRUE))
x <- lapply(x, t)

my_fun <- function(x) {
  str_trim(side = "both", x)
}

for (i in 1:10) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

x <- lapply(x, function(x) gsub("\\.$", "", x))
x <- lapply(x, function(x) gsub("\\,$", "", x))

my_fun <- function(x) {
  x[!x %in% ""]
}

for (i in 1:10) {
  x[[i]] <- apply(x[[i]], 2, my_fun)
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"

for (i in 1:10) {
  x[[i]] <- lapply(x[[i]], function(x) gsub(words2, "", x))
}

for (i in 1:10) {
  x[[i]] <- lapply(x[[i]], function(x) str_trim(x, side = "left"))
}

for (i in 1:10) {
  x[[i]] <- lapply(x[[i]], function(x) paste(x, collapse = " ; "))
}

nomi <- x

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 14
c14 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)




