library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 26)

for (page_index in 1:26) {
  link <- paste("https://www.senato.it/Leg18/3671?current_page_40451=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[2]] <- append(x[[2]], "Iniziative contro l'aumento dei costi delle materie prime e e per contrastare gli effetti del Covid-19", after = 7)
x[[2]] <- append(x[[2]], "Analisi delle razioni alimentari per allevamenti da latte", after = 9)

x[[21]] <- append(x[[21]], "Progetto Olimpolli Montagnani sull'impollinazione degli olivi con l'ausilio di droni", after = 3)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 26)

for (page_index in 1:26) {
  link <- paste("https://www.senato.it/Leg18/3671?current_page_40451=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

data[[26]] <- append(data[[26]], "settembre 2021", after = 3)

# 3. Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 26)

for (page_index in 1:26) {
  link <- paste("https://www.senato.it/Leg18/3671?current_page_40451=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("9 (Agricoltura)", 6)
x[[1]] <- append(x[[1]], a, after = 0) 
a <- rep("9 (Agricoltura)", 2)
x[[1]] <- append(x[[1]], a, after = 8)

a <- rep("9 (Agricoltura)", 10)
x[[2]] <- a

a <- rep("9 (Agricoltura)", 2)
x[[3]] <- append(x[[3]], a, after = 0)
x[[3]] <- append(x[[3]], "9 (Agricoltura)", after = 7)

a <- rep("9 (Agricoltura)", 10)
for (i in 4:19) {
  x[[i]] <- a
  }

a <- rep("9 (Agricoltura)", 3)
x[[20]] <- append(x[[20]], a, after = 2)
x[[20]] <- append(x[[20]], a, after = 6)

x[[21]] <- append(x[[21]], "9 (Agricoltura)", after = 0)
a <- rep("9 (Agricoltura)", 4)
x[[21]] <- append(x[[21]], a, after = 2)
x[[21]] <- append(x[[21]], "9 (Agricoltura)", after = 7)

x[[22]] <- append(x[[22]], "9 (Agricoltura)", after = 7)

x[[23]] <- append(x[[23]], "9 (Agricoltura)", after = 1)
x[[23]] <- append(x[[23]], "9 (Agricoltura)", after = 5)
x[[23]] <- append(x[[23]], "9 (Agricoltura)", after = 9)

a <- rep("9 (Agricoltura)", 10)
x[[24]] <- a
x[[24]][10] <- "Uffici di Presidenza Commissioni riunite 9 e 14"

x[[25]] <- a

a <- rep("9 (Agricoltura)", 3)
x[[26]] <- append(x[[26]], a, after = 0)

commissione <- x

# 4. Estrazioni nomi

nomi <- vector(mode = "list", length = 26)

for (page_index in 1:26) {
  link <- paste("https://www.senato.it/Leg18/3671?current_page_40451=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".titolo_pubblicato strong") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

y <- nomi

# Pre-cleaning

words_to_remove <- "Audizione|informale|informale,|in videoconferenza,|in videoconferenza|di rappresentanti"

for (i in 1:26) {
  y[[i]] <- gsub(words_to_remove, "", y[[i]]) %>%
    str_trim(side = "left")
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei"

for (i in 1:26) {
  y[[i]] <- gsub(words2, "", y[[i]]) %>%
    str_trim(side = "left")
}

# Integrazione manuale

y[[1]][[7]] <- "AIA, MIPAAF, LAV, FNOVI"
y[[1]][[8]] <- "Legambiente, Lipu, WWF, Coldiretti, Uecoop"

y[[3]][[3]] <- "Consorzio Prosciutto di Parma, Associazione Italiana Allevatori, LAV, ANMVI, Federparchi, ASSICA, UNAITALIA, Prof. Ferroglio"
y[[3]][[4]] <- "UNCIAGROALIMENTARE, COLDIRETTI"
y[[3]][[5]] <- "Arma dei Carabinieri - Comando unit? forestali, ambientali e agroalimentari; Regione Lombardia - Assessorato all'agricoltura, alimentazione e sistemi verdi"
y[[3]][[6]] <- "Dott. Alberto Laddomada, SIVEMP, ENPA"
y[[3]][[7]] <- "Conferenza delle Regioni e delle Province Autonome"
y[[3]][[9]] <- "Regione Liguria, Regione Piemonte, Istituto zooprofilattico sperimentale del Piemonte, Liguria e Val d'Aosta"
y[[3]][[10]] <- "FNOVI, Ministero della Salute, ANCI"

y[[6]][7] <- "UNCI Agroalimentare, COLDI, AGRINSIEME"

y[[7]][1] <- "Agrinsieme, UNCI Agroalimentare"

y[[9]][3] <- "UNCI Agroalimentare, Coldiretti, AGRINSIEME"
y[[9]][8] <- "UNCI Agroalimentare, Coldiretti, AGRINSIEME"

y[[11]][2] <- "UNCI Agroalimentare"

y[[12]][7] <- "Coldiretti"

y[[13]][6] <- "CIA, UECOOP, ALLEANZA AGROALIMENTARE, UNCI AGROALIMENTARE"
y[[13]][7] <- "Dott. Giuseppe Blasi - Capo Dipartimento delle politiche europee e internazionali e dello sviluppo rurale del MIPAAF"
y[[13]][8] <- "Dott. Alessandro Calliman - Associazione Veneta Allevatori (AVA) e Savigni Societ? Semplice Agricola"

y[[15]][1] <- "Arcicaccia, Federazione italiana caccia, Libera caccia"
y[[15]][4] <- "Prof. Paolo Ermacora - Universit? di Udine, Dott. Francesco Savian - esperto, Dott. Simone Saro - Servizio fitosanitario e chimico, ricerca, sperimentazione e assistenza tecnica dell'ERSA Friuli Venezia Giulia, Dott. Gianni Tacconi - CREA Genomic Research Center (Fiorenzuola d'Arda), Dott.ssa Laura Bardi - CREA Ingegneria e Trasformazioni Agroalimentari, Laboratorio di biotecnologie microbiche applicate all'agricoltura e all'agroindustria (Torino)"

y[[16]][1] <- "Dott. Gabriele Papa Pagliardini - direttore generale AGEA"
y[[16]][3] <- "Dott. Gabriele Papa Pagliardini - direttore generale AGEA"
y[[16]][9] <- "UNCI Agroalimentare"

y[[17]][1] <- "UNCI Agroalimentare, Coldiretti"
y[[17]][4] <- "Dott. Giuseppe Blasi - Capo Dipartimento delle politiche europee e internazionali e dello sviluppo rurale del MIPAAF"
y[[17]][5] <- "Dott.ssa Chiara Morone - Direzione agricoltura settore Fitosanitario e Servizi Tecnico-Scientifici della regione Piemonte "

y[[19]][2] <- "Coldiretti"
y[[19]][3] <- "Confagricoltura, CIA, Copagri, Coldiretti"
y[[19]][7] <- "Associazione Citt? del Bio"

y[[20]][2] <- "SIGA - FISV, CNR"
y[[20]][5] <- "MIPAAFT - ICQRF"
y[[20]][8] <- "UNCI Agroalimentare"
y[[20]][9] <- "UNCI Agroalimentare, AGRINSIEME, Coldiretti, UECOOP"

y[[21]][5] <- "ANBI Lombardia, ANBI Veneto, ANBI Emilia-Romagna, ANBI Campania, ANBI Sicilia occidentale"
y[[21]][9] <- "Prof.ssa Adriana GALDERISI - Professore associato di Tecnica e Pianificazione Urbanistica dell'Universit? degli Studi della Campania Vanvitelli, Prof. Carmelo DAZZI - Professore di Scienze Agrarie dell'Universit? degli Studi di Palermo"
y[[21]][10] <- " Prof. Paolo PILERI - Professore ordinario di pianificazione e progettazione urbanistica del Politecnico di Milano, Prof. Giuseppe CORTI - Professore ordinario di Pedologia-Dipartimento di Scienze agrarie, alimentari e ambientali dell'Universit? Politecnica delle Marche, Prof. Carlo BERIZZI - Associated Professor in Architectural and Urban Design-Coordinator for International Mobility-Faculty of Engineering dell'Universit? degli Studi di Pavia, Prof.ssa Simona TONDELLI - Professore associato di tecnica e pianificazione urbanistica-Dipartimento di Architettura dell'Universit? degli Studi di Bologna"

y[[22]][1] <- " Prof. Fabio CASTELLI - Presidente della Scuola di ingegneria e ordinario di idrologia e costruzioni idrauliche dell'Universit? degli Studi di Firenze, Prof. Terribile - Professore di pedologia dell'Universit? degli Studi Federico II di Napoli"

y[[23]][6] <- "WWF Italia"

y[[24]] <- append(y[[24]], "CNA - Confederazione Nazionale dell'Artigianato e della Piccola e Media Impresa", after = 6)
y[[24]][9] <- "MIPAAFT - ICQRF e DIQPAI"

y[[25]][1] <- "ANAI, FAI, UNAAPI, Confcommercio"
y[[25]][9] <- "FEDERUNACOMA, UNACMA, CAI, ENAMA"

y[[26]][1] <- "UNAITALIA - Associazione di filiere agroalimentari italiane delle carni e delle uova, ASSALZOO - Associazione nazionale tra i produttori di alimenti zootecnici, ANBI - Associazione nazionale Bonifiche Irrigazioni e miglioramenti Fondiari, Confederazione Italiana Liberi Agricoltori"
y[[26]][2] <- "AGCI-AGRITAL Settore agroittico alimentare, Fedagri-Confcooperative, Legacoop agroalimentare, UNCI Agroalimentare, UECOOP"
y[[26]][3] <- "Confagricoltura, Coldiretti, CIA-Agricoltori italiani, Copagri"
y[[26]][4] <- "Autorit? Garante della Concorrenza e del Mercato, Agrinsieme, Confcommercio, Federdistribuzione Ancc Coop e Ancd Conad, Confartigianato"

nomi <- y
rm(y)

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 7
c9 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c9, "C:/Users/pc/Desktop/Progetto Audizioni/raw_data/commissione9.csv", row.names = FALSE)

