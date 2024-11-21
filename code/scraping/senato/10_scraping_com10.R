library("rvest")
library("dplyr")
library("stringr")

# 1. Estrazione atto

atto <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".descrizione em") %>% html_text()
  atto[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- atto

x[[15]] <- append(x[[15]], "Ddl 2401 - Decreto-legge 130/2021 - Contenimento degli effetti degli aumenti dei prezzi nel settore elettrico e del gas naturale", after = 4)
x[[30]] <- append(x[[30]], "Chiusura degli impianti di cracking e degli aromatici di Porto Marghera, nonché sulle conseguenze di carattere ambientale per il sito interessato e per lo sviluppo di processi di transizione ecologica", after = 1)
x[[40]] <- append(x[[40]], "Affare assegnato n. 401 sui sistemi di sostegno e di promozione dei servizi turistici e le filiere produttive associate alla valorizzazione del territorio", after = 1)
x[[43]] <- append(x[[43]], "Atto n. 572 - Proposta di Linee guida per la definizione del Piano nazionale di ripresa e resilienza", after = 0)
x[[43]] <- append(x[[43]], "Atto n. 572 - Proposta di Linee guida per la definizione del Piano nazionale di ripresa e resilienza", after = 1)
x[[43]] <- append(x[[43]], "Affare sulle iniziative di sostegno ai comparti dell'industria, del commercio e del turismo nell'ambito della congiuntura economica conseguente all'emergenza da COVID-19 (n. 445)", after = 9)
x[[54]] <- append(x[[54]], "Affare sulle iniziative di sostegno ai comparti dell'industria, del commercio e del turismo nell'ambito della congiuntura economica conseguente all'emergenza da COVID-19 (n. 445)", after = 0)
x[[69]] <- append(x[[69]], "Vicende complesso industriale ILVA", after = 7)
x[[69]] <- append(x[[69]], "Affare sulle principali aree di crisi industriale complessa in Italia (n. 161)", after = 8)
x[[69]] <- append(x[[69]], "Atto Senato n. 717 (Conversione in legge del decreto legge 25 luglio 2018, n. 91, recante proroga di termini previsti da disposizioni legislative)", after = 9)
x[[70]] <- append(x[[70]], "Atto Senato n. 717 (Conversione in legge del decreto legge 25 luglio 2018, n. 91, recante proroga di termini previsti da disposizioni legislative)", after = 0)

atto <- x

# 2. Estrazione data

data <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".data em") %>% html_text()
  data[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- data

x[[3]] <- x[[3]][-4]
x[[61]] <- append(x[[61]], "01 Ottobre 2019", after = 8)
x[[70]] <- c("26 Luglio 2018", "20 Luglio 2022", "25 Febbraio 2021", "22 Settembre 2021", "20 Settembre 2021", "08 Febbraio 2019", "15 Settembre 2021")

data <- x

# Estrazione commissioni coinvolte

commissione <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".strong") %>% html_text()
  commissione[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- commissione

a <- rep("10 (Industria)", 10)
seq <- c(1,6:10,12,26:31,33:39,41,42,45:64,66:68)

for (i in seq) {
  x[[i]] <- a
}

a <- rep("10 (Industria)", 5)
x[[2]] <- append(x[[2]], a, after = 0)

a <- rep("10 (Industria)", 2)
x[[3]] <- append(x[[3]], a, after = 2)
a <- rep("10 (Industria)", 4)
x[[3]] <- append(x[[3]], a, after = 5)

x[[4]] <- append(x[[4]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 8)
x[[4]] <- append(x[[4]], a, after = 2)

a <- rep("10 (Industria)", 4)
x[[5]] <- append(x[[5]], a, after = 0)
a <- rep("10 (Industria)", 5)
x[[5]] <- append(x[[5]], a, after = 5)

a <- rep("10 (Industria)", 5)
x[[11]] <- append(x[[11]], a, after = 0)
a <- rep("10 (Industria)", 4)
x[[11]] <- append(x[[11]], a, after = 6)

x[[13]] <- append(x[[13]], "10 (Industria) ; X (Att. produttive)", after = 0)
a <- rep("10 (Industria)", 8)
x[[13]] <- append(x[[13]], a, after = 2)

a <- rep("10 (Industria)", 2)
x[[14]] <- append(x[[14]], a, after = 0)
a <- rep("10 (Industria)", 7)
x[[14]] <- append(x[[14]], a, after = 3)

a <- rep("10 (Industria)", 8)
x[[15]] <- append(x[[15]], a, after = 0)
x[[15]] <- append(x[[15]], "10 (Industria)", after = 9)

x[[23]] <- append(x[[23]], "10 (Industria)", after = 1)
x[[23]] <- append(x[[23]], "10 (Industria)", after = 5)
a <- rep("10 (Industria)", 3)
x[[23]] <- append(x[[23]], a, after = 7)

x[[25]] <- append(x[[25]], "2 (Giustizia) ; 10 (Industria)", after = 3)
x[[25]] <- append(x[[25]], "10 (Industria)", after = 4)
x[[25]] <- append(x[[25]], "10 (Industria)", after = 9)

a <- rep("10 (Industria)", 7)
x[[32]] <- append(x[[32]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[32]] <- append(x[[32]], a, after = 8)

a <- rep("10 (Industria)", 9)
x[[40]] <- append(x[[40]], a, after = 0)

a <- rep("10 (Industria)", 2)
x[[43]] <- append(x[[43]], a, after = 0)
a <- rep("10 (Industria)", 7)
x[[43]] <- append(x[[43]], a, after = 3)

x[[44]] <- append(x[[44]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 3)
x[[44]] <- append(x[[44]], a, after = 3)
a <- rep("10 (Industria)", 3)
x[[44]] <- append(x[[44]], a, after = 7)

x[[65]] <- append(x[[65]], "10 (Industria)", after = 0)
a <- rep("10 (Industria)", 8)
x[[65]] <- append(x[[65]], a, after = 2)

a <- rep("10 (Industria)", 7)
x[[69]] <- append(x[[69]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[69]] <- append(x[[69]], a, after = 8)

a <- rep("10 (Industria)", 3)
x[[70]] <- append(x[[70]], a, after = 0)
a <- rep("10 (Industria)", 2)
x[[70]] <- append(x[[70]], a, after = 4)

commissione <- x

# 4. Estrazione nomi

nomi <- vector(mode = "list", length = 70)

for (page_index in 1:70) {
  link <- paste("https://www.senato.it/Leg18/3674?current_page_40461=",page_index,"", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes(".titolo_pubblicato strong") %>% html_text()
  nomi[[page_index]] <- x
  print(paste("Page:", page_index))
  Sys.sleep(5)
}

x <- nomi
x[[3]] <- x[[3]][-4]

# Pre-cleaning

words_to_remove <- "Trasmissione|osservazioni|da parte|Audizione|informale|Audizioni|informali|in videoconferenza|di rappresentanti|proposta emendativa|documento|unitario|congiunte|ulteriori|integrazione|aggiornate|Comunicazioni|di:|Documento|trasmesso|memoria|inviato|Documenti|integrativi|trasmessi|Documentazione|trasmessa"

for (i in 1:70) {
  x[[i]] <- gsub(words_to_remove, "", x[[i]]) %>%
    str_trim(side = "left")
}

words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da"

for (i in 1:70) {
  x[[i]] <- gsub(words2, "", x[[i]]) %>%
    str_trim(side = "left")
}

# Integrazione manuale

x[[2]][6] <- "Assocarta ; Assogasmetano-Federmetano ; Federacciai ; Unionplast ; UNEM"
x[[2]][7] <- "Confindustria Alberghi ; AGCM Autorità Garante Concorrenza e Mercato ; ANCI ; Confirmi Industria ; ART Autorità di Regolazione dei Trasporti ; ASSTEL ; ARERA Autorità di regolazione per energia reti e ambiente ; Confindustria ; Garante per la sorveglianza dei prezzi ; Conferenza Regioni e Province Autonome ; Conferenza Stato Regioni"
x[[2]][8] <- "Assocostieri ; Assogasliquidi-Federchimica ; Assoutenti ; Caritas ; CIB Consorzio Italiano Biogas ; Altroconsumo ; Movimento Consumatori ; ANCE"
x[[2]][9] <- "Assopetroli-Assoenergia ; Federalberghi ; Confesercenti ; UIL ; Federterme ; Confagricoltura ; CISL ; CGIL ; Federazione ANIMA ; FILT CGIL ; Federturismo ; Confcommercio ; UGL ; FINCO"
x[[2]][10] <- "Elettricità futura ; Italia Solare ; Utilitalia ; ABI Associazione Bancaria Italiana ; Confetra ; Confartigianato Imprese ; Confapi ; CNA"

a <- read_html("https://www.senato.it/Leg18/3674?current_page_40461=3") %>%
  html_nodes("#list_40469_0 a") %>% html_text()

a <- a[-16]
  
a <- gsub("Documento", "", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ",")

x[[3]][[1]] <- a

x[[3]][[2]] <- "UNIRIMA ; Alleanza cooperative agroalimentare ; UNCAI"

x[[12]][1] <- "Federmeccanica - FMI-CISL - FIOM-CGIL - UILM-UIL"
x[[12]][6] <- "AN.BTI Associazione nazionale bus turistici italiani ; sindaco di Gradara"

x[[13]][7] <- "Acquirente Unico - ARERA - ENEL - ENI - Terna"

a <- read_html("https://www.senato.it/Leg18/3674?current_page_40461=61") %>%
  html_nodes("#container_40466_6 a") %>% html_text()

a <- a[-c(2,8)]

a <- gsub("Documento|depositato|trasmesso", "", a) %>%
  str_trim(side = "left")

a <- gsub("^da|^dalla|^dall'", "", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ";")

x[[61]][7] <- a

a <- read_html("https://www.senato.it/Leg18/3674?current_page_40461=61") %>%
  html_nodes("#list_40468_7 a") %>% html_text()

a <- a[-c(4,5,6,14)]

a <- gsub("Documento|depositato|Presentazione", "", a) %>%
  str_trim(side = "left")

a <- gsub("^da|^dalla|^dall'", "", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ";")

x[[61]][8] <- a

a <- read_html("https://www.senato.it/Leg18/3674?current_page_40461=61") %>%
  html_nodes("#container_40466_8 a") %>% html_text()

a <- a[-2]

a <- gsub("Memoria|Documento|depositato|depositata|trasmesso|Testo dell'intervento", "", a) %>%
  str_trim(side = "left")

a <- gsub("^da|^dalla|^dall'|^del|^dal", "", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ";")

x[[61]][9] <- a

x[[61]] <- append(x[[61]], "FIM-CISL ; FIOM-CGIL ; UILM-UIL", after = 9)
x[[62]][4] <- "Confartigianato Imprese Marche ; Invitalia ; Confindustria Centro Adriatico ; Confindustria Macerata ; Camera di Commercio delle Marche ; Camera di Commercio di Ancona ; CGIL-CISL-UIL Marche ; Centro Studi CNA Marche ; Regione Marche"
x[[62]][8] <- "Università degli Studi di Bari Aldo Moro Dipartimento di Scienze Biomediche ed oncologia umana - ASL Taranto ; CGIL-CISL-UIL Taranto ; Confartigianato Imprese Taranto"

a <- read_html("https://www.senato.it/Leg18/3674?current_page_40461=63") %>%
  html_nodes("#list_40469_2 a") %>% html_text()
a <- gsub("Documento trasmesso", "", a) %>%
  str_trim(side = "left")

a <- gsub("^da|^dalla|^dall'|^del|^dal", "", a) %>%
  str_trim(side = "left")

a <- gsub("\\s*\\([^\\)]+\\)","", a)
a <- paste(a, collapse = ";")

x[[63]][3] <- a
x[[63]][7] <- "Invitalia"
x[[63]][8] <- "Autorità di Sistema portuale del Mar Ionio Porto di Taranto ; Confindustria Taranto ; CGIL-CISL-UIL Taranto ; COBAS Taranto ; ARPA Puglia ; PeaceLink"

x[[65]][2] <- "Altroconsumo ; Confindustria ; prof.ssa Ilaria Pagani ; Confcommercio ; CNCU - Consiglio Nazionale consumatori e utenti"
x[[65]][5] <- "UGL Metalmeccanici ; UILM ; FIM-CISL - CISL ; FIOM-CGIL - CGIL Nazionale ; Comune di Villanova d'Albenga ; Unione Industriali della Provincia di Savona ; Assessore allo sviluppo economico della Regione Liguria"
x[[65]][10] <- "Sogin"

x[[66]][5] <- "Sogin"
x[[66]][10] <- "Nucleco - Sogin"

x[[67]][1] <- "Enea"

x[[70]][1] <- "Altroconsumo ; Movimento consumatori ; U.Di.Con. Unione per la difesa dei consumatori"
x[[70]][2] <- "CNA ; Energia Libera ; Utilitalia ; Confartigianato Imprese ; Elettricità futura ; AIGET ; Proxigas"
x[[70]][6] <- "Confindustria"
x[[70]][7] <- "Autorità Garante della Concorrenza e del Mercato ; Agrinsieme ; Confcommercio ; Federdistribuzione - Ancc Coop - Ancd Conad ; Confartigianato"

nomi <- x

a <- unlist(commissione)
b <- unlist(nomi)
c <- unlist(atto)
d <- unlist(data)

# Dataframe Commissione 7
c10 <- data.frame(COMMISSIONE=a, NOMI=b, ATTO=c, DATA=d)

write.csv(c10, "[path]/audizioni_informali/data/raw_data/senato/commissione10.csv", row.names = FALSE)
