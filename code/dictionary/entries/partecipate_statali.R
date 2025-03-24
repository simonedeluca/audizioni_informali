###############################################
#                 DIZIONARIO                  #
# Aziende e società a partecipazione pubblica #
###############################################


# Loading libraries
library(rvest)
library(tidyverse)
library(quanteda)
library(readxl)

senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")


# Sul sito del Ministero dell'Economia e delle Finanze, è pubblicato l'archivio storico delle partecipazioni dello Stato.
# (https://www.de.mef.gov.it/it/attivita_istituzionali/partecipazioni/elenco_partecipazioni/)

# È possibile scaricare i dati delle partecipazioni pubbliche dalla sezione open data.
# (https://www.de.mef.gov.it/it/attivita_istituzionali/partecipazioni_pubbliche/open_data_partecipazioni/dati_partecipazioni_2021.html)

# Procediamo con lo scraping delle principali controllate al termine dell'anno 2024.


# PARTECIPATE MINISTERO

link <- "https://www.de.mef.gov.it/it/attivita_istituzionali/partecipazioni/elenco_partecipazioni/"
partecipate <- read_html(link) %>% html_elements(".text ol a") %>% html_text2()

# Pulizia
contr_state <- str_to_lower(partecipate)
contr_state <- gsub("\\.", "", contr_state)
contr_state <- gsub("\\)", "", contr_state)
contr_state <- gsub("spa|srl|2020|holding nv", "", contr_state)

contr_state <- str_split(contr_state, "-|–|\\(")
contr_state <- unlist(contr_state)
contr_state <- trimws(contr_state)

contr_state <- contr_state[-37]
contr_state[38] <- "investimenti immobiliari italiani società di gestione del risparmio"

# Aggiungo varianti dei nomi: abbreviativi o errori di battitura presenti nei dati.
contr_state <- c(contr_state, "ferrovie dello stato", "italia trasportoaereo", "pagopa")

# Preprocessing dizionario

# Rimuovi apostrofo

contr_state <- gsub("’", " ", contr_state)

# Rimuovi stopwords

stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

contr_state <- lapply(contr_state, function(x) {
  for (i in stp_wrd) {
    x <- gsub(paste0("\\b", i, "\\b"), "", x)
  }
  return(x)
})

contr_state <- unlist(contr_state)
contr_state <- gsub("\\s+", " ", contr_state)

contr_state <- contr_state[-19] # statuto


# PARTECIPATE CAMERA

camera <- read.csv("C:/Users/SImone/Desktop/camera.csv")

partecipate_camera <- camera %>% filter(`Tipologia.soggetto`=="Aziende e società a partecipazione pubblica") %>% select(`Ente.Organizzazione`) %>% unique() #df

partecipate_camera <- camera %>% filter(`Tipologia.soggetto`=="Aziende e società a partecipazione pubblica") %>% pull(`Ente.Organizzazione`) %>% unique() #vector

partecip_camera <- gsub("\\.|\\)", "", partecipate_camera)
partecip_camera <- gsub("'", " ", partecip_camera)
partecip_camera <- gsub("spa|rappresentanti", "", partecip_camera)

for (i in stp_wrd) {
  partecip_camera <- gsub(paste0("\\b", i, "\\b"), "", partecip_camera)
}

partecip_camera <- str_split(partecip_camera, "-|\\(")
partecip_camera <- unlist(partecip_camera)
partecip_camera <- gsub("\\s+", " ", partecip_camera) %>% trimws() %>% unique()

partecip_camera <- partecip_camera[-c(27,39,48)]

# Perché associazione libera? borsa italiana?


# FONTE ESTERNA

link <- "https://www.economia-italia.com/aziende-partecipate-e-controllate-da-stato-italiano"

source1 <- read_html(link) %>% html_elements(".entry-content ul:nth-child(36) li a") %>% html_text2() #società quotate in borsa

source2 <- read_html(link) %>% html_elements(".entry-content ul:nth-child(38) li") %>% html_text2() #società con strumenti finanziari quotati

source3 <- read_html(link) %>% html_elements(".entry-content ul:nth-child(40) li") %>% html_text2() #società proprietà dello stato ma non quotate in borsa
source3[23] <- "EUtalia Studiare Sviluppo srl (100%)"

table  <- read_html(link) %>% html_element("tbody") %>% html_text2()

table <- str_split(table, "\t")
table <- unlist(table)
table <- str_split(table, "\n")
table <- unlist(table)

col1 <- seq(1, 45, 3)
col2 <- seq(2, 45, 3)
col3 <- seq(3, 45, 3)

table <- data.frame(nome = table[col1],
                    settore = table[col2],
                    quota_stato = table[col3])

source_ext <- c(source1, source2, source3, table$nome)
source_ext <- str_to_lower(source_ext)
source_ext <- gsub("\\(\\d[^)]*\\)", "", source_ext)


source_ext <- gsub("\\.", "", source_ext)
source_ext <- gsub("spa|srl|holding nv|2020", "", source_ext)

source_ext <- str_split(source_ext, "–")
source_ext <- unlist(source_ext)

source_ext <- str_split(source_ext, "\\(")
source_ext <- unlist(source_ext)

source_ext <- source_ext[-29]
source_ext[30] <- "investimenti immobiliari italiani società di gestione del risparmio"

source_ext <- gsub("’|\\)", " ", source_ext)

for (i in stp_wrd) {
  source_ext <- gsub(paste0("\\b", i, "\\b"), "", source_ext)
}

source_ext <- gsub("\\s+", " ", source_ext) %>% trimws() %>% unique()


# SPA IN ISTAT
istat <- read_excel("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/Istat2022.xlsx")
istat$ENTI <- str_to_lower(istat$ENTI)

istat$ENTI <- gsub("\\.", "", istat$ENTI)
istat_spa <- istat %>% filter(str_detect(ENTI, "\\bspa\\b"))

istat_spa <- istat_spa %>% pull("ENTI")

istat_spa[18] <- "ricerca sul sistema energetico - rse"
istat_spa[59] <- "aisa - arezzo impianti e servizi ambientali"
istat_spa[51] <- "porto turistico vieste"
istat_spa[71] <- "fira - finanziaria regionale abruzzese"
istat_spa[73] <- "finanziaria ligure per lo sviluppo economico - filse"
istat_spa[74] <- "finanziaria regionale per lo sviluppo del molise - finmolise"
istat_spa[77] <- "friuli venezia giulia strade"
istat_spa[85] <- "interventi geo ambientali - igea"
istat_spa[106] <- "quadrilatero marche umbria"
istat_spa[108] <- "fiera di brescia"
istat_spa[110] <- "sise – siciliana servizi emergenza"
istat_spa[116] <- "sistemi per la meteorologia e l’ambiente – sma campania"
istat_spa[120] <- "società di committenza – scr piemonte"
istat_spa[122] <- "società immobiliare nuove terme di castellammare di stabia - sint"
istat_spa[124] <- "autostrada del brennero – autobrennero"
istat_spa[126] <- "società per la logistica merci - slm"
istat_spa[129] <- "società riscossioni - soris"

istat_spa <- istat_spa[-c(42,53)]

vec_pulizia <- c("spa", "liquidazione", "coatta amministrativa", "in breve", "società per azioni", "sigla", "abbreviata", "siglabile", "approvvigionamento idrico", "fallimento", "unipersonale")

for (i in vec_pulizia) {
  istat_spa <- gsub(paste0("\\b", i, "\\b"), "", istat_spa)
}

for (i in stp_wrd) {
  istat_spa <- gsub(paste0("\\b", i, "\\b"), "", istat_spa)
}

istat_spa <- str_split(istat_spa, "–")
istat_spa <- unlist(istat_spa)

istat_spa <- str_split(istat_spa, "-")
istat_spa <- unlist(istat_spa)



istat_spa <- gsub("[,;:’'\"“”()\\-]", " ", istat_spa)

istat_spa <- gsub("\\s+", " ", istat_spa) %>% trimws()

istat_spa <- istat_spa[istat_spa != ""] %>% unique()

# Quali elementi del vettore trovano corrispondenza nei dati?

result <- sapply(istat_spa, function(i) {
  any(str_detect(senato$NOMI, paste0("\\b", i, "\\b")))
})

# Mostra gli elementi che sono stati trovati come parole a sé stanti
enti_istat <- istat_spa[result] # 19



# Quattro insiemi:

# vettore scraping ministero: contr_state #57
# dati Camera: partecip_camera #58
# fonte esterna: source_ext #58
# spa in istat presenti nei dati: enti_istat # 19 

intersect(contr_state, source_ext) # 46 my list
part_state <- c(contr_state, source_ext) %>% unique()


intersect(part_state, partecip_camera) # 18

part_camera <- setdiff(partecip_camera, part_state) 

# Quali elementi del vettore trovano corrispondenza nei dati?

result <- sapply(part_camera, function(i) {
  any(str_detect(senato$NOMI, paste0("\\b", i, "\\b")))
})

# Mostra gli elementi che sono stati trovati come parole a sé stanti
enti_camera <- part_camera[result] # 12
enti_camera <- enti_camera[-1]

# Aggiungiamo le partecipate in camera
part_state <- c(part_state, enti_camera)


intersect(enti_istat, part_state)
setdiff(enti_istat, part_state)

# Aggiungiamo le partecipate in istat
part_state <- c(part_state, "ricerca sistema energetico", "rse", "ales", "astral", "sviluppumbria")

saveRDS(part_state, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/part_state.RData")

