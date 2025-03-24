############################
#   Preprocessing Senato   #
############################

library(tidyverse)
library(quanteda)

# Merge the individual dataframes for commissions

x <- c(1:3,5:14)
data <- list()

for (i in x) {
  data[[i]] <- read.csv(paste0("[path]/C",i,".csv"), header = TRUE, stringsAsFactors = FALSE)
  }

# OR

path <- paste0("[path]/C",1:14,".csv")
path <- path[-4]

data <- lapply(path, read.csv)


senato <- bind_rows(data)

# Cleaning

# Remove NA values in NOMI (missing info)
sum(is.na(senato$NOMI))
which(is.na(senato$NOMI))
senato <- senato[!is.na(senato$NOMI),]

# Tutto minuscolo
senato$NOMI <- tolower(senato$NOMI)
senato$ATTO <- tolower(senato$ATTO)

# Check for duplicated rows. Si tratta delle audizioni a commissioni riunite
sum(duplicated(senato)) #957
senato <- unique(senato)

# Funzione (f1) per visualizzare i nomi che contengono un pattern specifico + numero di riga
# Inserire il valore in grepl
senato %>% mutate(nrow=row_number()) %>%
  filter(grepl("", NOMI)) %>% select(NOMI, nrow)

# Correggi errori

z <- c(6895,6896,6897)
for(i in z) {
  senato$NOMI[i] <- gsub("(università)", " \\1", senato$NOMI[i]) # aggiungi uno spazio prima della parola "università" nelle seguenti righe
}

senato$NOMI[6899] <- gsub("(\\!)", "\\1 ", senato$NOMI[6899]) # aggiungi uno spazio dopo il segno "!"
senato$NOMI[1359] <- gsub("(\\!)", "i", senato$NOMI[1359])

senato$NOMI[7021] <- gsub("100", "\\1 ", senato$NOMI[7021]) # aggiungi uno spazio dopo il segno "!"

# Uso f1 per sostituire il punto interrogativo con il carattere mancante
senato$NOMI[1423] <- gsub("(\\?)", "à", senato$NOMI[1423])
senato$NOMI[1611] <- gsub("(\\?)", "à", senato$NOMI[1611])
senato$NOMI[2035] <- gsub("(\\?)", "é", senato$NOMI[2035])
senato$NOMI[2091] <- gsub("(\\?)", "à", senato$NOMI[2091])

# Parole con l'apostrofo
words_apostrophe <- str_extract_all(senato$NOMI, "\\b\\w+'")
words_apostrophe <- unlist(words_apostrophe) %>% unique()

# Correggiamo gli apostrofi in accenti

senato$NOMI <- gsub("autorita'", "autorità", senato$NOMI)
senato$NOMI <- gsub("calo'", "calò", senato$NOMI)
senato$NOMI <- gsub("miccu'", "miccù", senato$NOMI)
senato$NOMI <- gsub("elettricita'", "elettricità", senato$NOMI)
senato$NOMI <- gsub("professionalita'", "professionalità", senato$NOMI)
senato$NOMI <- gsub("fimmano'", "fimmanò", senato$NOMI)
senato$NOMI <- gsub("liberta'", "libertà", senato$NOMI)
senato$NOMI <- gsub("comunita'", "comunità", senato$NOMI)
senato$NOMI <- gsub("parita'", "parità", senato$NOMI)
senato$NOMI <- gsub("cosi'", "così", senato$NOMI)
senato$NOMI <- gsub("piu'", "più", senato$NOMI)
senato$NOMI <- gsub("e'", "è", senato$NOMI)
senato$NOMI <- gsub("papa'", "papà", senato$NOMI)
senato$NOMI <- gsub("societa'", "società", senato$NOMI)
senato$NOMI <- gsub("priorita'", "priorità", senato$NOMI)
senato$NOMI <- gsub("universita'", "università", senato$NOMI)
senato$NOMI <- gsub("sanita'", "sanità", senato$NOMI)

# Eliminiamo il pattern ll'
senato$NOMI <- gsub("\\b(ll')\\b", "", senato$NOMI)


# Processiamo la variabile NOMI - rimozione punteggiatura e stopwords

# La punteggiatura porta significato. Possiamo decidere di eliminarla, perdendo informazione.
# Eliminiamo specifici segni di punteggiatura, non quelli facenti parte dei nomi degli enti (come #vita, FB&Associati)

senato$NOMI_clean <- gsub("\\.", "", senato$NOMI)
senato$NOMI_clean <- gsub("[,;'\"()\\-]", " ", senato$NOMI_clean) 
senato$NOMI_clean <- gsub("/", " ", senato$NOMI_clean) # aggiunto
senato$NOMI_clean <- gsub("\\s+", " ", senato$NOMI_clean) # Rimuovi spazi multipli

# Rimozione stopwords a meno che non siano parte della nomenclatura degli stakeholders.
stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, c("una", "nostra"))

# Funzione di pulizia
pulisci_testo <- function(testo) {
  for (i in stp_wrd) {
    testo <- gsub(paste0("\\b", i, "\\b"), "", testo)
  }
  testo <- trimws(gsub("\\s+", " ", testo))  # Rimuove spazi multipli e all'inizio/fine della stringa
  return(testo)
}

senato$NOMI_clean <- pulisci_testo(senato$NOMI_clean)
senato <- unique(senato)


# La punteggiatura porta significato. Possiamo decidere di eliminarla, perdendo informazione.

# Il punto "." è usato
# nelle abbreviazioni delle cariche: prof.ssa
# negli acronimi: ce.pa centro patronati/s.p.a. società per azioni

# La virgola "," può segnalare
# il ruolo, l'ente dell'audito: andrea orlando, ministro del lavoro e delle politiche sociali
# un elenco, adempiendo alla sua funzione grammaticale: comando unità forestali, ambientali e agroalimentari dell'arma dei carabinieri
# un'audizione congiunta: confartigianato, cna e casartigiani

# Il trattino "-" può segnalare
# un'audizione congiunta: ondata - trasparency international
# un acronimo: coni - comitato olimpico nazionale italiano
# un nome: motus-e
# un rapporto di associazione tra due enti: confsal-snalv

# Le parentesi segnalano
# acronimi: acer (european agency for the cooperation of energy regulators)/consorzio italiano biogas (cib)
# l'ente dell'audito: prof. gianfranco viesti (università di bari)

# Il punto e virgola ";" segnala più auditi nello stesso giorno sullo stesso atto.
# Non si può escludere che la virgola "," e la congiunzione "e" non siano usati
# allo stesso scopo.

# Le virgolette sono usate nelle citazioni: movimento "la lupa"

# I segni "@" "#" "/" "!" "&" sono parte di nomi di enti

# Eliminiamo specifici segni di punteggiatura, non quelli facenti parte dei nomi degli enti (es: #vita, FB&Associati)

write.csv(senato, "[path]/dataset_senato.csv", row.names = FALSE)



# Manutenzione dati
# obiettivo: standardizzare la nomenclacura degli stakeholders
# applichiamo il metodo del coseno per risolvere i mispelling più ovvi?


senato[5821,2] <- "associazione italiana diabetici" # rimosso acronimo in comune
senato[546,2] <-"tavolo permanente delle federazioni bandistiche italiane"


senato[4738,2] <- c("associazioni dei consumatori", "altroconsumo", "unc unione nazionale consumatori", "associazione mo bast!")
senato[5364,2] <- "transport & environment"

senato[7001,2] <- "associazione italiana promozione scienza aperta"
senato[732,2] <- "associazione italiana promozione scienza aperta"


# Forum terzo settore = Forum nazionale terzo settore
# Fondazione telethon = telethon

senato[43,2] <- "associazione docenti e dirigenti scolastici italiani"
senato[1045,2] <- "assemblee di dio in italia"

senato[237,2] <- "actionaid ondata transparency international italia"
