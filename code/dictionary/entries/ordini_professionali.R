########################
#      DIZIONARIO      #
# Ordini professionali #
########################

# Per creare la lista degli ordini professionali, cerco l'espressione sul motore
# di ricerca Google e confronto più risultati. 

# La voce di Wikipedia è aggiornata e diventa la nostra fonte di riferimento.
# Tuttavia, l'elenco non presenta gli acronimi.

# Il sito "Borsa e Finanza" contiene l'elenco, acronimi compresi.

# Integriamo le informazioni con spunti dai nostri dati per costruire la voce
# per il dizionario.

# Una valida scorciatoia è cercare le espressioni "ordine", "collegio", "consiglio
# nazionale". Ma il matching porterà a risultato anche falsi positivi.

# Procediamo con lo scraping e le modifiche di integrazione.

# Loading packages
library(rvest)
library(tidyverse)
library(quanteda)

link <- "https://borsaefinanza.it/ordini-professionali-tutti-quelli-attivi-in-italia/"

elenco <- read_html(link) %>% html_elements("#article-content-custom ul") %>% html_text2()

ord_prof <- str_split(elenco, "\n") %>% unlist()

ord_prof[1] <- "consiglio nazionale consulenti del lavoro - consiglio nazionale ordine consulenti del lavoro"
ord_prof[11] <- "cnpapal - consiglio nazionale periti agrari e periti agrari laureati"
ord_prof[12] <- "consiglio nazionale periti industriali e periti industriali laureati"
ord_prof[14] <- "collegio nazionale agrotecnici e agrotecnici laureati"
ord_prof[26] <- "consiglio nazionale geometri e geometri laureati"

ord_prof <- c(ord_prof, "consiglio ordine nazionale tecnologi alimentari - ordine tecnologi alimentari")

# Tutto minuscolo + separare acronimi
ord_prof <- tolower(ord_prof)
ord_prof <- str_split(ord_prof, "-|–") %>% unlist() %>% str_trim(side="both")

# Eliminiamo stopwords
stp_wrd <- stopwords('it')
stp_wrd <- setdiff(stp_wrd, "una")

for (i in stp_wrd) {
  ord_prof <- gsub(paste0("\\b", i, "\\b"), "", ord_prof)
}

# Eliminiamo segni di punteggiatura
ord_prof <- gsub("[,’]", "", ord_prof)

# Riduciamo gli spazi bianchi a uno
ord_prof <- gsub("\\s+", " ", ord_prof)

#saveRDS(ord_prof, file = "[path]/ordini_pro.RData")

