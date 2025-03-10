######################################################
#                     DIZIONARIO                    #
# Associazioni e organizzazioni della società civile #
######################################################

# L'espressione terzo settore identifica quegli enti che operano e si collocano in
# determinati settori, ma non riconducibili né al Mercato né allo Stato;
# è una realtà sociale, economica e culturale in continua evoluzione.

# Loading libraries
library(tidyverse)
library(rvest)


senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")


# Esploriamo gli enti della società civile nei dati della Camera
camera <- read.csv("C:/Users/SImone/Desktop/camera.csv")

civile_camera <- camera %>% filter(`Tipologia soggetto`=="Associazioni e organizzazioni della società civile") %>% select(`Ente/Organizzazione`) %>% unique() #df

# Costruire espressioni regolari per cercare parole esatte
patterns <- paste0("\\b", civile_camera$`Ente/Organizzazione`, "\\b")

# Creare un vettore logico per ogni acronimo
match_logico <- sapply(patterns, function(i) grepl(i, senato$NOMI))

# Estrarre solo gli acronimi che appaiono nei dati come parole a sé stanti o come match esatti.
org_civil <- civile_camera$`Ente/Organizzazione`[apply(match_logico, 2, any)]

# Verifichiamo la bontà delle corrispondenze.

# Falso positivo: associazione libera
org_civil[3] <- "libera associazioni, nomi e numeri contro le mafie"

# Modifica
org_civil[44] <- "telethon"

# Nuove organizzazioni più variazioni nella nomenclatura per scopi di matching
org_civil <- c(org_civil, "age", "cittadinanza attiva", "transparency", "anbima", "unc",
               "unione nazionale consumatori", "associazione mo bast!", "comma 2",
               "associazione italiana diabetici") 


# Rimuoviamo stopwords e punteggiatura dai dati come nella fase preliminare all'analisi del testo.
# Le corrispondenze tra nomi e organizzazioni aumentano.

# Ripetiamo le operazioni nelle righe 21-30, incrociando le variabili NOMI_clean nei dati su camera e senato.
# Troviamo 15 corrispondenze in più.

# Aggiungiamo i nuovi elementi più variazioni (acronimo, nome esteso).
org_civil <- c(org_civil, "associazione sostenitori amici polizia stradale", "asaps", "agesc", "associazione genitori scuole cattoliche",
               "moige", "movimento italiano genitori", "unione superiore maggiori d italia", "usmi", "alleanza italiana sviluppo sostenibile",
               "asvis", "associazione italiana promozione scienza aperta", "coordinamento liste diritto studio", "forum associazioni familiari",
               "associazione nazionale genitori soggetti autistici", "angsa", "associazione italiana turismo responsabile", "aitr", "fire",
               "federazione italiana uso razionale energia", "coordinamento libere associazioni professionali", "colap", "alleanza povertà",
               "anffas", "associazione nazionale famiglie persone disabilità intellettiva relazionale", "federazione italiana superamento handicap",
               "federazione associazioni nazionali persone disabilità", "anfn", "associazione nazionale famiglie numerose", "associazione città bio")

# Organizzazioni in "associazioni dei consumatori"
org_civil <- c(org_civil, "assoutenti", "codici", "centro diritti cittadino", "confconsumatori", "movimento consumatori", "udicon")

# Organizzazioni in FAND - federazione tra le associazioni nazionali delle persone con disabilità
org_civil <- c(org_civil, "anmic", "associazione nazionale mutilati e invalidi civili", "anmil",
               "ente nazionale sordi", "uici", "unione italiana dei ciechi e degli ipovedenti", "anglat")

#saveRDS(org_rappr, file = "[path]/org_rappr.RData")

# Note
# fand - acronimo comune: federazione tra le associazioni nazionali delle persone con disabilità, associazione italiana diabetici
# aisa - acronimo comune: associazione italiana promozione scienza aperta, associazione nazionale imprese salute animale

saveRDS(org_civil, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_società-civile.RData")
