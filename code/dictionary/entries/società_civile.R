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

civile_camera <- camera %>% filter(`Tipologia.soggetto`=="Associazioni e organizzazioni della società civile") %>% select(`Ente.Organizzazione`) %>% unique() #df

# Costruire espressioni regolari per cercare parole esatte
patterns <- paste0("\\b", civile_camera$`Ente.Organizzazione`, "\\b")

# Creare un vettore logico per ogni acronimo
match_logico <- sapply(patterns, function(i) grepl(i, senato$NOMI))

# Estrarre solo gli acronimi che appaiono nei dati come parole a sé stanti o come match esatti.
org_civil <- civile_camera$`Ente.Organizzazione`[apply(match_logico, 2, any)]

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


# La parola "forum" è un pattern specifico della categoria?
forum <- senato %>% filter(str_detect(NOMI_clean, "forum")) %>% pull(NOMI_clean) %>% unique()

org_civil <- c(org_civil, "forum associazione donne giuriste", "forum nazionale associazioni genitori scuola - fonags",
               "federazione degli studenti", "movimento studenti azione cattolica - msac", "unione degli studenti - uds",
               "forum nazionale associazioni studentesche - fonas", "forum nazionale associazioni genitori scuola - fonags",
               "forum nazionale educazione musicale", "forum nazionale salviamo il paesaggio difendiamo i territori",
               "forum italiano movimenti acqua - fima", "forum ex articolo 26", "forum nazionale giovani",
               "forum disuguaglianze diversità", "forum nazionale servizio civile")

# Il pattern "onlus" (organizzazione non lucrativa di utilità sociale) si può associare alla categoria.

senato %>% filter(str_detect(NOMI_clean, "onlus")) %>% pull(NOMI_clean)

org_civil <- c(org_civil, "organizzazione internazionale protezione animali - oipa", "ataaci", "lav- lega anti vivisezione",
               "anffas", "federazione italiana superamento handicap - fish", "federfardis", "save the children", "marevivo")

org_civil <- c(org_civil, "fridays for future", "greenpeace", "transparency", "slow food", "actionaid", "differenza donna")

# Movimento, comitato, fondazione...




















#saveRDS(org_civil, file = "[path]/org_società-civile.RData")

# Note

# Acronimi in comune
# fand: federazione tra le associazioni nazionali delle persone con disabilità, associazione italiana diabetici
# aisa: associazione italiana promozione scienza aperta, associazione nazionale imprese salute animale
# adi: associazione dottorandi e dottori di ricerca, associazione degli italianisti, associazione docenti e dirigenti scolastici,
#      assodanza italia, assemblee di dio in italia, associazione italiana di dietetica e nutrizione medica

# FONAS - Forum Nazionale Associazioni Studentesche
# È composto da 7 associazioni: Federazione degli Studenti (FdS), Movimento Studenti di Azione Cattolica (MSAC),
# Movimento Studenti Cattolici (MSC), Movimento Studentesco Nazionale (MSN), Rete degli Studenti Medi (RSM),
# StudiCentro (SC) e Unione degli Studenti (UdS).

# FONADDS è il Forum delle Associazioni professionali dei Docenti e dei Dirigenti: dove?
# Ne fanno parte: proteo fare sapere, ADI, AIMC, ANDIS, APEF, CIDI, DIESSE, DISAL, FNISM, IRASE, IRSEF/IRFED, LEGAMBIENTE scuola e formazione, MCE, UCIIM.

# Associazioni Professionali (da aggiungere a org_rappr)
# proteo, associazione docenti e dirigenti scolastici, associazione italiana maestri cattolici, associazione nazionale dirigenti scolastici - andis,
# cidi - centro iniziativa democratica insegnanti, diesse - didattica e innovazione scolastica, associazione dirigenti scolastici - disal,
# dirigenti scuole autonome e libere - disal, mce - movimento di cooperazione educativa, uciim

# Forum arte e spettacolo (FAS) - organizzazione rappresentanza
# fpa fotografi - organizzazioni rappresentanza

# Forum PA srl - azienda

# Consiglio nazionale dei consumatori e degli utenti (CNCU) - istituzione
