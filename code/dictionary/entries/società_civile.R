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

# Rimuoviamo stopwords e punteggiatura dai dati come nella fase preliminare all'analisi del testo.
# Le corrispondenze tra nomi e organizzazioni aumentano.

# Incrociamo le variabili NOMI_clean nei dati su camera e senato.

civile_camera <- camera %>% filter(`Tipologia.soggetto`=="Associazioni e organizzazioni della società civile") %>% select(`NOMI_clean`) %>% unique() #df

# Costruire espressioni regolari per cercare parole esatte
patterns <- paste0("\\b", civile_camera$`NOMI_clean`, "\\b")

# Creare un vettore logico per ogni acronimo
match_logico <- sapply(patterns, function(i) grepl(i, senato$NOMI_clean))

# Estrarre solo gli acronimi che appaiono nei dati come parole a sé stanti o come match esatti.
org_civile <- civile_camera$`NOMI_clean`[apply(match_logico, 2, any)]

# Verifichiamo la bontà delle corrispondenze.

# Modifica
org_civile[4] <- "libera associazioni, nomi e numeri contro le mafie"
org_civile[58] <- "telethon"

# Nuove organizzazioni più variazioni nella nomenclatura per scopi di matching
org_civile <- c(org_civile, "age", "cittadinanza attiva", "transparency", "anbima", "unc",
               "unione nazionale consumatori", "associazione mo bast!", "comma 2",
               "associazione italiana diabetici") 



# Aggiungiamo i nuovi elementi più variazioni (acronimo, nome esteso).
org_civile <- c(org_civile, "associazione sostenitori amici polizia stradale", "asaps", "agesc", "associazione genitori scuole cattoliche",
               "moige", "movimento italiano genitori", "unione superiore maggiori d italia", "usmi", "alleanza italiana sviluppo sostenibile",
               "asvis", "associazione italiana promozione scienza aperta", "coordinamento liste diritto studio", "forum associazioni familiari",
               "associazione nazionale genitori soggetti autistici", "angsa", "associazione italiana turismo responsabile", "aitr", "fire",
               "federazione italiana uso razionale energia", "coordinamento libere associazioni professionali", "colap", "alleanza povertà",
               "anffas", "associazione nazionale famiglie persone disabilità intellettiva relazionale", "federazione italiana superamento handicap",
               "federazione associazioni nazionali persone disabilità", "anfn", "associazione nazionale famiglie numerose", "associazione città bio")

# Organizzazioni in "associazioni dei consumatori"
org_civile <- c(org_civile, "assoutenti", "codici", "centro diritti cittadino", "confconsumatori", "movimento consumatori", "udicon")

# Organizzazioni in FAND - federazione tra le associazioni nazionali delle persone con disabilità
org_civile <- c(org_civile, "anmic", "associazione nazionale mutilati e invalidi civili", "anmil",
               "ente nazionale sordi", "uici", "unione italiana dei ciechi e degli ipovedenti", "anglat") %>% unique()


# Espandiamo la categoria, continuando il ragionamento.
# La parola "forum" è un pattern specifico della categoria?
forum <- senato %>% filter(str_detect(NOMI_clean, "forum")) %>% pull(NOMI_clean) %>% unique()

# No, la parola forum non è esclusiva del terzo settore. Compaiono enti appartenenti ad altre categorie.
# individuiamo quelli della società civile.
org_civile <- c(org_civile, "forum associazione donne giuriste", "forum nazionale associazioni genitori scuola", "fonags",
               "forum nazionale associazioni studentesche", "fonas",
               "federazione degli studenti", "movimento studenti azione cattolica", "msac", "rete degli studenti medi",
               "rete degli studenti", "unione degli studenti", "uds",
               "forum nazionale educazione musicale", "forum nazionale salviamo il paesaggio difendiamo i territori",
               "forum italiano movimenti acqua", "fima", "forum ex articolo 26", "forum nazionale giovani",
               "forum disuguaglianze diversità", "forum nazionale servizio civile", "forum associazioni familiari",
               "forum terzo settore", "forum nazionale terzo settore")

# FONAS - Forum Nazionale Associazioni Studentesche - società civile
# È composto da 7 associazioni: Federazione degli Studenti (FdS), Movimento Studenti di Azione Cattolica (MSAC),
# Movimento Studenti Cattolici (MSC), Movimento Studentesco Nazionale (MSN), Rete degli Studenti Medi (RSM),
# StudiCentro (SC) e Unione degli Studenti (UdS).



# INSERZIONE sindacati_org-categoria: il ragionamento è stato sviluppato qui e poi trascritto

# FONADDS è il Forum delle Associazioni professionali dei Docenti e dei Dirigenti - organizzazione rappresentanza
# Ne fanno parte: proteo fare sapere, ADI, AIMC, ANDIS, APEF, CIDI, DIESSE, DISAL, FNISM, IRASE, IRSEF/IRFED, LEGAMBIENTE scuola e formazione, MCE, UCIIM.

# Associazioni Professionali (da aggiungere al vettore org_rappr perché presenti nei dati)
# proteo, associazione docenti e dirigenti scolastici, associazione italiana maestri cattolici, associazione nazionale dirigenti scolastici - andis,
# cidi - centro iniziativa democratica insegnanti, diesse - didattica e innovazione scolastica, associazione dirigenti scolastici - disal,
# dirigenti scuole autonome e libere - disal, mce - movimento di cooperazione educativa, uciim

# forum arte e spettacolo (fas) - organizzazione rappresentanza

vec_rappr <- c("forum associazioni professionali docenti dirigenti", "fonadds",
               "proteo", "associazione docenti e dirigenti scolastici", "associazione italiana maestri cattolici", "associazione nazionale dirigenti scolastici", "andis",
               "cidi", "centro iniziativa democratica insegnanti", "diesse", "didattica e innovazione scolastica", "associazione dirigenti scolastici", "disal",
               "dirigenti scuole autonome e libere", "disal", "mce", "movimento di cooperazione educativa", "uciim",
               "forum arte e spettacolo", "fas")


# INSERZIONE azienda privata

privati <- "forum pa"



# ONLUS

# Il pattern "onlus" (organizzazione non lucrativa di utilità sociale) si può associare alla categoria.

onlus <- senato %>% filter(str_detect(NOMI_clean, "onlus")) %>% pull(NOMI_clean)

# Possiamo usare il pattern "onlus" ma, nei casi in cui è omesso, dobbiamo individuare la sigla o il nome esteso dell'ente.
onlus <- c("onlus", "organizzazione internazionale protezione animali", "oipa", "ataaci", "akita rescue", "pet rescue",
           "lav", "lega anti vivisezione", "anffas", "federazione italiana superamento handicap", "fish", "federfardis",
           "save the children", "marevivo")



# FONDAZIONI

fond_senato <- senato %>% filter(str_detect(NOMI_clean, "fondazione")) %>% pull(NOMI_clean) %>% unique()

# Non tutte le fondazioni o i comitati ricadono nella società civile. La forma giuridica non determina la finalità.
# I nostri criteri: se iscritto al RUNTS o ETS -> società civile (natura giuridica)
# Altrimenti, chi li finanzia o controlla? Che funzione svolgono?

fond_civile <- fond_senato[c(1,3,5,7:9,12,14,16:19,23,27,31:35)]
fond_ricerca <- fond_senato[c(2,4,10,24,29,30,38)]
fond_ist <- fond_senato[c(6,25,28)]

fond_civile <- c(fond_civile, "fondazione bellisario", "telethon")
fond_ricerca <- c(fond_ricerca, "fondazione ricerca salute")
fond_ist <- c(fond_ist, "fondazione scuola beni attività culturali", "sbac", "orchestra roma lazio", "ico")


# COMITATI

comit_senato <- senato %>% filter(str_detect(NOMI_clean, "comitato")) %>% pull(NOMI_clean) %>% unique()

comit_civile <- comit_senato[c(1,3,4,5,6,7,10,26,27,28,29,30,31,32,33,34,35,40,41,49)]
comit_rappr <- comit_senato[c(2,8,9,22,45)]
comit_ist <- comit_senato[c(11,38)]

# invece di aggiungere la stringa (che può contenere altre info come il nome e il ruolo della persona
# in rappresentanza dell'ente), salviamo solo l'ente nel vettore giusto; e isoliamo gli acronimi.

comit_civile <- c(comit_civile, "comitato trenitalia nuorese", "comitato vigilanza nucleare")
comit_rappr <- c(comit_rappr, "cobti", "comitato nazionale danza arte spettacolo", "condas", "cism", "comitato italiano scienze motorie",
                 "codim", "comitato docenti indirizzo musicale", "comitato ippico guidatori allenatori", "ciga", "comitato air",
                 "autonoleggiatori italiani riuniti")
comit_ist <- c(comit_ist, "edufin", "comitato nazionale universitario", "cnu", "comitato apprendimento pratico musica studenti", "cnapm",
               "conferenza direttori conservatori musica", "comitato tecnico scientifico", "comitato olimpico nazionale italiano", "coni",
               "comitato tecnico paralimpico", "cip", "comitato esperti", "comitato organizzativo milano cortina", "comitato nazionale bioetica",               "comitato scientifico futuro europa")
comit_ricerca <- c("accademia georgofili", "comitato glaciologico italiano")


# Aggiungiamo altre organizzazioni

org_civile <- c(org_civile, onlus, fond_civile, comit_civile, "fridays for future", "greenpeace", "transparency", "slow food", "actionaid",
               "differenza donna", "unicef", "ape", "associazione progetto endometriosi", "legambiente", "pro natura", "anfaa") %>% unique()


# Save data
saveRDS(org_civile, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_società-civile.RData")





# NOTES

# Fondazioni e comitati di natura istituzionale in institutions
# I vettori vec_rappr e comit_rappr in sindacati_org-categoria
# "fondazione fs" in partecipate

# Nuove categorie:
centri_ricerca <- c(fond_ricerca, comit_ricerca, "censis")
privati <- "forum pa"



# Pattern da esplorare: movimento, precari



# Acronimi in comune
# fand: federazione tra le associazioni nazionali delle persone con disabilità, associazione italiana diabetici
# aisa: associazione italiana promozione scienza aperta, associazione nazionale imprese salute animale
# adi: associazione dottorandi, dottori di ricerca e ricercatori in italia; associazione dottorandi e dottori di ricerca (italiani);
#      associazione degli italianisti; associazione docenti e dirigenti scolastici (italiani); assodanza italia
#      associazione italiana di dietetica e nutrizione clinica
#      assemblee di dio in italia


# unione superiore maggiori d italia - chiesa, inserita in società civile, meglio in altro?




# Revisione

# Partiamo dal vettore nel dizionario

dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")
dict10 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")

dizionario <- c(dizionario_pt1, dizionario_babele, dict10)
civil <- dizionario[["soc_civile"]]

# Carichiamo il vettore 'Istituzioni'
ist <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised.RData")

# istituzioni: centro regionale s alessio, museo egizio, museo glauco lombardi
ist[["v20"]] <- c(ist[["v20"]], "centro regionale s alessio") # Aziende, enti e strutture sociosanitarie
ist[["v8"]] <- c(ist[["v8"]], "museo egizio", "museo glauco lombardi") # Enti pubblici culturali, ricreativi e sportivi
ist <- unlist(ist)

# Rimozioni da s. civile
intersect(civil, ist)
civile <- setdiff(civil, ist)

# Rimozioni da ist
ist <- ist[ist != "astral"]
ist <- ist[ist != "sviluppumbria"]

# salviamo il vettore istituzioni
saveRDS(ist, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution-revised.RData")


# Carichiamo il vettore 'Categoria'
cat <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_categoria-revised.RData")

# controlliamo gli elementi in comune
misclass <- intersect(cat, civile)
# rimozione da civile
civile <- setdiff(civile, misclass)
cat <- cat[cat != "fima"]

# Spostiamo gli attori misclassificati in civile senza dividerli nei sottotipi di categoria.

cat <- c(cat, "federazione italiana mercanti arte", "colap", "civita", "nuovo imaie", "fondazione nazionale ricerca commercialisti", "fondazione sviluppo sostenibile",
         "associazione principesca", "comitato venezia lavora", "facciamo la conta", "coordinamento free", "gbc italia",
         "ampress", "parks", "liberi uguali", "airicerca", "attrici attori uniti", "granosalus", "research4life",
         "modomo", "cnps", "cresco", "aeranti corallo", "valore d",
         "comitato idonei vvf", "comitato idonei vvf 250", "comitato sud italia docenti precari terza fascia", "comitato docenti idonei ruolo suppletive concorso 2016",
         "comitato spontaneo concorso dsga", "comitato uniti terza fascia", "comitato difendiamo concorso dsga",
         "varie associazioni precari", "docenti ruolo ingabbiati ogni ordine grado percorsi abilitanti", "docenti precari a066",
         "docenti precari scuola pordenone", "comunicato diritti docenti senza 180x3", "gruppi facebook docenti terza fascia 2020",
         "docenti didattica musica gruppo operativo ddm go", "movimento dignità docenza universitaria",
         "rete 29 aprile", "insegnanti formati preparati",
         "associazione riders decreto", "riders union bologna",
         "european dance union", "danza error system", "parks liberi uguali", "lea liberi editori autori", "ampress attività motoria prevenzione sviluppo salute")

# flag categoria: fire, fondazione nazionale ricerca commercialisti, fondazione sviluppo sostenibile, gbc italia, italia solare, airicerca, research4life

intersect(cat, civile)
civile <- setdiff(civile, cat)

# salviamo il vettore categoria
saveRDS(cat, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/cat_revised_up.RData")



# Centri studi (con expertise) in Altro

centri_studi <- c("fondazione italia digitale", "fondazione gimbe", "fondazione astrid", "fondazione centro studi doc", "associazione italiana promozione scienza aperta",
                  "acbs", "airdm", "centro studi livatino")

# Società scientifiche (con expertise) in Altro

soc_scientifiche <- c("adapt", "aidlass", "sigg", "simeu", "siot", "sifo", "sirm", "sinpia", "fimar", "aiom", "sit")

altro <- c(centri_studi, soc_scientifiche)

intersect(civile, altro)

civile <- setdiff(civile, altro)
civile <- c(civile, "pro test italia")

# salviamo altro
saveRDS(altro, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_altro.RData")


# Privati

privati <- c("proges", "coopculture")
intersect(civile, privati)
civile <- setdiff(civile, privati)

# salviamo privati
saveRDS(privati, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_privati.RData")


# Expertise in civile: opera nazionale montessori. isde, centro italiano riqualificazione fluviale, patto per la scienza, ail, aism, onap, fondazione una, telethon, macula, asvis

# salviamo civile aggiornato
saveRDS(civile, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/civile_update.RData")





