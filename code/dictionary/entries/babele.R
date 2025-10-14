library(tidyverse)
library(quanteda)
library(quanteda.textstats)

result  <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/result_pt1.csv", header = TRUE, stringsAsFactors = FALSE)
senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")

# Filtriamo i nomi con zero corrispondenze nel dizionario
babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 1676 enti da classificare

# Cerchiamo pattern specifici di categoria, nomi o aggettivi che identificano una classe specifica o che riducano le opzioni.

# Ad esempio, i pattern "associazion*" e "asso*" catturano quasi 300 stringhe.
# Ancora, l'aggettivo "precari" cattura comitati della società civile, sopratutto legati al mondo della scuola.


# ASSOCIAZIONISMO

# L'universo delle associazioni nel nostro lavoro comprende principalmente due galassie:
# quello delle associazioni professionali e di rappresentanza e quello delle associazioni della società civile.

# I risultati necessitano un filtraggio, con un occhio agli acronimi e alle denominazioni estese perché
# le stringhe possono contenere uno o l'altra e non necessariamente la nomenclatura doppia.

associazionismo <- babele %>% filter(str_detect(text, "associazione")) %>% arrange(text)

# Facciamo la ricerca ente per ente sul web per l'assegnazione dell'etichetta, in stile umano e non artificiale.

# Fino al risultato 54, sono tutte associazioni di categoria tranne la riga 7 e 52.
# C'è un modo per estrarre questi acronimi: precedono la parola "associazione"

ass_split <- associazionismo %>%
  filter(row_number() <= 54) %>%
  mutate(acrn = str_extract(text, ".*(?=associazione)"),
         denom_estesa = str_extract(text, "(associazione).*"))

ass_split[34,2] <- "anbi"
ass_split[47,2] <- "anpit"

ass_split <- ass_split[-c(7,52), ] # contiene lista di associazioni a rappresentanza delle categorie, in formato acronimo e nome esteso

# Assegniamo la stringa al vettore della categoria scelta.

centr_ricerca <- associazionismo$text[c(88,124,138,191,221,222)]

soc_civile <- associazionismo$text[c(7,56,59,62,64,66,67,68,70,71,72,75,76,79,80,
                              82,83,84,87,93,94,95,101,104,113,116,118,129,
                              133,143,145,146,151,158,176,186,192,198,200,201,
                              206,208,213,215,216,226,231,236,237,238,243)]

# Agenzie regionali, associazioni di promozione del territorio, formazione (ibrido tra civile/ricerca/istituzione).
# Ci sono gli estremi per una sottocategoria "associazione di enti pubblici"?
istituzioni <- associazionismo$text[c(52,65,74,77,78,99,166,187,189,210,220)]

seq <- 1:nrow(associazionismo)
index_label <- c(88,124,138,191,221,222, # centri di ricerca
                 7,56,59,62,64,66,67,68,70,71,72,75,76,79,80,
                 82,83,84,87,93,94,95,101,104,113,116,118,129, 
                 133,143,145,146,151,158,176,186,192,198,200,201,
                 206,208,213,215,216,226,231,236,237,238,243, # società civile
                 52,65,74,77,78,99,166,187,189,210,220) # istituzioni

# Le altre sono associazioni di categoria
index_rapp <- setdiff(seq, index_label)
rappr <- associazionismo$text[index_rapp]

rappr <- rappr[53:176]
rappr <- c(rappr, ass_split$denom_estesa)

### acronimi da aggiungere

soc_civile <- c(soc_civile, "aics", "anvcg", "anmli")

rappr <- c(rappr, ass_split$acrn, "acci", "adep", "ariacs", "aesvi", "ebs", "assoege", "agici", "assogot", "angot", "agot",
           "gti", "agta", "isp", "assist", "aib", "aicg", "aicep", "aidaf", "aiget", "aipe", "airu", "amt", "sid", "anap",
           "anbi", "ancrit", "anasf", "anev", "anec", "anet", "anfols", "anfis", "angt", "anpit", "anpe", "anp", "antac",
           "lapet", "assoebios", "conforma", "italiafestival", "atf", "atit", "astra", "assoittica", "assonime",
           "assoprevidenza", "abi", "esco unite", "h2it", "unaitalia")

istituzioni <- c(istituzioni, "anpci", "associazione nazionale piccoli comuni")


# Individuiamo le stringhe che iniziano per asso- e sono composte da una sola parola.
asso <- babele %>% filter(str_detect(text, regex("^asso[^ ]*$"))) %>% pull(text) # 29 - tutte di categoria
rappr <- c(rappr, asso)

# Prendiamo il pattern associazioni

associazioni <- babele %>% filter(str_detect(text, "associazioni")) %>% pull(text)
associazioni_rappr <- associazioni[c(2,3,4,5,6,10,11,13,15)]
associazioni_civile <- associazioni[c(1,7,8,9,12,14,16,17)]

rappr <- c(rappr, associazioni_rappr, "unaapi")
soc_civile <- c(soc_civile, associazioni_civile)


# DIZIONARIO

# Aggiungiamo al dizionario i vettori usati per descrivere le categorie.

dict1 <- dictionary(list(istituzioni=istituzioni,
                        rappr=rappr,
                        soc_civile=soc_civile,
                        centr_ricerca=centr_ricerca))
                        

# TEXT ANALYSIS

# Creating a quanteda corpus
corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict1, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                             sum = istituzioni+rappr+soc_civile+centr_ricerca) %>%
  select(istituzioni, rappr, soc_civile, centr_ricerca, length, sum)
                            
testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# ATTENZIONE - NUOVO CORPUS

# da babele togliamo i pattern esaminati, ricreiamo il corpus e esploriamo la frequenza delle parole più ricorrenti.

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 1311 enti da classificare

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)
dfm_result <- dfm(tok)
textstat_frequency(dfm_result, n=40)

# Nelle prime 10 posizioni, ci sono aggettivi (nazionale, italiana, italiano)
# e parole (italia, federazione, unione, società, consorzio, coordinamento)
# a cui è difficile dare una collocazione netta, come invece accade per
# il pattern "spa" (27 volte) che identifica aziende private.

privati <- "spa"

# Dalla posizione 10 alla 20, vale lo stesso discorso.
# Parole (centro, istituto, consiglio, presidente, movimento, docenti, rete, direttore)
# e l'aggettivo "generale" sono difficili da collocare

# Forse a docenti (16 risultati) possiamo dare l'etichetta di "categoria" ma in presenza
# dell'aggettivo "precari" assume più il carattere di movimento di base.

# Setacciamo il pattern. (18 tot)

docenti <- babele %>% filter(str_detect(text, "docenti")) %>% pull(text) %>% unname() # elimina nomi del vettore

docenti_categ <- docenti[c(1,3,10,11,12,13,14,16)]
docenti_civil <- docenti[c(2,4,5,5,6,6,7,15)]
docenti_civil <- c(docenti_civil, "coordinamento nazionale precari scuola", "movimento dignità docenza universitaria")

# Setacciamo il pattern studenti - istituzioni (10)
studenti <- babele %>% filter(str_detect(text, "studenti")) %>% pull(text) %>% unname() # elimina nomi del vettore

studenti <- studenti[c(1,2,4,6,7)]
studenti <- c(studenti, "cnsu", "issm", "cnsi", "consiglio universitario nazionale", "cun")

# cammini - civile (4)
cammini <- babele %>% filter(str_detect(text, "cammin")) %>% pull(text) %>% unname()

# Altri pattern istituzionali

ist <- c("tribunale", "procuratore", "agenzia italia digitale", "ansf", "agenzia nazionale nuove tecnologie energia sviluppo economico sostenibile")


dict2 <- dictionary(list(privati=privati,
                         istituzioni=c(ist,studenti),
                         soc_civile=c(cammini,docenti_civil),
                         rappr=docenti_categ))


corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict2, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr) %>%
  select(privati, istituzioni, soc_civile, rappr, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# miseri 80

# Terzo attacco alla torre!!!

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 1231 enti da classificare

patt_ist <- c("banca d italia", "proc", "procura")

# Prendiamo il pattern coordinamento
# (non siamo un sindacato nè asso di categoria ma una piattaforma di confronto tra lav, impr e prof...)

coord <- babele %>% filter(str_detect(text, "coordinamento")) %>% pull(text) %>% unname() # 22 + 2

coord_civile <- coord[c(1,4,12,16,17,18,19,20,21)] # conamal, cresco
coord_categ <- coord[c(2,3,5,6,7,8,9,10,13,14,15,22)] # consorzio raee
coord_research <- coord[c(11)]

# Aggiungiamo gli acronimi
coord_civile <- c(coord_civile, "conamal", "cresco")

# I consorzi sono associazioni di imprese. Giuridicamente, sono enti privati senza scopo di lucro.
# Nascono per coordinare e gestire strategie e azioni comuni tra i consorziati.
# Possono rientrare in privati o in categoria.
consorzi <- babele %>% filter(str_detect(text, "consorzi")) %>% pull(text) %>% unname() # 29

# Aggiungiamo gli acronimi
consorzi <- c(consorzi, "cib", "cic", "corepla", "conai", "coripet", "cobat",
              "anbi", "associazione nazionale bonifiche irrigazioni miglioramenti fondiari")

# Liceo - Istituzioni o centri di ricerca? 8
# L'Istat annovera le università tra gli enti pubblici. Seguendo questa logica, anche gli altri istituti statali
# vanno nello stesso insieme, a meno che non decidiamo di fare una scelta metodologica diversa.
liceo <- babele %>% filter(str_detect(text, "liceo")) %>% pull(text) %>% unname() 

# Istituto - 22
istituto <- babele %>% filter(str_detect(text, "istituto")) %>% pull(text) %>% unname() 

ist_categ <- istituto[c(1,2,5,9)] #int,igi,
ist_ricer <- istituto[c(3,4,6,22)] #inu, ispi
ist_civil <- istituto[c(8,17)]
ist_priva <- istituto[c(10,19)] # formazione privata
ist_istit <- istituto[c(11,12,13,14,15,16,18)] #ismea

ist_categ <- c(ist_categ, "int", "igi")
ist_ricer <- c(ist_ricer, "inu", "ispi")
ist_istit <- c(ist_istit, "ismea")

# C'è il tema della formazione privata, come delle università private e telematiche.

# Fanne 131 per abbattere la fortezza dei 1000.

# Movimento - 14
movimento <- babele %>% filter(str_detect(text, "movimento")) %>% pull(text) %>% unname() 

mov_civile <- movimento[c(2,3,4,5,7,9,12,14)]
partiti <- movimento[6]
mov_categ <- movimento[c(1,8,10,11,13)]

# Accademia - 3
accademia <- babele %>% filter(str_detect(text, "accademia")) %>% pull(text) %>% unname()
accad_civ <- accademia[c(2,3)]
accad_res <- accademia[1]

# Consiglio - 14
consiglio <- babele %>% filter(str_detect(text, "consiglio")) %>% pull(text) %>% unname()

consiglio_cat <- consiglio[c(6,8,10,12)] # cncc
consiglio_ist <- consiglio[c(1,2,3,5,7,9,11,14)]

consiglio_cat <- c(consiglio_cat, "cncc")

# 100

# Federazion - 35

federazion <- babele %>% filter(str_detect(text, "federazion")) %>% pull(text) %>% unname()

federazion_civil <- federazion[c(4,10,27)] # fedea
federazion_categ <- federazion[c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,28,29,30,31,32,33,34,35)]
federazion_ricer <- federazion[25]

federazion_civil <- c(federazion_civil, "fedea")
federazion_categ <- c(federazion_categ, "fapav", "fnsi", "federvivo", "fmi", "federmoto", "fipsas", "confguide")


# Pattern feder - togliamo i risultati di federazion
feder <- babele %>% filter(str_detect(text, "feder")) %>% pull(text) %>% unname()
feder <- setdiff(feder, federazion) #31

feder_civil <- feder[c(3,13,16)]
feder_categ <- setdiff(feder, feder_civil)


dict3 <- dictionary(list(privati=ist_priva,
                         istituzioni=c(patt_ist,liceo,ist_istit,consiglio_ist),
                         soc_civile=c(coord_civile,ist_civil,mov_civile,accad_civ,federazion_civil,feder_civil),
                         rappr=c(coord_categ,coord_research,consorzi,ist_categ,mov_categ,consiglio_cat,federazion_categ,feder_categ),
                         centr_ricerca=c(ist_ricer,accad_res,federazion_ricer),
                         altro=partiti))


corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict3, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 198 colpiti

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 1033 enti da classificare - resiste!

# Unione - pattern categoria eccetto udi e trifulau
unione <- babele %>% filter(str_detect(text, "unione")) %>% pull(text) %>% unname()
unione <- unione[-c(7,23)] # 29
unione <- c(unione, "unirima", "uniem")

# il pattern "donne" raccoglie enti della società civile + acrn udi
uni_donne <- c("donne", "udi", "unione piemontese trifulau")

### ABBATTUTO ###

# società italiana - tra accademia e rappresentanza. per differenziarle dai sindacati scelgo "centri di ricerca"

società_ita <- babele %>% filter(str_detect(text, "società italiana")) %>% pull(text) %>% unname() # 17

soc_ita_rapp <- c("società italiana autori editori", "siae", "società italiana mediatori familiari", "simef")
soc_ita_ricer <- setdiff(società_ita, soc_ita_rapp)
soc_ita_ricer <- c(soc_ita_ricer, "siped", "simg")

# società

società <- babele %>% filter(str_detect(text, "società")) %>% pull(text) %>% unname()
società <- setdiff(società, società_ita)

società_civil <- c("vas", "verdi ambiente società")
società_priv <- società[c(4,8)]
società_ricer <- c("società chimica italiana", "società botanica italiana", "società internazionale biourbanistica",
                   "società geologica italiana")
società_rapp <- "consulta società scientifiche"

# Centro - centro studi (think tank, centri di ricerca, filtrati per natura e funzioni), centri cinofili

centro <- babele %>% filter(str_detect(text, "centro")) %>% pull(text) %>% unname() # 23

centro_civil <- centro[c(1,3,4,7,22)] # csi
centro_ricer <- centro[c(6,8,9,13,14,16,18,20)]
centro_priv <- centro[c(10,11,12,15)] # cesi
centro_rapp <- c("centro italiano alluminio", "centroal")
centro_ist <- centro[21]

centro_civil <- c(centro_civil, "csi")
centro_ricer <- c(centro_ricer, "centro studi internazionali")
centro_priv <- c(centro_priv, "cesi")

# Rete (15)
# Sembra associabile prevalentemente alla società civile, legato al tema dei diritti civili;
# ma può agire come network di rappresentanza professionale, quando promuove un settore.

rete <- babele %>% filter(str_detect(text, "rete")) %>% pull(text) %>% unname()

rete_priv <- rete[1]
rete_civil <- rete[c(2,3,4,5,6,8,11,15)]
rete_categ <- rete[c(7,10,12,13,14)]
rete_ist <- rete[9]

# italia, presidente, direttore, commissione, studi, osservatorio, gruppo, lega, conferenza, imprese,
# mercato, fiori, garante, direzione, srl, alleanza, pro, club, medici, ente, italy, ambiente, sicurezza

# nazionale, italiana, italiano, generale, italiani, internazionale, pubblici, italian, european,


# Osservatorio

osservatori <- babele %>% filter(str_detect(text, "osservatori")) %>% pull(text) %>% unname() # 11

osserv_ricer <- osservatori[c(4,9,10,11)] # omar, italiadecide
osserv_civil <- osservatori[2] # osservatorio nazionale bullismo disagio giovanile
osserv_categ <- c("ora ncc", "osservatorio regionale autoservizi")
osserv_istit <- c("osservatorio asse ferroviario torino lione", "osservatorio nazionale infrastrutture")

osserv_ricer <- c(osserv_ricer, "omar", "italiadecide")
osserv_civil <- c(osserv_civil, "osservatorio nazionale bullismo disagio giovanile")

# Commissione

commissione <- babele %>% filter(str_detect(text, "commissione")) %>% pull(text) %>% unname() # 10

comm_istit <- commissione[c(2,4,6,7,8,9)]
comm_categ <- commissione[10]

comm_istit <- c(comm_istit, "ciim", "uninfo")

# studi (fa contesto con centro - centro studi)

# Gruppo
gruppo <- babele %>% filter(str_detect(text, "gruppo")) %>% pull(text) %>% unname() # 9

gruppo_categ <- gruppo[c(1,2)]
gruppo_priv <- gruppo[c(3,4,6,7,8)]
gruppo_ricer <- gruppo[c(5,9)]

gruppo_priv <- c(gruppo_priv, "unipol")
gruppo_ricer <- c(gruppo_ricer, "#vita")


dict4 <- dictionary(list(privati=c(società_priv,centro_priv,rete_priv,gruppo_priv),
                         istituzioni=c(centro_ist,rete_ist,osserv_istit,comm_istit),
                         soc_civile=c(uni_donne,società_civil,centro_civil,rete_civil,osserv_civil),
                         rappr=c(unione,soc_ita_rapp,soc_ita_rapp,società_rapp,centro_rapp,rete_categ,osserv_categ,comm_categ,gruppo_categ),
                         centr_ricerca=c(soc_ita_ricer,società_ricer,centro_ricer,osserv_ricer,gruppo_ricer)
                         ))

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict4, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 139 corrispondenze!

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 894 enti da classificare - siamo dentro!


# Italia

italia <- babele %>% filter(str_detect(text, "italia")) %>% pull(text) %>% unname() # 98

italian <- babele %>% filter(str_detect(text, "italian")) %>% pull(text) %>% unname() # 32
italia <- setdiff(italia, italian) # 66


italia_civil <- italia[c(2,4,25,27,28,30,43,44,53,58,60,61,66)] # italia solare, medici senza frontiere, green building council italia
italia_ist <- italia[c(36)] # italia digitale
chiesa <- italia[c(6,7)]
italia_priv <- italia[c(5,9,11,13,15,17,29,31,32,34,35,37,38,41,46,47,48,51,52,55,57,59)] # forte componente media/tech
italia_categ <- italia[c(12,14,16,22,23,39,40,42,49,54,56,62,65)] # wgi, assodanza
partiti <- italia[c(18,19,20,21)] 

italia_civil <- c(italia_civil, "italia solare", "medici senza frontiere", "green building council italia")
italia_ist <- c(italia_ist, "italia digitale")
italia_categ <- c(italia_categ, "wgi", "assodanza")



italian_civil <- italian[c(1,3,16,22,31)] #capitolo italiano, creative commons, raai, registro attori attrici italiani, equo garantito
italian_categ <- italian[c(5,7,11,14,15,18,23,29,32)] #segretariato italiano giovani medici, sigm, ibar
italian_priv <- italian[17] # nestlé, oti
italian_ricer <- italian[c(10,28)] # formazione, limes
italian_part_stat <- c("borsa merci telematica italiana", "bmti") # 24
italian_ist <- c("ente italiano normazione", "uni", "ente nazionale cinofilia italiana", "enci") # 25,26

italian_civil <- c(italian_civil, "capitolo italiano", "creative commons", "raai", "registro attori attrici italiani", "equo garantito")                 
italian_categ <- c(italian_categ, "segretariato italiano giovani medici", "sigm", "ibar")
italian_priv <- c(italian_priv, "nestlé", "oti")
italian_ricer <- c(italian_ricer, "limes")


dict5 <- dictionary(list(privati=c(italia_priv, italian_priv),
                         istituzioni=c(italia_ist, italian_ist),
                         soc_civile=c(italia_civil, italian_civil),
                         rappr=c(italia_categ, italian_categ),
                         centr_ricerca=c(italian_ricer),
                         part_statali=italian_part_stat,
                         altro=c(chiesa, partiti)))



corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict5, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+part_statali+altro) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, part_statali, altro, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 100 colpi!

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 794 enti da classificare - obiettivo 500!

# Sostantivi presidente, direttore, conferenza, mercato, fiori
# Aggettivi nazionale, generale

# mercato fiori - mercati all'ingrosso che funzionano come strutture pubbliche
# aziende speciali comunali o consorzi pubblici

mercato_fiori <- "mercato fiori" # 8 ist

# Conferenza - tutte istituzioni
conferenza <- babele %>% filter(str_detect(text, "conferenza")) %>% pull(text) %>% unname() # 8

conferenza_ist <- conferenza[c(2,6)]
conferenza_ist <- c(conferenza_ist, "cunsf", "conferenza universitaria nazionale scienze formazione")

# Direttore
direttore <- babele %>% filter(str_detect(text, "direttore")) %>% pull(text) %>% unname() # 8

direttore_ricer <- "osservatorio sicurezza internazionale"
direttore_ist <- c("uama", "dipartimento finanze", "grande progetto pompei", "mibac", "mise")
direttore_priv <- c("euromedia research")

# Presidente
presidente <- babele %>% filter(str_detect(text, "presidente")) %>% pull(text) %>% unname() # 9

pres_ist <- c("adsp", "commissione tecnica tranvie")  #  autorità di sistema portuale
pres_civil <- c("edge", "acbs", "emergency", "aero club marina massa")
pres_ricer <- "astril"


# Generale - tutte istituzioni
# I pattern direttore generale e direzione generale sono spesso associabili a istituzioni.

generale <- babele %>% filter(str_detect(text, "generale")) %>% pull(text) %>% unname() # 11

generale_ist <- generale[10]
generale_ist <- c(generale_ist, "ragioneria generale stato", "mit", "motorizzazione civile",
                  "divisione aerea", "mise", "mibac", "grande progetto pompei")

srl_priv <- "srl"


# Nazionale

nazionale <- babele %>% filter(str_detect(text, "nazionale")) %>% pull(text) %>% unname() # 30

# "garante" sembra essere associata a istituzioni. soprattutto con "autorità".

nazionale_ist <- nazionale[c(3,11,19,30)] # vigili fuoco
nazionale_civil <- nazionale[c(1,4,7,9,13,16,21,25)] # avis
nazionale_categ <- nazionale[c(8,10,12,14,20,24,29)]
nazionale_ricer <- nazionale[c(27,28)]

nazionale_ist <- c(nazionale_ist, "vigili fuoco", "ente nazionale cinofilia italiana", "enci")
nazionale_civil <- c(nazionale_civil, "avis")


dict6 <- dictionary(list(privati=c(direttore_priv, srl_priv),
                         istituzioni=c(mercato_fiori, conferenza_ist, direttore_ist, pres_ist, generale_ist, nazionale_ist),
                         soc_civile=c(pres_civil, nazionale_civil),
                         rappr=nazionale_categ,
                         centr_ricerca=c(direttore_ricer, pres_ricer, nazionale_ricer)))

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict6, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 69 colpiti!

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 725 enti da classificare - obiettivo 500!

# Proviamo ad elaborare una strategia.

# 1. Esplora textstats per individuare i pattern ancora presenti.
# 2. Conta il numero di stringhe composte da una sola parola. Si tratta di acronimi per la maggioranza.
# 3. Incrocia il dizionario dei nomi propri per individuare le stringhe contenenti nomi di persona.

# 1.

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)
dfm_result <- dfm(tok)
textstat_frequency(dfm_result, n=40)

# Pattern individuati:
# &, garante, european, italy, fise, distretto, alleanza, lega, network, system, ingegner, partners, cooperative,
# direzione, nazionale, rivista, liberi, autori, pro, infanzia, group, filiera, immobiliare, conferenza, cultura, museo.

et <- babele %>% filter(str_detect(text, "&")) %>% pull(text) %>% unname() # 7

et_priv <- et[c(1,3,4,6,7)]
et_civil <- et[5]

et_civil <- c(et_civil, "pro vita & famiglia", "pro vita famiglia")


# garante pattern istituzioni
garante <- babele %>% filter(str_detect(text, "garante")) %>% pull(text) %>% unname() # 7
garante[4] <- "garante privacy dati personali"


european <- babele %>% filter(str_detect(text, "european")) %>% pull(text) %>% unname() # 6

european_civil <- european[1]
european_categ <- european[c(2,3,4,6)]
european_ist <- european[5]


italy <- babele %>% filter(str_detect(text, "italy")) %>% pull(text) %>% unname() # 7

italy_categ <- italy[c(2,3,4)]
italy_priv <- italy[c(5,6,7)]
italy_categ <- c(italy_categ, "american chamber of commerce italy")


# fise - federazione italiana sport equestri (in categoria)
fise <- c("fise", "unicircular")


# distretto - natura privata consortile (in categoria)
distretto <- babele %>% filter(str_detect(text, "distretto")) %>% pull(text) %>% unname() # 5

# alleanza
alleanza <- babele %>% filter(str_detect(text, "alleanza")) %>% pull(text) %>% unname() # 4

alleanza_civil <- "alleanza infanzia"
alleanza_categ <- alleanza[c(1,2,3)]

# lega
lega <- babele %>% filter(str_detect(text, "lega")) %>% pull(text) %>% unname() # 7

lega_civil <- lega[2]
partiti <- "lega"
lega_categ <- lega[c(5,7)]

# stringhe in cui lega è parte della parola
lega_priv <- "studio legale"
lega_categ <- c(lega_categ, "ordine revisori legali", "delegazione diplomate ruolo riserva")

# network (2)
network_civil <- "rebel network"
network_categ <- "network place innovazione biomedica"

# system - civile
system <- "danza error system"

# ingegner - esperti
ingegner <- "ingegner"

# partners - privati
partners <- "comin § partners"

# direzione
direzione_ist <- c("direzione centrale anticrimine", "mite") # ministero transizione ecologica

# nazionale
nazionale_categ <- c("cluster nazionale chimica verde", "spring", "partite iva nazionali")
nazionale_ist <- "istituto nazionale urbanistica"

# rivista - centri di ricerca, think tank, media
rivista <- "rivista"

# liberi
liberi <- babele %>% filter(str_detect(text, "liberi")) %>% pull(text) %>% unname() # 3
liberi_civil <- liberi[c(1,2)]
partiti <- c(partiti, "liberi uguali")

# autori
autori_categ <- "100 autori"

# pro
pro_civil <- "lega pro animale"

# group - privati
group <- babele %>% filter(str_detect(text, "group")) %>% pull(text) %>% unname() # 3
group_priv <- group

# filiera
filiera_priv <- c("re mind", "remind")

# immobiliari
immobiliari_ricer <- "scenari immobiliari"

# cultura
cultura_part <- "zetema progetto cultura"
chiesa <- "cultura cattolica"
cultura_categ <- "assni cultura cinematografica"

# museo - alcuni in istituzioni perché gestiti dal ministero, altri in ets perché gestiti da fondazioni
museo <- babele %>% filter(str_detect(text, "museo")) %>% pull(text) %>% unname() # 3

museo_ist <- museo[1]
museo_civil <- museo[c(2,3)]

# Dizionario 7

dict7 <- dictionary(list(privati=c(et_priv, italy_priv, partners, group, filiera_priv),
                         istituzioni=c(garante, european_ist, direzione_ist, nazionale_ist, museo_ist),
                         soc_civile=c(et_civil, european_civil, alleanza_civil, lega_civil, network_civil, system, liberi_civil, pro_civil, museo_civil),
                         rappr=c(european_categ, italy_categ, fise, distretto, alleanza_categ, lega_categ, network_categ, nazionale_categ, autori_categ, cultura_categ),
                         centr_ricerca=c(rivista, immobiliari_ricer),
                         altro=c(partiti, chiesa),
                         esperti=ingegner,
                         part_statali=cultura_part))

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict7, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 84 frecce! 

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 641 enti da classificare - obiettivo 500!

# 2.

library(stringr)

babele %>%
  summarise(
    single_word = sum(!str_detect(text, " "))
  ) # 363 soggetti

# 3.

library(rvest)

link <- "https://www.nomix.it/nomi-italiani-maschili-e-femminili.php"
page <- read_html(link)

nomi <- vector(mode = "list")

letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "L", "M", "NO", "PQ", "R", "S", "TUV", "WZ")

for (i in letters) {
  link <- paste("https://www.nomix.it/nomi-italiani-lettera-",i,".php", sep = "")
  page <- read_html(link)
  x <- page %>% html_nodes("td") %>% html_text()
  nomi[[i]] <- x
  print(paste("Page:", i))
  Sys.sleep(5)
}

proper_names <- vector(mode="list", length=17)
proper_names[1:17] <- nomi

nomi_propri <- unlist(proper_names)
nomi_propri <- str_to_lower(nomi_propri)
nomi_propri <- sort(nomi_propri)
nomi_propri <- nomi_propri[-(1:14)]
nomi_propri <- unique(nomi_propri)

dict_names <- dictionary(list(names = nomi_propri))

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict_names, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result)) %>%
  select(length, names)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 73 corrispondenze. alcuni nomi sono accompagnati dal titolo o ruolo professionale, o dall'ente che rappresentano.
# classifichiamo le info aggiuntive al nome - cognome.

civile <- c("partecipante concorso", "article 19", "terzo settore", "ora quando libere", "cismai", # se non ora quando libere
            "telefono rosa", "maison antigone", "icaro", "cadec", "inclusione donna", "ass alice", "ada") # ass diritti anziani

esperti <- c("dssa", "giornalista", "inviato speciale")

istituzioni <- c("questore", "pres", "on", "ten") # on: onorevole, pres è neutro in generale

media <- c("il mattino", "eurispes") # il mattino

chiesa <- c("upci", "sma", "don", "suor", "chiesa cristiana universale nuova gerusalemme")

partecipate <- "pedemontana veneta"

privati <- c("google", "ilva", "lorenzo baldrighi artists management")

categoria <- c("cogita", "alleanza fotovoltaico italia")


dict8 <- dictionary(list(privati=privati,
                         istituzioni=istituzioni,
                         soc_civile=civile,
                         rappr=categoria,
                         centr_ricerca=media,
                         altro=chiesa,
                         esperti=esperti,
                         part_statali=partecipate))


# Come facciamo ad identificare i soggetti senza ulteriori informazioni? Stringa tipo: nome cognome.

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict8, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 38 risultati!

# 73 - 38 =  35 nome cognome. come individuarli?


babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 603 enti da classificare - obiettivo 500!


babele %>%
  summarise(
    n_una_parola = sum(str_count(text, "\\S+") == 2)
  )

babel_mult_word <- babele %>% filter(str_count(text, "\\S+") > 2) %>%
  pull(text) %>% unname()

rappr <- babel_mult_word[c(1,2,3,12,22,24,27,60,65,66,67,80,86)] # keepon live, egualia, gat, agilo, pv cycle, eco pv
privat <- babel_mult_word[c(4,46,47,55,57,59,61,62,63,64.68,70,77,79,83,84)] # rcs, sio, innova, elemens, gme
esperti <- babel_mult_word[c(5,35,36,37,40,50,88)]
civile <- babel_mult_word[c(8,16,28,29,30,34,45,52,58,71,85,90,91,93,94)] # ente promozione sportiva, asd, isde
media <- babel_mult_word[c(17,78)]
istituzioni <- babel_mult_word[c(15,19,33,41,42,43,51,87,92)]
partecipate <- babel_mult_word[c(54)]
think_tank <- babel_mult_word[c(56,75)]
centro_ricerca <- babel_mult_word[c(69,95)]
formazione <- babel_mult_word[89]

rappr <- c(rappr, "keepon live", "egualia", "agilo", "pv cycle", "eco pv")
privat <- c(privat, "rcs", "sio", "innova", "elemens", "gme")
civile <- c(civile, "ente promozione sportiva", "asd", "isde")





babel_two_word <- babele %>% filter(str_count(text, "\\S+") == 2) %>%
  pull(text) %>% unname()

esperti <- c(esperti, babel_two_word[c(1,2,9,10,11,15,16,17,18,20,27,28,29,30,31,32,33,35,36,39,41,42,45,
                                       46,47,64,76,78,80,82,91,123,124,125,128,137)])

privat <- c(privat, babel_two_word[c(3,6,61,66,83,95,97,107,108,110,111,115,116,117,126,134,141,146,147)])

istituzioni <- c(istituzioni, babel_two_word[c(8,51,57,72,93,127)])
centro_ricerca <- c(centro_ricerca, babel_two_word[c(99,129)], "garr", "sigo") # garr, sigo

civile <- c(civile, babel_two_word[c(4,13,14,37,44,68,71,77,79,81,87,88,89,98,101,113,130,131)],
            "giffoni film festival", "uniamo", "consult@noi")
# giffoni film festival, uniamo, consult@noi

rappr <- c(rappr, babel_two_word[c(7,12,19,49,52,54,55,56,62,84,85,96,100,102,103,106,112,114,120,
                                   121,142,143,144)],
           "cup", "rpt", "unionsoa", "generalsoa", "associazione a dj", "anbba", "aogoi")
# cup (comitato unico professioni), rpt, unionsoa, generalsoa, associazione a dj, anbba, aogoi

media <- c(media, c(babel_two_word[c(26,65,90,92,94,104,105,132)]))
religione <- c("padre occhetta", "assemblea rabbini")
think_tank <- c(think_tank, babel_two_word[c(140)])
partiti <- c("misto", "azione")
partecipate <- c(partecipate, babel_two_word[86])


dict9 <- dictionary(list(privati=privat,
                         istituzioni=istituzioni,
                         soc_civile=civile,
                         rappr=rappr,
                         centr_ricerca=c(centro_ricerca, media, think_tank, formazione),
                         altro=c(religione, partiti),
                         esperti=esperti,
                         part_statali=partecipate))

corpus <- corpus(babele, text_field = "text")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dict9, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto

# 192 affondi!

babele <- result %>% filter(sum==0) %>% select(text) %>% unique() # 399 enti da classificare - nuovo obiettivo 200!

# Uniamo i singoli volumi del dizionario.
dizionario <- c(dict1, dict2, dict3, dict4, dict5, dict6, dict7, dict8, dict9)

saveRDS(dizionario, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
