#########################################
#              DIZIONARIO               #
# Sindacati e Associazioni di Categoria #
#########################################

# Loading libraries
library(rvest)
library(tidyverse)

senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")

# Qual è la differenza tra sindacati e associazioni di categoria?

# Non esiste il registro delle suddette categorie. Cerchiamo di ricostruire l'ecosistema.

# In primo luogo, appuntiamo i pattern "sindacat*" e "sindacal*",

# In Italia esistono quattro principali confederazioni sindacali:
# la CGIL (Confederazione generale italiana del lavoro);
# la CISL (Confederazione italiana sindacati lavoratori);
# la UIL (Unione italiana del lavoro);
# l’UGL (Unione generale del lavoro).

# Riportiamo un'accezione del termine "confederazione" dall'enciclopedia Treccani:
# «unione nazionale o internazionale di federazioni o di associazioni appartenenti
# a diverse categorie professionali, che costituisce la massima espressione
# dell’organizzazione sindacale orizzontale»

# Le confederazioni si articolano nelle federazioni di categoria, che organizzano
# gli iscritti sia per settore economico sia per area territoriale.

# Per rilevare le federazioni di categoria di CIGL e CISL bastano le sigle delle
# confederazioni, in quanto sono nomi composti. Vediamo due esempi:
# FIOM CGIL, Impiegati e Operai Metallurgici;
# FAI CISL, e così tutti gli altri.

# Per la UIL, è necessario aggiungere le sigle delle federazioni di categoria
# dove il pattern UIL è completato da altre lettere che specificano il settore
# di produzione.

link <- "https://www.uil.it/gruppodirigente.asp"
uil <- read_html(link) %>% html_elements("#footerSezioniBodyArea1 a") %>% html_text2()

# Rimuoviamo lo spazio dalle sigle e aggiungiamo UIL per catturare entrambe le varianti:
# es. UIL PA e UILPA.

uil <- gsub(" ", "", uil)
uil <- c("uil", uil)
uil[13] <- "uil oo.cc."

# Per UGL, basta la sigla.

org_rappr <- c("sindacat*", "sindacal*", "cgil", "cisl", uil, "ugl")

# Un cenno all'orientamento politico:
# CGIL: sinistra;
# CISL: democristiano, repubblicano, cattolico;
# UIL: socialdemocratico, repubblicano, liberale;
# UGL: destra.


# Integriamo la lista con le organizzazioni sindacali aderenti al T.U. sulla rappresentanza
# sindacale Confindustria - CIGL, CISL, UIL (mappate dall'Inps) e con altre organizzazioni
# presenti in liste di organizzazioni sindacali sul web (Wikipedia).

# Disclaimer: riportiamo solo le organizzazioni presenti nei nostri dati da classificare
# (previa verifica manuale nel dataset durante la ricerca sul web).

# Le organizzazioni individuate sono:
# cisal, cobas, confsal, cub, unci, usb, gilda, sivelp, sulpl, cida, anpav, anpac,
# assindatcolf, nursind, cse, flp, assovolo, assolavoro

# Scopriamo la natura delle organizzazioni appena individuate.

# La lista elenca confederazioni, federazioni e associazioni, sindacati che danno vita o
# fanno parte di vari network della rappresentanza categoriale. Esploriamo questi ecosistemi
# per verificare la presenza di ulteriori organizzazioni tra gli stakeholders da classificare.

# È un'occasione per allungare la lista: ricostruiamo il network delle confederazioni
# aggiungendo federazioni, associazioni, sindacati aderenti.

# Disclaimer: se presenti nei dati da classificare e non rilevabili con la sigla della confederazione.

# Ad esempio:
# "cub flaica" non verrà aggiunto in quanto rilevabile con la sigla "cub";
# "snals-confsal" verrà rilevato con la sigla "confsal".
# Se "flaica" e "snals" non fossero accompagnati dalla sigla della confederazione
# tra i nomi da classificare, verrebbero aggiunti.
# A volte, è presente il nome esteso al posto dell'acronimo.

# Let's go

cisal <- c("cisal", "anief", "sinappe", "federazione italiana autonoma lavoratori dello spettacolo")

# federagenti (Federazione Nazionale Agenti, Rappresentanti e Intermediari del Commercio) di cisal;
# federagenti (Federazione Nazionale Agenti Raccomandatari Marittimi e Mediatori Marittimi) di confetra;


confsal <- c("confsal", "fials", "sappe", "anpa", "confederazione italiana liberi agricoltori", "uci",
             "unione coltivatori italiani", "associazione italiana coltivatori", "fesica")

# fials (sanità) di confsal; fials (federazione italiana autonoma lavoratori dello spettacolo) di cisal;
# aic ("associazione italiana coltivatori"); aic ("associazione italiana celiachia");
# aic ("associazione italiana calciatori"); "aic" (associazione italiana compostaggio)


cida <- c("cida", "federmanager")

# Sivelp aderisce a Federprofessioni.
# Assindatcolf aderisce al sistema Confedilizia.
# Gilda, Nursind e Flp aderiscono a CGS - confederazione generale sindacale.
# Assolavoro aderisce al sistema Confindustria.

cse <- c("cse", "confederazione indipendente sindacati europei")

# cisal, confsal, cida, cse, cobas, cub, unci, usb, sulpl, anpav, anpac, assovolo


# Aggiorniamo il vettore organizzazioni di rappresentanza
org_rappr <- c(org_rappr, cisal, confsal, cida, cse, "cobas", "cub", "unci", "usb", "sulpl", "anpav", "anpac", "assovolo")


# Ricordiamoci Federprofessioni, Confedilizia, CGS, Confindustria

# La galassia Confindustria è stata esplorata in uno script a parte.

federprofessioni <- c("federprofessioni", "sivelp")

confedilizia <- c("confedilizia", "adsi", "ania", "aspesi", "assindatcolf", "fiaip", "fidaldo")

cgs <- c("cgs", "gilda", "fgu", "flp", "nursind")

confindustria <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/confindustria/confind_list.RData")
confindustria <- unlist(confindustria) %>% unique()

# Aggiorniamo il vettore
org_rappr <- c(org_rappr, federprofessioni, confedilizia, cgs, confindustria)


# Quali sono le organizzazioni di rappresentanza più influenti in Italia?
# [fonte: https://www.ferpi.it/news/quali-sono-le-associazioni-di-categoria-piu-influenti-in-italia]

# Associazione degli industriali: Confindustria
# Associazioni delle piccole e medie imprese: Confartigianato, Confapi, Cna, Cgia
# Associazioni dei commercianti: Confcommercio, Confesercenti
# Associazioni del comparto agricolo: Coldiretti, Confagricoltura, Cia
# Associazioni delle cooperative: Confcooperative, Legacoop


# Confindustria già trattata.

# Confartigianato, Confapi, CNA, CGIA, Coldiretti, non hanno associazioni di categoria
# da registrare perché non presenti nei nostri dati. Salviamo solo i nomi delle confederazioni.


# Galassia Confcommercio - Associazioni di categoria

link <- "https://www.confcommercio.it/associazioni"
acrnm <- read_html(link) %>% html_elements(".title-associazione") %>% html_text2()
categ <- read_html(link) %>% html_elements(".category-associazione") %>% html_text2()
descr <- read_html(link) %>% html_elements(".descrizione-associazione") %>% html_text2()

df_confcommercio <- data.frame(acronimo = acrnm,
                               categoria = categ,
                               descrizione = descr)
df_confcommercio[10,2] <- "Associazioni nazionali di categoria"
df_confcommercio[95,2] <- "Associazioni nazionali di categoria"
df_confcommercio[101,2] <- "Associazioni nazionali di categoria"

# Acronimi delle associazioni di categoria - Confcommercio

acrnm <- df_confcommercio %>% filter(categoria=="Associazioni nazionali di categoria") %>% pull(acronimo)
acrnm <- str_to_lower(acrnm)
acrnm <- gsub("[\\.'’]", "", acrnm)
acrnm <- gsub("-", " ", acrnm)
acrnm <- gsub("confcommercio imprese per litalia", "", acrnm)
acrnm <- gsub("–", " ", acrnm)
acrnm <- gsub("\\s+", " ", acrnm)
acrnm <- str_trim(acrnm, side="both")

# Costruire espressioni regolari per cercare parole esatte
patterns <- paste0("\\b", acrnm, "\\b")

# Creare un vettore logico per ogni acronimo
match_logico <- sapply(patterns, function(i) grepl(i, senato$NOMI))

# Estrarre solo gli acronimi che appaiono nei dati come parole a sé stanti o come match esatti.
acrnm_matched <- acrnm[apply(match_logico, 2, any)] # 41

confcommercio <- acrnm_matched[-6] # falso positivo

# Nomi estesi delle associazioni di categoria - Confcommercio

descr <- df_confcommercio %>% filter(categoria=="Associazioni nazionali di categoria") %>% pull(descrizione)

# Pulizia segni di punteggiatura + stopwords
descr <- gsub("Confcommercio-Imprese per l'Italia", "", descr)
descr <- gsub("Affiliato", "", descr)
descr <- gsub("[-,\\'()]", " ", descr)
descr <- gsub("\\.", "", descr)
descr <- gsub("\\s+", " ", descr) %>% trimws()
descr <- str_to_lower(descr)

# L'incrocio, post rimozione stopwords dal vettore e dai dati, ha generato i seguenti risultati:

confcommercio <- c(confcommercio, "confcommercio", "federazione italiana tabaccai",
                   "associazione italiana pellicceria", "associazione distributori farmaceutici")


confesercenti <- c("confesercenti", "assoturismo", "fiba","federagit", "federnoleggio", "assoviaggi",
                   "anama", "assoartisti", "faib", "sil", "assopanificatori", "aisad")

confagricoltura <- c("confagricoltura", "api", "associazione piscicoltori italiani", "associazione italiana pellicceria",
                     "assoverde", "federazione apicoltori italiani", "uncai")

# fai ("federazione apicoltori italiani") di confagricoltura; fai (federazioni autotrasportatori italiani) [web]; fai (fondo ambiente italiano)
# "fondo ambiente italiano" è una fondazione, quindi non in questa categoria.

cia <- c("cia", "aiel", "associazione florovivaisti italiani")

confcooperative <- c("confcooperative", "federcasse", "fedagri", "fedagripesca")

legacoop <- c("legacoop", "ancc coop", "ancd conasd", "culturmedia")

org_rappr <- c(org_rappr, "confartigianato", "confapi", "cna", "cgia", "coldiretti",
               confcommercio, confesercenti, confagricoltura, cia, confcooperative, legacoop)

org_rappr <- unique(org_rappr) #6

# PARTE 2


# L'idea è di cercare i pattern "confederazione" e "conf-" (forma abbreviata)
# nei nostri dati per individuare ulteriori organizzazioni.

senato %>% filter(grepl("confederazione", NOMI)) %>% select(NOMI) %>% unique()

# Salviamo il pattern "confederazione" e gli acronimi delle confederazioni audite al Senato
# non rilevabili con il pattern.

pattern1 <- c("confederazione", "cgdp", "aepi", "cna", "cida", "cgs", "confasi",
              "confarca", "confimi", "cgbi", "confsal", "confetra", "cosmed",
              "cse", "confapi", "cub")

# mispelling "legacoop": "lega coop"

# Setacciamo i risultati
pattern1 <- setdiff(pattern1, org_rappr)


aepi <- c("aepi", "ancot", "associazione nazionale consulenti tributari", "federazione italiana cuochi", "associazione ncc italia")

confasi <- c("confasi", "ciu unionquadri", "asal")

confimi <- c("confimi", "assorimap", "finco", "assohoreca") # Confederazione dell’Industria Manifatturiera Italiana e dell’Impresa Privata

confetra <- c("confetra", "fedit", "assologistica", "aicai", "anama", "assohandlers", "fercargo", 
              "trasporto unito", "trasportunito")

cosmed <- c("cosmed", "anaao assomed", "fedir", "sivemp")


# Aggiorniamo il vettore
org_rappr <- c(org_rappr, "confederazione", "cgdp", aepi, confasi, "confarca", confimi, "cgbi", confetra, cosmed)

org_rappr <- unique(org_rappr) #3

# PARTE 3


# Cerchiamo il pattern "conf-" assicurandoci di escludere i falsi positivi.

conf <- senato %>% filter(grepl("conf", NOMI)) %>% select(NOMI) %>% unique()

# Nomi falsi positivi: conferenza, confederazione delle regioni e delle province autonome,
#                      sconfinata, conforma

name_to_exclude <- c("conferenza", "confederazione delle regioni e delle province autonome",
                     "sconfinata", "conforma", "confad")

conf <- conf %>%
  filter(!grepl(paste(name_to_exclude, collapse = "|"), NOMI))

# Rimuoviamo dalla ricerca "conf-" le associazioni già individuate
# (sia dal web che con il pattern "confederazione" nei dati).

conf <- conf %>%
  filter(!grepl(paste(org_rappr, collapse = "|"), NOMI))

# Setacciamo i risultati.

# Annotiamo 5 mispelling: riguardano confindustria, confartigianato, confagricoltura, confesercenti


# Galassia Confprofessioni - Associazioni professionali affiliate

link <- "https://confprofessioni.eu/chi-siamo/associazioni/?_gl=1*iwz2x4*_up*MQ..*_ga*Mzc0MzA5NTg5LjE3Mzk2MjM0NDg.*_ga_H0JT461JLS*MTczOTYyMzQ0Ny4xLjAuMTczOTYyMzQ0Ny4wLjAuMA..*_ga_1LEPDHHR94*MTczOTYyMzQ0Ny4xLjAuMTczOTYyMzQ0Ny4wLjAuMA.."
acrnm <- read_html(link) %>% html_elements(".box-text-inner") %>% html_text2()
descr <- c("associazione nazionale consulenti del lavoro", "associazione nazionale commercialisti",
           "associazione dei dottori commercialisti e degli esperti contabili", "associazione nazionale archeologi")

confprofessioni <- acrnm[c(2,3,8,13,15,16,17)]
confprofessioni <- c(confprofessioni, descr, "confprofessioni")


conflavoro <- c("conflavoro", "maavi")

confservizi <- c("confservizi", "asstra", "utilitalia")

# "confimprese italia" diverso da "confimprese"
confimprese_italia <- c("confimprese italia", "asci", "confimprese demaniali")
# Ancimp aderisce ad ANIE - Confindustria. Tra le organizzazioni federate di confimprese_italia.
# mispelling "confimprese italia": "confimpreseitalia"

# confcultura è l'alias di confindustria cultura italia
confindustria <- c(confindustria, "confcultura")

# "confturismo" è l’espressione unitaria delle organizzazioni nazionali rappresentative delle imprese e
# delle professioni turistiche aderenti a Confcommercio.
confturismo <- c("confturismo", "anbba", "faita", "federalberghi", "fiavet", "fipe", "rescasa")
fipe <- c("fipe", "sib", "sindacato italiano balneari", "silb")
confcommercio <- c(confcommercio, confturismo, fipe)
# mispelling "silb": "silbfipe"

org_rappr <- c(org_rappr, confprofessioni, conflavoro, confservizi, "confabitare", confimprese_italia,
               "confcultura", confturismo, fipe, "confconsumatori")

org_rappr <- unique(org_rappr) #6





# Reti di rappresentanza.

# "alleanza delle cooperative italiane": (Legacoop, Confcooperative, Agci)

# "rete imprese italia": (Casartigiani, CNA, Confartigianato, Confcommercio, Confesercenti)

# "agrinsieme": coordinamento che rappresenta le aziende e le cooperative di Cia, Confagricoltura, Copagri e Alleanza delle cooperative agroalimentari (Agci-Agrital, Confcooperative FedAgriPesca e Legacoop Agroalimentare)

reti_rappr <- c("alleanza delle cooperative italiane", "rete imprese italia", "agrinsieme", "copagri")



# Nuove organizzazioni

finco <- c("finco", "federcontribuenti", "aci", "ancca", "assoidroelettrica", "fiper", "fire")

abi <- c("acri", "assifact", "assilea", "associazione nazionale fra le banche popolari", "assofiduciaria", "assofin", "assogestioni")

camer_com <- c("camera di commercio", "unioncamere", "assocamerestero")
# mispelling "camera di commercio": "camere di commercio"

org_picked <- c("anceferr", "agrodipab", "aigab", "property managers", "property manager",
                "clia", "fsi usae", "fieg", "agci", "casartigiani", "ancimp", "federagenti",
                "confimprese", "fisi")

# "fisi": federazione italiana sindacati intercategoriali; fisi: "federazione italiana sport invernali"

# Acronimi che si riferiscono a più organizzazioni:
# federagenti, fials, aic, fisi, fai, confimprese

new_org <- c(finco, abi, camer_com, org_picked)


org_rappr <- c(org_rappr, reti_rappr, new_org)

org_rappr <- unique(org_rappr) #3


# Save data
saveRDS(org_rappr, file = "[path]/org_rappresentanza.RData")


############################################
# INTEGRAZIONI: AGGIUNTA DI NUOVI ELEMENTI #
############################################


# Load data
org_rappr <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_rappresentanza.RData")


# Da script: società_civile

# FONADDS è il Forum delle Associazioni professionali dei Docenti e dei Dirigenti - organizzazione rappresentanza
# Ne fanno parte: proteo fare sapere, ADI, AIMC, ANDIS, APEF, CIDI, DIESSE, DISAL, FNISM, IRASE, IRSEF/IRFED, LEGAMBIENTE scuola e formazione, MCE, UCIIM.

# Associazioni Professionali (da aggiungere a org_rappr)
# proteo, associazione docenti e dirigenti scolastici, associazione italiana maestri cattolici, associazione nazionale dirigenti scolastici - andis,
# cidi - centro iniziativa democratica insegnanti, diesse - didattica e innovazione scolastica, associazione dirigenti scolastici - disal,
# dirigenti scuole autonome e libere - disal, mce - movimento di cooperazione educativa, uciim

# forum arte e spettacolo (fas) - organizzazione rappresentanza

vec_rappr <- c("forum associazioni professionali docenti dirigenti", "fonadds",
               "proteo", "associazione docenti e dirigenti scolastici", "associazione italiana maestri cattolici", "associazione nazionale dirigenti scolastici", "andis",
               "cidi", "centro iniziativa democratica insegnanti", "diesse", "didattica e innovazione scolastica", "associazione dirigenti scolastici", "disal",
               "dirigenti scuole autonome e libere", "disal", "mce", "movimento di cooperazione educativa", "uciim",
               "forum arte e spettacolo", "fas")

# Tra i comitati, abbiamo filtrato i seguenti elementi:
comit_rappr <- c("comitato bus turistici", "comitato intermagistrature", "comitato unico professioni", "comitato valorizzazione dottorato",
                 "comitato sindacale ncc fiumicino", "cobti", "comitato nazionale danza arte spettacolo", "condas", "cism",
                 "comitato italiano scienze motorie", "codim", "comitato docenti indirizzo musicale", "comitato ippico guidatori allenatori",
                 "ciga", "comitato air", "autonoleggiatori italiani riuniti")

org_rappr <- c(org_rappr, vec_rappr, comit_rappr, "fpa fotografi", "acri") %>% unique()


# Save data
saveRDS(org_rappr, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/org_rappresentanza.RData")


# Revisione dizionario

# In parallelo alla costruzione del codebook, ripassiamo alcune scelte dubbie.


# Abbiamo individuato 5 sottotipi nella categoria:
# 1. Sindacati e organizzazioni di tutela collettiva
# 2. Associazioni datoriali, imprenditoriali e settoriali 
# 3. Associazioni professionali, ordini e organismi professionali di settore
# 4. Consorzi, filiere, distretti e cluster produttivi
# 5. Reti, alleanze, coordinamenti, movimenti e comitati categoriali


# La lista degli attori nella categoria è stata ottenuta dalla combinazione di tre sessioni di lavoro:
# 1. dizionario_pt1: contiene galassia confindustria e ordini professionali
# 2. dizionario_babele
# 3. dict10

# In totale, si registrano 1530 voci ma gli attori unici sono verosimilmente di meno.
# Più che standardizzare le nomenclature, ci interessa correggere le misclassificazioni.











privati <- c("energia nazionale", "8puntozero")

cat_to_civil <- c("confconsumatori", "touring club italiano", "associazione campeggiatori turistici d italia", "unpli")

ss_cr <- c("associazione italiana studio prevenzione analisi crimine",
           "aistec", "associazione italiana psicologia giuridica", "federazione medico sportiva italiana",
           "commissione italiana insegnamento matematica", "ciim",
           "commissione didattica nazionale coordinamento società storiche",
           "associazione italiana arbitrato")
# flag expertise al sottogruppo



# rimuovere "cluster tecnologico nazionale energia ctne" da centri di ricerca


# pt.3

civile <- c("convol", "csvnet", "link2007", "equogarantito", "acli", "enil", "federazione italiana caccia")

ss <- c("siml", "aifm", "fimeuc", "sivae", "airp", "goal") #expertise


# Carchiamo il vettore dal dizionario completo

dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")
dict10 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")

dizionario <- c(dizionario_pt1, dizionario_babele, dict10)

cat3 <- dizionario[["rappr"]] %>% unique()

# Eseguiamo le modifiche

# Pattern da aggiungere
to_add <- c("nidil", #sindacati
            "enci", "italia solare", #ba
            "raai", "green building council italia", "riders italia", "riders decreto", "movimento docenti motivati formazione continua", #reti
            "comitato nazionale specializzandi sostegno", "stabilizzazione docenti 180x3", "coordinamento nazionale precari scuola", "docenti precari scuola",
            "cluster tecnologico nazionale energia ctne") 

# flag: italia solare, green building council italia, cluster tecnologico nazionale energia ctne, associazione degli italianisti, fondazione italia cina,
#       diesse - didattica e innovazione scolastica, cidi - centro iniziativa democratica insegnanti, mce - movimento di cooperazione educativa, proteo, uciim

cat3 <- c(cat3, to_add)

# Pattern da rimuovere:
to_remove <- c("associazione italiana arbitrato camera arbitrale milano", "camera commercio", "unioncamere", "assocamerestero",
               "associazione nazionale comuni aeroportuali", "aci", "automobile club d'italia", "siae", "confconsumatori",
               "associazione campeggiatori turistici d italia", "unpli", "associazione italiana studio prevenzione analisi crimine",
               "aistec", "associazione italiana psicologia giuridica", "federazione medico sportiva italiana",
               "commissione didattica nazionale coordinamento società storiche", "consulta società scientifiche",
               "coordinamento presidenti corsi laurea scienze formazione primaria",
               "coordinamento presidenti corsi laurea educatore socio pedagogico pedagogista conclep",
               "centro studi coordinamento industrie beni consumo", "automobile club d italia", "touring club italiano",
               "energia nazionale", "aps", "8puntozero", "convol", "csvnet", "link2007", "equogarantito", "acli", "enil",
               "federazione italiana caccia", "siml", "aifm", "fimeuc", "sivae", "airp", "goal")
cat3 <- setdiff(cat3, to_remove)


# Contare quante volte ogni stringa della voce "categoria" compare nei dati

classifica_categoria3 <- tibble(pattern = cat3) %>%
  rowwise() %>%
  mutate(
    pattern_regex = str_c("\\b", str_escape(pattern), "\\b"),
    n_match = sum(str_detect(senato$NOMI_clean, regex(pattern_regex, ignore_case = TRUE)))
  ) %>%
  ungroup() %>%
  select(-pattern_regex) %>%
  arrange(desc(n_match))

# Solo stringhe con corrispondenza
vec_cat <- classifica_categoria3 %>%
  filter(n_match > 0) %>%
  pull(pattern)

vec_cat <- c(vec_cat, "sindacat*", "sindacal*", "unione industrial*", "doc/it")

saveRDS(vec_cat, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_categoria-revised.RData")

