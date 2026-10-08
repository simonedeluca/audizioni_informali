
# SOGGETTI ISTITUZIONALI / AUTORITA’ INDIPENDENTI

# L'[Istat](https://www.istat.it/classificazione/elenco-delle-unita-istituzionali-appartenenti-al-settore-delle-amministrazioni-pubbliche/) ha pubblicato la classificazione annuale degli enti pubblici dal 2020 al 2023.

# I dati che seguono sono contenuti nel documento del 2022 (la XVIII legislatura comprende il periodo che va dal 03/18 al 10/22).

# Scopriamo i 38 settori individuati fra amministrazioni centrali e locali. 

istat <- read_excel("[path]/Istat2022.xlsx")
istat$ENTI <- str_to_lower(istat$ENTI)
unique(istat$LABEL)

# Ogni settore contiene l'elenco delle unità istituzionali che lo compongono, i nostri stakeholders.

# Diamo un'occhiata al gruppo 1 "Organi costituzionali e di rilievo costituzionale".
istat %>% filter(LABEL=="Organi costituzionali e di rilievo costituzionale")

# Le unità saranno salvate in un vettore che si riferisce al gruppo 1; il vettore sarà incrociato con i nomi da classificare, alla ricerca di eventuali corrispondenze da marcare con l'etichetta del gruppo.

# È buona pratica riportare sia l'acronimo che la denominazione estesa dello stesso ente.

# In questo caso, mancano gli acronimi CNEL per "Consiglio Nazionale dell'Economia e del Lavoro", CSM per "Consiglio Superiore della Magistratura", UPB per "Ufficio Parlamentare di Bilancio".

# Aggiungiamoli al vettore del gruppo perché vengano contemplati nel meccanismo di matching.
v1 <- istat %>% filter(LABEL=="Organi costituzionali e di rilievo costituzionale") %>% pull(ENTI)
v1 <- c(v1, "cnel", "csm", "upb")

# Vediamo il gruppo 2 "Presidenza del Consiglio dei Ministri e Ministeri".
istat %>% filter(LABEL=="Presidenza del Consiglio dei Ministri e Ministeri")

# Innanzitutto, notiamo l'elenco dei ministeri. Uno stratagemma per snellire il vettore è l'individuazione di pattern per riferirsi a più soggetti della stessa categoria ("ministero").

# Il vettore del gruppo 2 "Presidenza del Consiglio dei Ministri e Ministeri" include le cariche e le strutture riferite al governo.
v2 <- c("presidenza del consiglio", "presidente del consiglio", "ministero", "ministro", "vice ministr*", "sottosegretario", "consiglio dei ministri", "mipaaf", "mipaaft", "icqrf", "protezione civile")

# Note gruppo 2 (aggiunti)
# Ministero delle Politiche Agricole, Alimentari e Forestali (MIPAAF)
# Ministero delle Politiche Agricole, Alimentari e Forestali e del Turismo (MIPAAFT)
# Ispettorato centrale della tutela della qualità e della repressione frodi dei prodotti agroalimentari (ICQRF) organismo di controllo dei prodotti agroalimentari in IT e EU.
# Dipartimento della Protezione Civile

# I gruppi 3-9 contengono gli elenchi completi degli enti riconducibili ai rispettivi settori istituzionali. Copiamoli nei vettori corrispondenti.
v3 <- istat %>% filter(LABEL=="Agenzie fiscali") %>% pull(ENTI)
v4 <- istat %>% filter(LABEL=="Enti di regolazione dell'attività economica") %>% pull(ENTI)
v5 <- istat %>% filter(LABEL=="Enti produttori di servizi economici") %>% pull(ENTI)
v6 <- istat %>% filter(LABEL=="Autorità amministrative indipendenti") %>% pull(ENTI)
v6 <- c(v6, "banca d'italia")
v7 <- istat %>% filter(LABEL=="Enti a struttura associativa") %>% pull(ENTI)
v8 <- istat %>% filter(LABEL=="Enti produttori di servizi assistenziali, ricreativi e culturali") %>% pull(ENTI)
v9 <- istat %>% filter(LABEL=="Enti e Istituzioni di ricerca") %>% pull(ENTI)

# Completiamo la lista delle autorità indipendenti (fonte Wikipedia).
v6 <- c(v6, "cgs", "commissione di vigilanza sui fondi pensione - covip", "commissione nazionale per le società e la borsa - consob", "garante nazionale dei diritti delle persone private della libertà personale - gnpl", "istituto per la vigilanza sulle assicurazioni - ivass")

# Forme da tenere in considerazione perché i dati contengono errori (check)
# commissione di garanzia
# garante nazionale

# Alcuni enti sono stati salvati con il nome esteso più l'acronimo, preceduto dal trattino. Le buone pratiche suggeriscono di tenere le espressioni separate nel vettore perché potremmo incontrare solo una delle due forme nelle stringhe da classificare.
# Inoltre, sono presenti delle formule di rumore che andrebbero rimosse: riportiamole in un vettore di pulizia. Rimandiamo le operazioni alla fase successiva.

# Per il gruppo 10 "Istituti zooprofilattici sperimentali" può bastare l'espressione parziale.
v10 <- "istituto zooprofilattico"

# I gruppi 11-15 trattano gli enti locali.

# Regioni e province autonome
v11 <- c("sicilia", "sardegna", "calabria", "basilicata", "puglia", "campania", "molise", "abruzzo", "lazio", "toscana", "umbria", "marche", "emilia romagna", "liguria", "piemonte", "lombardia", "veneto", "friuli venezia giulia", "trentino alto adige", "valle d'aosta", "province autonome di")

# Province e città metropolitane
v12 <- c("provincia", "metropolitana", "consorzio comunale")

# Comuni
v13 <- "comune"

# Comunità montane (none)*
# *La nota (none) indica che il gruppo ha generato zero risultati con la ricerca manuale nei dati da classificare; il vettore non ha senso di esistere ai fini del nostro studio.

# Unioni di comuni
v15 <- c("unione comuni", "unione montana")


# Passiamo in rassegna i restanti settori della PA.

# Agenzie, enti e consorzi per il diritto allo studio universitario (none)

# Agenzie ed enti regionali del lavoro (none)

# Agenzie regionali per la rappresentanza negoziale (none)                                 

# Agenzie regionali per le erogazioni in agricoltura (none)

# Agenzie regionali sanitarie e aziende ed enti di supporto al SSN
v20 <- c("emergenza urgenza", "ares 118")

# Enti di governo dei servizi idrici e/o dei rifiuti (ex AATO) (none)

# Autorità di sistema portuale
v22 <- "autorità di sistema portuale"

# Aziende ospedaliere, aziende ospedaliero-universitarie, policlinici e istituti di ricovero e cura a carattere scientifico pubblici
v23 <- "policlinico"

# Azienda sanitaria locale
v24 <- c("asl", "usl")

# Camere di commercio, industria, artigianato e agricoltura e unioni regionali
v25 <- "camera di commercio"

# Consorzi di bacino imbrifero montano (none)

# Parchi nazionali, consorzi ed enti gestori di parchi e aree naturali protette
v27 <- "parco"

# Agenzie ed enti regionali di sviluppo agricolo (none)

# Agenzie ed enti per il turismo (none)

# Agenzie ed enti regionali e provinciali per la formazione, la ricerca e l'ambiente
v30 <- "arpa" #Agenzia Regionale per la Protezione dell’Ambiente

# Autorità di bacino del distretto idrografico
v31 <- "autorità di bacino"

# Consorzi tra amministrazioni locali (none)

# Consorzi interuniversitari di ricerca
v33 <- c("consorzio interuniversitario", "consorzio nazionale interuniversitario")

# Fondazioni lirico-sinfoniche (none)     

# Teatri nazionali e di rilevante interesse culturale (none)

# Università e istituti di istruzione universitaria pubblici
v36 <- c("università", "politecnico")

# Altre amministrazioni locali
v37 <- c("aipo", "astral", "istituto regionale per la floricoltura", "sviluppumbria")

# Enti nazionali di previdenza e assistenza sociale  
v38 <- istat %>% filter(LABEL=="Enti nazionali di previdenza e assistenza sociale") %>% pull(ENTI)


# I settori descritti dall'Istat coprono una fetta importante delle istituzioni italiane, ma la nostra categoria è più ampia.


# Andiamo ad individuare ulteriori gruppi.

# Istituzioni internazionali
org_int <- c("organizzazione mondiale della sanità", "ocse", "fondo monetario internazionale")

# Europa
eu <- c("commissione europea", "comitato europeo delle regioni", "banca europea", "corte dei conti europea")

# Organismi degli enti locali
org_enti_local <- c("conferenza stato regioni", "consiglio regionale", "conferenza delle regioni e delle province autonome", "conferenza dei presidenti delle assemblee legislative delle regioni e delle province autonome", "associazione nazionale piccoli comuni d'italia - anpci")
# anpci qui o in associazioni?

# Corpi armati dello Stato
forze_sicurezza <- c("esercito", "carabinieri", "marina militare", "capitaneri*", "aeronautica", "guardia di finanza", "polizia", "penitenziaria", "vigili del fuoco", "vvf")

gradi_militari <- c("gen", "ammiraglio", "col")

# Cariche istituzionali
carica_ist <- c("ambasciatore", "amb", "console", "senatore", "sindaco", "assessore", "cons", "capo di gabinetto", "commissari*")
# commissari* è propriamente istituzionale?
# mancano alcuni ruoli come "presidente" o "vice presidente", a cui preferiamo dare connotazione neutra perché non esclusivi del settore pubblico.

# Iniziamo a formare i gruppi neutri con le parole comuni alle categorie. Si creano poi nuovi gruppi, con le parole associate alle prime che ne specificano la dimensione. Teniamo questi accoppiamenti in mente nella costruzione delle regole per l'assegnazione della categoria attesa.

# Cariche neutre
carica_neutra <- c("presidente", "pres", "vice presidente", "direttore", "dirigente", "garante")

# Struttura neutra
str_neutra <- c("consiglio", "commissione", "comitato", "dipartimento", "istituto")
# dipartimento è proprio dei soggetti istituzionali?
# comitato è proprio delle organizzazioni civili?

# Dimensione istituzionale
dim_ist <- c("tecnic*", "bilaterale", "di stato", "di garanzia")

# Dimensione geografica
geo <- c("mondiale", "internazionale", "europea*", "nazional*", "italia*", "interregionale", "regional*", "interprovinciale", "provincial*", "comunal*")

# Regole:  
# [1] struttura neutra + dimensione istituzionale = soggetto istituzionale

# Salviamo altre informazioni da collocare nei gruppi opportuni.

# Università private/telematiche, in contrapposizione con l'istruzione pubblica
uni_private <- c("bocconi", "cattolica", "mercatorum", "gregoriana", "telematica", "campus biomedico", "luiss")


# Lista per il dizionario

ist_list <- list(v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v8=v8, v9=v9,
                 v10=v10, v11=v11, v12=v12, v13=v13, v15=v15, v20=v20, v22=v22,
                 v23=v23, v24=v24, v25=v25, v27=v27, v30=v30, v31=v31, v33=v33,
                 v36=v36, v37=v37, v38=v38, org_enti_local=org_enti_local,
                 forze_sicurezza=forze_sicurezza,gradi_militari=gradi_militari,
                 carica_ist=carica_ist, eu=eu, org_int=org_int)


# Save data
saveRDS(ist_list, file = "[path]/institution_list.RData")


############################################
# INTEGRAZIONI: AGGIUNTA DI NUOVI ELEMENTI #
############################################


# Load data
ist_list <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list.RData")


# Da script: società_civile

# Fondazioni e comitati di natura istituzionale
fond_ist <- c("fondazione inarcassa", "direttore fondazione scuola beni attività culturali sbac", "fondazione agrion",
              "fondazione slala", "fondazione scuola beni attività culturali", "sbac", "orchestra roma lazio", "ico")

comit_ist <- c("comitato europeo regioni", "comitato sindaci amministratori lazio abruzzo sicurezza caro pedaggi a24 a25",
               "edufin", "comitato nazionale universitario", "cnu", "comitato apprendimento pratico musica studenti",
               "cnapm", "conferenza direttori conservatori musica", "comitato tecnico scientifico" , "comitato olimpico nazionale italiano",
               "coni", "comitato tecnico paralimpico", "cip", "comitato esperti", "comitato organizzativo milano cortina",
               "comitato nazionale bioetica", "comitato scientifico futuro europa")

# Aggiunta dei vettori alla lista
ist_list$fond_ist = fond_ist
ist_list$comit_ist = comit_ist
ist_list$new_vec = c("consiglio nazionale dei consumatori e degli utenti", "CNCU")


# Save data
saveRDS(ist_list, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list.RData")


# Revisione dizionario

# In parallelo alla costruzione del codebook, aggiorniamo le liste dei sottotipi nella categoria,
# alla luce delle risultanze della prima classificazione degli attori, in particolare dei casi
# ambigui.


dict_ist <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list.RData")

# Organi costituzionali, di rilievo costituzionale, giurisdizionali e di controllo
dict_ist[["v1"]] <- c(dict_ist[["v1"]], "corte di cassazione")
dict_ist[["v1"]] <- dict_ist[["v1"]][-c(1,7,8)]

# Presidenza del Consiglio dei ministri e Ministeri
dict_ist[["v2"]] <- dict_ist[["v2"]][-c(1,2,4,5,6,7)]
dict_ist[["v2"]] <- c(dict_ist[["v2"]], "presidenza del consiglio dei ministri")

# Agenzie fiscali
dict_ist[["v3"]]

# Enti di regolazione dell’attività economica
dict_ist[["v4"]] <- dict_ist[["v4"]][
  !str_detect(dict_ist[["v4"]], fixed("s.p.a."))
  ]

# Enti produttori di servizi economici
dict_ist[["v5"]] <- dict_ist[["v5"]][
  !str_detect(dict_ist[["v5"]], fixed("s.p.a."))
]
dict_ist[["v5"]] <- dict_ist[["v5"]][-c(1,2,4,6:8,10:14)]

# Autorità amministrative indipendenti
dict_ist[["v6"]] <- c(dict_ist[["v6"]], "banca d'italia")
dict_ist[["v6"]] <- dict_ist[["v6"]][-c(10)]

# Enti a struttura associativa
dict_ist[["v7"]] <- dict_ist[["v7"]][-c(2:4)]
dict_ist[["v7"]] <- c(dict_ist[["v7"]], "ancim - associazione nazionale comuni isole minori",
                      "anpci - associazione nazionale piccoli comuni italiani")

# Enti pubblici culturali, ricreativi e sportivi
dict_ist[["v8"]] <- dict_ist[["v8"]][-5]
dict_ist[["v8"]] <- dict_ist[["v8"]][
  !str_detect(dict_ist[["v8"]], fixed("s.p.a."))
  ]  
dict_ist[["v8"]] <- dict_ist[["v8"]][-c(1:3,6:19)]
dict_ist[["v8"]] <- c(dict_ist[["v8"]], "parco archeologico", "fondazione ottavio ziino orchestra di roma e del lazio")

# Enti e istituzioni di ricerca

# Istituti zooprofilattici italiani

# Enti locali - v11:v13,v15
dict_ist[["enti_locali"]] <- c(dict_ist[["v11"]], dict_ist[["v12"]], dict_ist[["v13"]], dict_ist[["v15"]])
dict_ist <- dict_ist[-c(11,12,13,14)]
dict_ist[["enti_locali"]] <- dict_ist[["enti_locali"]][-24]
dict_ist[["enti_locali"]] <- c(dict_ist[["enti_locali"]], "regione")

# Aziende, enti e strutture del SSN
dict_ist[["v20"]] <- c("ssuem 118", "ares 118", "areu lombardia", "sistema 118", "centrale operativa 118", "118 sassari")

# Autorità di sistema portuale

# Aziende ospedaliere, aziende ospedaliero-universitarie, policlinici e istituti di ricovero e cura a carattere scientifico pubblici

# Azienda sanitaria locale

# Camere di commercio

dict_ist <- dict_ist[-c(16)]

# Agenzie ed enti regionali, provinciali e interregionali per l’ambiente, territorio, ricerca e formazione
dict_ist[["v30"]] <- c("arpa", "istituto regionale per la floricoltura", "agenzia provinciale per le risorse idriche e l’energia della provincia autonoma di trento", "Agenzia provinciale per l’ambiente e la tutela del clima della provincia autonoma di bolzano", "aipo")

# Autorità di bacino del distretto idrografico

# Consorzi interuniversitari di ricerca

# Università e istituti di istruzione universitaria statali

# Enti nazionali di previdenza e assistenza sociale
dict_ist[["v38"]] <- dict_ist[["v38"]][c(21,22)]

# Organizzazioni internazionali
dict_ist[["org_int"]] <- c("organizzazione mondiale della sanità animale", "ocse", "fondo monetario internazionale", "world customs organization")

# Istituzioni e organismi dell'Unione Europea
dict_ist[["eu"]][3] <- "banca europea per gli investimenti"
dict_ist[["eu"]] <- c(dict_ist[["eu"]], "bce")

# Organismi di raccordo
dict_ist[["org_enti_local"]] <- dict_ist[["org_enti_local"]][-5]
dict_ist[["org_raccordo"]] <- dict_ist[["org_enti_local"]]
dict_ist <- dict_ist[-c(22)]

# Corpi armati dello Stato
dict_ist[["forze_sicurezza"]] <- c("capitaneri*", "polizia", "carabinieri", "guardia di finanza", "vigili del fuoco")
dict_ist[["corpi_armati"]] <- dict_ist[["forze_sicurezza"]]
dict_ist <- dict_ist[-c(22)]

# Fondazioni istituzionali
dict_ist[["fond_ist"]] <- c("sbac", "fondazione scuola per i beni e le attività culturali", "agrion", "fondazione ottavio ziino orchestra di roma e del lazio")

# Comitati istituzionali
dict_ist[["comit_ist"]] <- dict_ist[["comit_ist"]][-c(1,4,5,10:13,15)]

dict_ist <- dict_ist[-c(27)]

# Reti e associazioni tematiche di enti territoriali
dict_ist[["reti_territori"]] <- c("associazione nazionale città del tartufo", "associazione città dell'olio", "aicc", "associazione italiana città della ceramica",
                                "associazione la strada della ceramica in umbria", "associazione borghi più belli d'italia")

# Università e istituti di istruzione universitaria non statali
dict_ist[["uni_non_statali"]] <- c("università bocconi", "università cattolica", "università mercatorum", "università gregoriana", "università telematica", "campus biomedico", "luiss")

# Ruoli istituzionali

# Gradi militari
dict_ist[["gradi_militari"]] <- c("gen", "ten", "ammiraglio", "ispettore", "comandante")

# Cariche istituzionali
dict_ist[["carica_ist"]] <- c("presidente del consiglio" , "ministro" , "vice ministr*" , "sottosegretario" , "sindaco" , "assessore" , "senatore" , "ambasciatore" , "amb", "console" ,
                              "capo di gabinetto" , "commissario ad acta" , "commissari* straordinari*", "commissari designati", "commissario per la messa in sicurezza", "commissaria europea",
                              "commissario generale sezione expo", "cons")


# Save data
saveRDS(dict_ist, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised.RData")

###

vec_ist <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised.RData")

# Misclassificazioni di categoria in istituzioni


# Camere di commercio
vec_ist[["v25"]] <- c(vec_ist[["v25"]], "camere commercio")

# Enti a struttura associativa
vec_ist[["v7"]] <- c(vec_ist[["v7"]], "unioncamere", "assocamerestero")

# Enti di ricerca e organismi tecnico-scientifici
vec_ist[["v9"]] <- c(vec_ist[["v9"]], "ente italiano normazione", "uni", "uninfo") # flag enti

# Organismi di raccordo istituzionale
vec_ist[["org_raccordo"]] <- c(vec_ist[["org_raccordo"]], "coordinamento presidenti corsi laurea scienze formazione primaria", "coordinamento presidenti corsi laurea educatore socio pedagogico pedagogista",
                               "conferenza presidenti conservatori musica", "conferenza direttori conservatori musica", "cpcsai", "cnsi", "consulta società scientifiche") # flag enti

# Reti e associazioni tematiche di enti territoriali
vec_ist[["reti_territori"]] <- c(vec_ist[["reti_territori"]], "associazione nazionale comuni aeroportuali")

# Enti pubblici culturali, ricreativi e sportivi
vec_ist[["v8"]] <- c(vec_ist[["v8"]], "aci", "automobile club d italia", "club alpino italiano", "lega navale italiana", "aero club", "anmli")

# Enti produttori di servizi economici
vec_ist[["v5"]] <- c(vec_ist[["v5"]], "siae")

# Save data
saveRDS(vec_ist, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution_list-revised.RData") # need preprocessing

# Qual è lo scarto tra il vettore istituzioni nel dizionario completo e institution_list_revised???
