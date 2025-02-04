
# SOGGETTI ISTITUZIONALI / AUTORITA’ INDIPENDENTI

# L'[Istat](https://www.istat.it/classificazione/elenco-delle-unita-istituzionali-appartenenti-al-settore-delle-amministrazioni-pubbliche/) ha pubblicato la classificazione annuale degli enti pubblici dal 2020 al 2023.

# I dati che seguono sono contenuti nel documento del 2022 (la XVIII legislatura comprende il periodo che va dal 03/18 al 10/22).

# Scopriamo i 38 settori individuati fra amministrazioni centrali e locali. 

istat <- read_excel("~/.../audizioni_informali/data/Istat2022.xlsx")
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
org_int <- c("organizzazione mondiale della sanità", "ocse", "fondo monetario internazionale", "croce rossa", "unicef", "wwf", "save the children")
# è utile una distinzione tra istituzioni politiche e ong?

# Europa
eu <- c("commissione europea", "comitato europeo delle regioni", "banca europea", "corte dei conti europea")

# Organismi degli enti locali
org_enti_local <- c("conferenza stato regioni", "consiglio regionale", "conferenza delle regioni e delle province autonome", "conferenza dei presidenti delle assemblee legislative delle regioni e delle province autonome", "associazione nazionale piccoli comuni d'italia - anpci")
# anpci qui o in associazioni?

# Corpi armati dello Stato
forze_sicurezza <- c("esercito", "carabinieri", "marina militare", "capitaneri*", "aeronautica", "guardia di finanza", "polizia", "penitenziaria")

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

# Altri enti (to ask)
# Dove collocare "vigili del fuoco - vvf", "banca d'italia", "poste italiane", "trenitalia"?

# Lista per il dizionario

ist_list <- list(v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v8=v8, v9=v9,
                 v10=v10, v11=v11, v12=v12, v13=v13, v15=v15, v20=v20, v22=v22,
                 v23=v23, v24=v24, v25=v25, v27=v27, v30=v30, v31=v31, v33=v33,
                 v36=v36, v37=v37, v38=v38, org_enti_local=org_enti_local,
                 forze_sicurezza=forze_sicurezza,gradi_militari=gradi_militari,
                 carica_ist=carica_ist, eu=eu)


save(ist_list, file = "~/.../audizioni_informali/data/clean_data/istitution_list.RData")
