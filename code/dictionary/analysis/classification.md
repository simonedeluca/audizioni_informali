Classification of the stakeholders - Progetto Audizioni
================
Simone De Luca - Università di Milano

<style type="text/css">
body{ /* Normal  */
  font-size: 18px;
  font-family: "Times New Roman";
  text-align: justify;
}
h1 { /* Header 1 */
  font-size: 30px;
  color: #183691;
  text-align: justify;
}
h2 { /* Header 2 */
  font-size: 24px;
  color: #63A35C;
  text-align: justify;
}
</style>

## INTRODUZIONE

L’obiettivo di questa parte del progetto è classificare gli stakeholders
che hanno partecipato alle audizioni informali del Senato.

Nel campo del Natural Language Processing (NLP), l’approccio scelto per
eseguire la classificazione è la tecnica basata su regole (rule-based
tecnique): si tratta di dare istruzioni sull’etichetta da assegnare a
partire da specifiche condizioni. In pratica, cerchiamo le parole chiave
o combinazioni di parole associate ad una categoria e raccogliamo le
associazioni in un dizionario di settore.

La tecnica si avvale di un ragionamento deduttivo, ideale al nostro caso
perché ereditiamo le categorie dalla precedente analisi.

Le categorie proposte sono sei:

1)  Soggetti istituzionali/autorità indipendenti

2)  Sindacati e associazioni di categoria

3)  Associazioni ed organizzazioni della società civile

4)  Aziende e società a partecipazione pubblica

5)  Aziende private

6)  Professori universitari/esperti/professionisti

Il lavoro è finalizzato alla creazione di un dizionario utile alla
classificazione degli stakeholders nel panorama italiano.

L’idea è di “spacchettare” le macrocategorie in sottocategorie, dal
generale al particolare, e di descrivere i singoli insiemi. Il secondo
livello, oltre a dare profondità all’analisi, è utile dal punto di vista
semantico nello sviluppo del dizionario, perché aiuta a circoscrivere la
selezione delle parole identificative dei vari sottogruppi.

Gli insiemi sono descritti con l’elenco dei rispettivi membri, organismi
e ruoli interni.

Il sistema di regole per assegnare la nomenclatura gioca un ruolo
fondamentale.

Bisogna tener conto che l’informazione da codificare può essere diretta
o risultante dalla combinazione di più elementi. I nomi da classificare
sono delle stringhe di testo più o meno informative; nella fattispecie,
sono di lunghezza diversa, possono contenere errori di varia natura,
varianti per riferirsi allo stesso soggetto, casi di omonimia.

Vediamo un esempio pratico.

Prendiamo le espressioni “Presidenza del Consiglio dei Ministri” e
“Presidente del Consiglio”. Entrambe si riferiscono alla figura di
Giuseppe Conte, al tempo capo del governo: la prima forma è
standardizzatà dall’Istat, la seconda è un’alternativa che si trova nel
dataset del Senato.

Ma le stesse espressioni si prestano a più declinazioni, cioè quando
sono una delle informazioni contenute nella stringa.

``` r
senato <- read_excel("[path]/dataset_senato.xlsx")
senato %>% filter(grepl("presidenza del consiglio", NOMI, ignore.case = TRUE)) %>% select(NOMI) %>% unique()
```

    ## # A tibble: 5 × 1
    ##   NOMI                                                                          
    ##   <chr>                                                                         
    ## 1 presidenza del consiglio dei ministri                                         
    ## 2 sottosegretario di stato alla presidenza del consiglio dei ministri con deleg…
    ## 3 sottosegretario di stato alla presidenza del consiglio dei ministri, valentin…
    ## 4 sottosegretario di stato alla presidenza del consiglio dei ministri valentina…
    ## 5 sottosegretario di stato alla presidenza del consiglio dei ministri, giancarl…

``` r
senato %>% filter(grepl("presidente del consiglio", NOMI, ignore.case = TRUE)) %>% select(NOMI)
```

    ## # A tibble: 9 × 1
    ##   NOMI                                                                          
    ##   <chr>                                                                         
    ## 1 presidente del consiglio regionale michele pais                               
    ## 2 giuseppe conte, presidente del consiglio                                      
    ## 3 on antonio leone, presidente del consiglio di presidenza della giustizia trib…
    ## 4 presidente del consiglio direttivo della divisione calcio paralimpico e speri…
    ## 5 presidente del consiglio di indirizzo e vigilanza dell'inps                   
    ## 6 presidente del consiglio nazionale dell'ordine dei consulenti del lavoro      
    ## 7 prof franco locatelli, professore ordinario di pediatria generale e specialis…
    ## 8 pier virgilio dastoli, presidente del consiglio italiano del movimento europeo
    ## 9 presidente del consiglio regionale del friuli venezia giulia

La forma “presidenza del consiglio” preceduta dalla parola
“sottosegretario” identifica un soggetto diverso anche se cambia poco ai
fini della classificazione visto che si resta nel campo del governo.

La carica di “presidente del consiglio” invece è un’espressione comune
che può sfociare dall’ambito istituzionale (“presidente del consiglio
nazionale dell’ordine dei consulenti del lavoro”).

La codifica di ulteriori elementi nelle strighe ne determinerà la
corretta classificazione secondo un sistema di regole che tenga conto
del peso delle combinazioni.

Al proposito, verranno creati vettori a carattere neutro che accolgono
quei termini (come “presidente” e “consiglio”) non associabili in modo
univoco a nessuna categoria.

Più il dizionario è completo, maggiore sarà l’accuratezza della
classificazione. La proposta non ha pretese esaustive, cioè non
garantisce la copertura totale degli enti appartenenti alle classi
proposte.

L’orizzonte è il dataset delle audizioni informali del Senato della XIII
legislatura.

Procediamo con la descrizione della prima categoria.

## SOGGETTI ISTITUZIONALI / AUTORITA’ INDIPENDENTI

L’[Istat](https://www.istat.it/classificazione/elenco-delle-unita-istituzionali-appartenenti-al-settore-delle-amministrazioni-pubbliche/)
ha pubblicato la classificazione annuale degli enti pubblici dal 2020 al
2023.

I dati che seguono sono contenuti nel documento del 2022 (la XVIII
legislatura comprende il periodo che va dal 03/18 al 10/22).

Scopriamo i 38 settori individuati fra amministrazioni centrali e
locali.

``` r
istat <- read_excel("[path]/data/Istat2022.xlsx")
istat$ENTI <- str_to_lower(istat$ENTI)
unique(istat$LABEL)
```

    ##  [1] "Organi costituzionali e di rilievo costituzionale"                                                                                 
    ##  [2] "Presidenza del Consiglio dei Ministri e Ministeri"                                                                                 
    ##  [3] "Agenzie fiscali"                                                                                                                   
    ##  [4] "Enti di regolazione dell'attività economica"                                                                                       
    ##  [5] "Enti produttori di servizi economici"                                                                                              
    ##  [6] "Autorità amministrative indipendenti"                                                                                              
    ##  [7] "Enti a struttura associativa"                                                                                                      
    ##  [8] "Enti produttori di servizi assistenziali, ricreativi e culturali"                                                                  
    ##  [9] "Enti e Istituzioni di ricerca"                                                                                                     
    ## [10] "Istituti zooprofilattici sperimentali"                                                                                             
    ## [11] "Regioni e province autonome"                                                                                                       
    ## [12] "Province e città metropolitane"                                                                                                    
    ## [13] "Comuni"                                                                                                                            
    ## [14] "Comunità montane"                                                                                                                  
    ## [15] "Unioni di comuni"                                                                                                                  
    ## [16] "Agenzie, enti e consorzi per il diritto allo studio universitario"                                                                 
    ## [17] "Agenzie ed enti regionali del lavoro"                                                                                              
    ## [18] "Agenzie regionali per la rappresentanza negoziale"                                                                                 
    ## [19] "Agenzie regionali per le erogazioni in agricoltura"                                                                                
    ## [20] "Agenzie regionali sanitarie e aziende ed enti di supporto al SSN"                                                                  
    ## [21] "Enti di governo dei servizi idrici e/o dei rifiuti (ex AATO)"                                                                      
    ## [22] "Autorità di sistema portuale"                                                                                                      
    ## [23] "Aziende ospedaliere, aziende ospedaliero-universitarie, policlinici e istituti di ricovero e cura a carattere scientifico pubblici"
    ## [24] "Aziende sanitarie locali"                                                                                                          
    ## [25] "Camere di commercio, industria, artigianato e agricoltura e unioni regionali"                                                      
    ## [26] "Consorzi di bacino imbrifero montano"                                                                                              
    ## [27] "Parchi nazionali, consorzi ed enti gestori di parchi e aree naturali protette"                                                     
    ## [28] "Agenzie ed enti regionali di sviluppo agricolo"                                                                                    
    ## [29] "Agenzie ed enti per il turismo"                                                                                                    
    ## [30] "Agenzie ed enti regionali e provinciali per la formazione, la ricerca e l'ambiente"                                                
    ## [31] "Autorità di bacino del distretto idrografico"                                                                                      
    ## [32] "Consorzi tra amministrazioni locali"                                                                                               
    ## [33] "Consorzi interuniversitari di ricerca"                                                                                             
    ## [34] "Fondazioni lirico-sinfoniche"                                                                                                      
    ## [35] "Teatri nazionali e di rilevante interesse culturale"                                                                               
    ## [36] "Università e istituti di istruzione universitaria pubblici"                                                                        
    ## [37] "Altre amministrazioni locali"                                                                                                      
    ## [38] "Enti nazionali di previdenza e assistenza sociale"

Ogni settore contiene l’elenco delle unità istituzionali che lo
compongono, i nostri stakeholders.

Diamo un’occhiata al gruppo 1 “Organi costituzionali e di rilievo
costituzionale”.

``` r
istat %>% filter(LABEL=="Organi costituzionali e di rilievo costituzionale")
```

    ## # A tibble: 9 × 2
    ##   ENTI                                                    LABEL                 
    ##   <chr>                                                   <chr>                 
    ## 1 camera dei deputati                                     Organi costituzionali…
    ## 2 consiglio di stato                                      Organi costituzionali…
    ## 3 consiglio nazionale dell’economia e del lavoro          Organi costituzionali…
    ## 4 consiglio superiore della magistratura                  Organi costituzionali…
    ## 5 corte costituzionale                                    Organi costituzionali…
    ## 6 corte dei conti                                         Organi costituzionali…
    ## 7 segretariato generale della presidenza della repubblica Organi costituzionali…
    ## 8 senato della repubblica                                 Organi costituzionali…
    ## 9 ufficio parlamentare di bilancio                        Organi costituzionali…

Le unità saranno salvate in un vettore che si riferisce al gruppo 1; il
vettore sarà incrociato con i nomi da classificare, alla ricerca di
eventuali corrispondenze da marcare con l’etichetta del gruppo.

È buona pratica riportare sia l’acronimo che la denominazione estesa
dello stesso ente.

In questo caso, mancano gli acronimi CNEL per “Consiglio Nazionale
dell’Economia e del Lavoro”, CSM per “Consiglio Superiore della
Magistratura”, UPB per “Ufficio Parlamentare di Bilancio”.

Aggiungiamoli al vettore del gruppo perché vengano contemplati nel
meccanismo di matching.

``` r
v1 <- istat %>% filter(LABEL=="Organi costituzionali e di rilievo costituzionale") %>% pull(ENTI)
v1 <- c(v1, "cnel", "csm", "upb")
```

Vediamo il gruppo 2 “Presidenza del Consiglio dei Ministri e Ministeri”.

``` r
istat %>% filter(LABEL=="Presidenza del Consiglio dei Ministri e Ministeri")
```

    ## # A tibble: 16 × 2
    ##    ENTI                                                              LABEL      
    ##    <chr>                                                             <chr>      
    ##  1 ministero degli affari esteri e della cooperazione internazionale Presidenza…
    ##  2 ministero del lavoro e delle politiche sociali                    Presidenza…
    ##  3 ministero del turismo                                             Presidenza…
    ##  4 ministero dell’economia e delle finanze                           Presidenza…
    ##  5 ministero dell’interno                                            Presidenza…
    ##  6 ministero dell’istruzione                                         Presidenza…
    ##  7 ministero dell’università e della ricerca                         Presidenza…
    ##  8 ministero della cultura                                           Presidenza…
    ##  9 ministero della difesa                                            Presidenza…
    ## 10 ministero della giustizia                                         Presidenza…
    ## 11 ministero della salute                                            Presidenza…
    ## 12 ministero della transizione ecologica                             Presidenza…
    ## 13 ministero delle infrastrutture e della mobilità sostenibili       Presidenza…
    ## 14 ministero delle politiche agricole alimentari e forestali         Presidenza…
    ## 15 ministero dello sviluppo economico                                Presidenza…
    ## 16 presidenza del consiglio dei ministri                             Presidenza…

Innanzitutto, notiamo l’elenco dei ministeri. Uno stratagemma per
snellire il vettore è l’individuazione di pattern per riferirsi a più
soggetti della stessa categoria (“ministero”).

Il vettore del gruppo 2 “Presidenza del Consiglio dei Ministri e
Ministeri” include le cariche e le strutture riferite al governo.

``` r
v2 <- c("presidenza del consiglio", "presidente del consiglio", "ministero", "ministro", "vice ministr*", "sottosegretario", "consiglio dei ministri", "mipaaf", "mipaaft", "icqrf", "protezione civile")

# Ministero delle Politiche Agricole, Alimentari e Forestali (MIPAAF)
# Ministero delle Politiche Agricole, Alimentari e Forestali e del Turismo (MIPAAFT)
# Ispettorato centrale della tutela della qualità e della repressione frodi dei prodotti agroalimentari (ICQRF) organismo di controllo dei prodotti agroalimentari in IT e EU.
# Dipartimento della Protezione Civile
```

I gruppi 2-9 contengono gli elenchi completi degli enti riconducibili ai
rispettivi settori istituzionali. Copiamoli nei vettori corrispondenti.

``` r
v3 <- istat %>% filter(LABEL=="Agenzie fiscali") %>% pull(ENTI)
v4 <- istat %>% filter(LABEL=="Enti di regolazione dell'attività economica") %>% pull(ENTI)
v5 <- istat %>% filter(LABEL=="Enti produttori di servizi economici") %>% pull(ENTI)
v6 <- istat %>% filter(LABEL=="Autorità amministrative indipendenti") %>% pull(ENTI)
v7 <- istat %>% filter(LABEL=="Enti a struttura associativa") %>% pull(ENTI)
v8 <- istat %>% filter(LABEL=="Enti produttori di servizi assistenziali, ricreativi e culturali") %>% pull(ENTI)
v9 <- istat %>% filter(LABEL=="Enti e Istituzioni di ricerca") %>% pull(ENTI)
```

Completiamo la lista delle autorità indipendenti (fonte Wikipedia).

``` r
v6 <- c(v6, "cgs", "commissione di vigilanza sui fondi pensione - covip", "commissione nazionale per le società e la borsa - consob", "garante nazionale dei diritti delle persone private della libertà personale - gnpl", "istituto per la vigilanza sulle assicurazioni - ivass")

# Forme da tenere in considerazione perché i dati contengono errori

# commissione di garanzia
# garante nazionale
```

Alcuni enti sono stati salvati con il nome esteso più l’acronimo,
preceduto dal trattino. Le buone pratiche suggeriscono di tenere le
espressioni separate nel vettore perché potremmo incontrare solo una
delle due forme nelle stringhe da classificare. Inoltre, sono presenti
delle formule di rumore che andrebbero rimosse: riportiamole in un
vettore di pulizia./Rimandiamo le operazioni alla fase successiva.

Per il gruppo 10 “Istituti zooprofilattici sperimentali” può bastare
l’espressione parziale.

``` r
v10 <- "istituto zooprofilattico"
```

I gruppi 11-15 trattano gli enti locali.

``` r
# Regioni e province autonome
v11 <- c("sicilia", "sardegna", "calabria", "basilicata", "puglia", "campania", "molise", "abruzzo", "lazio", "toscana", "umbria", "marche", "emilia romagna", "liguria", "piemonte", "lombardia", "veneto", "friuli venezia giulia", "trentino alto adige", "valle d'aosta", "province autonome di")

# Province e città metropolitane
v12 <- c("provincia", "metropolitana", "consorzio comunale")

# Comuni
v13 <- "comune"

# Comunità montane (none)*

# Unioni di comuni
v15 <- c("unione comuni", "unione montana")

# *La nota (none) indica che il gruppo ha generato zero risultati con la ricerca manuale nei dati da classificare; il vettore non ha senso di esistere ai fini del nostro studio.
```

Passiamo in rassegna i restanti settori della PA.

``` r
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
```

I settori descritti dall’Istat coprono una fetta importante delle
istituzioni, ma ci sono dei gruppi che mancano all’appello.

``` r
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
```

Iniziamo a formare i gruppi neutri con le parole comuni alle categorie.
Si creano poi nuovi gruppi, con le parole associate alle prime che ne
specificano la dimensione. Teniamo questi accoppiamenti in mente nella
costruzione delle regole per l’assegnazione della categoria attesa.

``` r
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
```

Regole:  
\[1\] struttura neutra + dimensione istituzionale = soggetto
istituzionale

Salviamo altre informazioni da collocare nei gruppi opportuni.

``` r
# Università private/telematiche, in contrapposizione con l'istruzione pubblica
uni_private <- c("bocconi", "cattolica", "mercatorum", "gregoriana", "telematica", "campus biomedico", "luiss")

# Altri enti (to ask)
# Dove collocare "vigili del fuoco - vvf", "banca d'italia", "poste italiane", "trenitalia"?
```

## PULIZIA

Passiamo alla fase di pulizia. Sia le stringhe da classificare che i
vettori che descrivono le categorie devono essere processati alla stessa
maniera, in modo da ottenere una forma standardizzata comune.

Le modifiche coinvolgono la punteggiatura, le parole poco significative,
e la separazione degli acronimi dalle rispettive denominazioni per
esteso.

A questo punto, creiamo il nostro corpus dai nomi del Senato, da cui
otteniamo la lista dei tokens.

``` r
# Pulizia dataset

# La punteggiatura porta significato. Possiamo decidere di eliminarla, perdendo informazione.
# Eliminiamo specifici segni di punteggiatura, non quelli facenti parte dei nomi degli enti (come #vita, FB&Associati)

senato$NOMI <- gsub("[,;'\"()\\-]", " ", senato$NOMI) 

senato$NOMI <- gsub("\\s+", " ", senato$NOMI) # Rimuovi spazi multipli

# Creating a quanteda corpus
corpus <- corpus(senato, text_field = "NOMI")

# Tokenise text with preprocessing steps
tok <- corpus %>% tokens() %>% tokens_remove(stopwords('it')) # remove stopwords (Italian)
```

``` r
# Store the vectors in a list
list_vec <- list(v1=v1, v2=v2, v3=v3, v4=v4, v5=v5, v6=v6, v7=v7, v8=v8, v9=v9,
                 v10=v10, v11=v11, v12=v12, v13=v13, v15=v15, v20=v20, v22=v22,
                 v23=v23, v24=v24, v25=v25, v27=v27, v30=v30, v31=v31, v33=v33,
                 v36=v36, v37=v37, v38=v38, org_enti_local=org_enti_local,
                 forze_sicurezza=forze_sicurezza,gradi_militari=gradi_militari,
                 carica_ist=carica_ist, eu=eu, org_int=org_int, carica_neutra=carica_neutra,
                 str_neutra=str_neutra, dim_ist=dim_ist, geo=geo, uni_private=uni_private)

# Preprocessing
# 1. Staccare gli acronimi 

# Crea una tibble con due colonne: nomi e valori (per verificare quali segni di punteggiatura precedono gli acronimi)
unlist_vec <- unlist(list_vec) # Unisci la lista mantenendo i nomi
my_tibble <- tibble(name = names(unlist_vec), values = unlist_vec)

# trattino (hyphen), trattino medio (en dash), aperta parentesi

#Loop through each vector in the list
for (i in names(list_vec)) {
  # Extract the vector
  vec <- list_vec[[i]]
  # Split each element of the vector by hyphen or en dash + flat the split components
  new_vec <- unlist(strsplit(vec, " [-–\\(]"))
  # Update the list with the modified vector
  list_vec[[i]] <- new_vec
}

# 2. Rimuovere punteggiatura (eccetto segno *) e stopwords
list_vec <- lapply(list_vec, function(x) gsub("\\.", "", x))
list_vec <- lapply(list_vec, function(x) gsub("[[:punct:]&&[^*]]", " ", x))

vec_pulizia <- c("spa", "in liquidazione", "coatta amministrativa", "in breve", "società per azioni", "in sigla", "scpa", "arl")

for (i in vec_pulizia) {
  list_vec <- lapply(list_vec, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

stp_wrd <- stopwords('it')

# Pulire ogni vettore della lista per ogni elemento del vettore di pulizia come parola a sé stante
for (i in stp_wrd) {
  list_vec <- lapply(list_vec, function(x) {
    gsub(paste0("\\b", i, "\\b"), "", x)
  })
}

list_vec <- lapply(list_vec, function(x) gsub("\\s+", " ", trimws(x)))

# Function to remove empty elements
list_vec <- lapply(list_vec, function(x) x[x != ""])
```

## DIZIONARIO

Aggiungiamo al dizionario i vettori usati per descrivere le categorie.

``` r
dict <- dictionary(list(v1=list_vec[["v1"]],
                        v2=list_vec[["v2"]],
                        v3=list_vec[["v3"]],
                        v4=list_vec[["v4"]],
                        v5=list_vec[["v5"]],
                        v6=list_vec[["v6"]],
                        v7=list_vec[["v7"]],
                        v8=list_vec[["v8"]],
                        v9=list_vec[["v9"]],
                        v10=list_vec[["v10"]],
                        v11=list_vec[["v11"]],
                        v12=list_vec[["v12"]],
                        v13=list_vec[["v13"]],
                        v15=list_vec[["v15"]],
                        v20=list_vec[["v20"]],
                        v22=list_vec[["v22"]],
                        v23=list_vec[["v23"]],
                        v24=list_vec[["v24"]],
                        v25=list_vec[["v25"]],
                        v27=list_vec[["v27"]],
                        v30=list_vec[["v30"]],
                        v31=list_vec[["v31"]],
                        v33=list_vec[["v33"]],
                        v36=list_vec[["v36"]],
                        v37=list_vec[["v37"]],
                        v38=list_vec[["v38"]],
                        org_enti_local=list_vec[["org_enti_local"]],
                        forze_sicurezza=list_vec[["forze_sicurezza"]],
                        gradi_militari=list_vec[["gradi_militari"]],
                        carica_ist=list_vec[["carica_ist"]],
                        eu=list_vec[["eu"]],
                        org_int=list_vec[["org_int"]],
                        carica_neutra=list_vec[["carica_neutra"]],
                        str_neutra=list_vec[["str_neutra"]],
                        dim_ist=list_vec[["dim_ist"]],
                        geo=list_vec[["geo"]],
                        uni_private=list_vec[["uni_private"]]))
```

## ANALISI DEL TESTO

Analizziamo il testo usando il nostro dizionario. Raggruppiamo i
risultati per categoria in modo da snellire il conteggio.

``` r
# Apply the dictionary and construct the document-feature matrix
dfm_result <- tokens_lookup(tok, dictionary = dict, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- dfm_result %>% convert(to="data.frame") %>% as_tibble()

result <- result %>% mutate(length = ntoken(dfm_result))

class <- result %>% mutate(sogg_istituz = v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+
                             v12+v13+v15+v20+v22+v23+v24+v25+v27+v30+v31+v33+
                             v36+v37+v38+org_enti_local+forze_sicurezza+gradi_militari+
                             carica_ist+eu,
                           org_int=org_int,
                           neutral=carica_neutra+str_neutra,
                           dim_ist=dim_ist,
                           geo=geo,
                           uni_private = uni_private) %>%
  select(doc_id, sogg_istituz, org_int, neutral, dim_ist, geo, uni_private, length)

print(class, n=30)
```

    ## # A tibble: 7,086 × 8
    ##    doc_id sogg_istituz org_int neutral dim_ist   geo uni_private length
    ##    <chr>         <dbl>   <dbl>   <dbl>   <dbl> <dbl>       <dbl>  <int>
    ##  1 text1             0       0       0       0     0           0      2
    ##  2 text2             0       0       0       0     0           0      2
    ##  3 text3             0       0       0       0     0           0      2
    ##  4 text4             2       0       0       0     0           0      3
    ##  5 text5             0       0       0       0     0           0      4
    ##  6 text6             0       0       0       0     0           0      3
    ##  7 text7             1       0       0       0     0           0      1
    ##  8 text8             1       0       0       0     0           0      1
    ##  9 text9             1       0       0       0     0           0      1
    ## 10 text10            0       0       0       0     0           0      1
    ## 11 text11            0       0       0       0     0           0      1
    ## 12 text12            0       0       0       0     0           0      1
    ## 13 text13            0       0       0       0     0           0      1
    ## 14 text14            0       0       0       0     0           0      1
    ## 15 text15            0       0       0       0     1           0      3
    ## 16 text16            0       0       0       0     0           0      1
    ## 17 text17            0       0       0       0     0           0      1
    ## 18 text18            0       0       0       0     0           0      1
    ## 19 text19            0       0       1       0     0           0      5
    ## 20 text20            0       1       0       0     0           0      1
    ## 21 text21            0       0       0       0     0           0      1
    ## 22 text22            0       0       0       0     0           0      1
    ## 23 text23            0       0       0       0     0           0      1
    ## 24 text24            0       0       0       0     0           0      1
    ## 25 text25            0       0       0       0     0           0      1
    ## 26 text26            0       0       0       0     0           0      1
    ## 27 text27            0       0       0       0     0           0      1
    ## 28 text28            0       0       0       0     0           0      3
    ## 29 text29            0       0       0       0     0           0      1
    ## 30 text30            0       0       0       0     0           0      1
    ## # ℹ 7,056 more rows

A questo punto, basterà creare un indice per determinare la categoria
attesa.
