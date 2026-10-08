library(tidyverse)
library(quanteda)

# Manutenzione dataset

# Obiettivi:
# 1. Correggere gli errori (battitura - typos), ambiguità (acronimi uguali per enti diversi).
# 2. Eliminare righe superflue
# 3. Standardizzare la nomenclacura dei singoli stakeholders (abbreviazioni, acronimo-nome esteso)

# Gli errori e le ambiguità sono state appuntate strada facendo nella classificazione.

# Disclaimer!
# Dopo la correzione, bisogna ripetere le operazioni di pulizia dei nomi.

# Strategia in aiuto per punto 3: metodo del coseno (rinviata)


senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/clean_data/dataset_senato.csv")


senato[5824,2] <- "associazione italiana diabetici" # rimosso acronimo in comune
senato[546,2] <- "tavolo permanente delle federazioni bandistiche italiane"
senato[4740,2] <- "associazioni dei consumatori ; altroconsumo ; (unc) unione nazionale consumatori ; associazione mo bast!"
senato[5367,2] <- "transport & environment"
senato[7004,2] <- "associazione italiana promozione scienza aperta"
senato[732,2] <- "associazione italiana promozione scienza aperta"
senato[43,2] <- "associazione docenti e dirigenti scolastici italiani"
senato[1045,2] <- "assemblee di dio in italia"
senato[237,2] <- "actionaid - ondata - transparency international italia"
senato[82,2] <- "fondazione promozione sociale onlus"
senato[2691,2] <- "comitato apprendimento pratico musica studenti (cnapm) ; conferenza direttori conservatori musica"
senato[5719,2] <- "legambiente - pro natura - comitato vigilanza sul nucleare del piemonte"
senato[7085,2] <- "associazione air italia agenti immobiliari riuniti"
senato[2967,2] <- "associazione produttori esecutivi"
senato[885,2] <- "(angdp) associazione nazionale giudici di pace ; (unagipa) unione nazionale giudici di pace"
senato[1442,2] <- "(arti) agenzia regionale tecnologia e innovazione"
senato[6302,2] <- "associazione medici diabetologi (amd) - società italiana diabetologia (sid)" #congiunta
senato[39,2] <- "associazione nazionale docenti (and)"
senato[3266,2] <- "associazione nazionale esercenti multiplex (anem) ; federazione carta grafica"
senato[4637,2] <- "nuova associazione bieticoltori italiani"
senato[3009,2] <- "associazione reti teatrali italiane (arti)"
senato[2749,2] <- "consiglio universitario nazionale (cun) ; consiglio nazionale studenti universitari (cnsu)"
senato[4480,2] <- "assoferilizzanti - federchimica"
senato[1139,2] <- "centro studi internazionali"
senato[3577,2] <- "presidente commissione tecnica tranvie e sistemi ferroviari istituita presso l'ordine degli ingegneri della provincia di roma"
senato[4640,2] <- "confederazione agromeccanici e agricoltori italiani"
senato[2035,2] <- "nestlé italiana spa"
senato[4609,2] <- "federazione apicoltori italiani"
senato[6731,2] <- "fondo ambiente italiano - inu - slow food italia - wwf"
senato[2576,2] <- "conferenza delle regioni e delle province autonome"
senato[4049,2] <- "conferenza delle regioni e delle province autonome"
senato[1149,2] <- "alessandro orsini direttore osservatorio sulla sicurezza internazionale luiss"
senato[2114,2] <- "fabrizia lapecorella direttore dipartimento finanze"
senato[1034,2] <- "maria luisa stasi, article 19"
senato[900,2] <- "avv maria sabina lembo"
senato[2874,2] <- "associazione a dj"
senato[1148,2] <- "comitato di collegamento di cattolici per una civiltà dell'amore onlus"
senato[2931,2] <- "lavoratori della danza"
senato[3092,2] <- "suor anna monia alfieri, il diritto di apprendere"
senato[2974,2] <- "giffoni film festival"
senato[5392,2] <- "(anbba) associazione nazionale bed & breakfast affittacamere case per vacanza locazioni turistiche"
senato[5462,2] <- "(anbba) associazione nazionale bed & breakfast affittacamere case per vacanza locazioni turistiche"
senato[6591,2] <- "consult@noi"
senato[6700,2] <- "rén collective"
senato[4761,2] <- "free2move esolutions"
senato[5857,2] <- "direttore generale della direzione generale dei rapporti lavoro delle relazioni industriali del ministero del lavoro e delle politiche sociali"
senato[4483,2] <- "coldiretti - fondo ambiente italiano - inu - legambiente - lipu - slow food italia - touring club italiano - wwf"
senato[7020,2] <- "100 autori"
senato[3286,2] <- "associazione professioni pedagogiche app ; associazione pedagogisti educatori italiani apei"
senato[29,2] <- "cgil"
senato[1059,2] <- "arca 2000"
senato[1266,2] <- "alleanza cooperative italiane"
senato[1296,2] <- "confindustria alberghi"
senato[1450,2] <- "marco esposito"
senato[1488,2] <- "confindustria alberghi"
senato[1766,2] <- "confagricoltura"
senato[2014,2] <- "assoviaggi confesercenti"
senato[1821,2] <- "confesercenti"
senato[1879,2] <- "anitrav"
senato[2291,2] <- "confindustria servizi hcfs"
senato[2748,2] <- "conferenza direttori conservatori musica"
senato[2860,2] <- "coopculture"
senato[2872,2] <- "parchi permanenti italiani"
senato[2873,2] <- "movimento facciamo la conta"
senato[3001,2] <- "coordinamento la musica che gira"
senato[3369,2] <- "alleanza cooperative italiane"
senato[3634,2] <- "ministro innovazione tecnologica transizione digitale vittorio colao"
senato[3637,2] <- "trasporto unito"
senato[2970,2] <- "ifc italian film commission"
senato[3362,2] <- "federazione italiana sport equestri"
senato[4147,2] <- "unci agroalimentare"
senato[4813,2] <- "movimento nazionale liberi farmacisti federazione nazionale parafarmacie italiane federazione farmacisti disabilità onlus confederazione unitaria libere parafarmacie italiane"
senato[5093,2] <- "conferenza regioni province autonome"
senato[5218,2] <- "alleanza fotovoltaico italia"
senato[5458,2] <- "base balneare donnedamare"
senato[6759,2] <- "associazione italiana compostaggio"
senato[6786,2] <- "re te imprese italia"
senato[6837,2] <- "confartigianato"
senato[7024,2] <- "agcom"
senato[7027,2] <- "iliad"
senato[7028,2] <- "cfwa coalizione fixed wireless access"
senato[3217,2] <- "professor luca serianni"
senato[4484,2] <- "fondo italiano ambiente"
senato[6988,2] <- "tavolo autoconsumo efficienza energetica"
# Rimozione righe
senato <- senato[-c(4580,3578,4484:4490,5393:5395,5463:5465,5119,7003,5858), ]
rownames(senato) <- NULL

senato <- senato %>% separate_rows(NOMI, sep = ";")

# Rimuoviamo gli altri segni di punteggiatura

senato$NOMI_clean <- gsub("\\.", "", senato$NOMI)
senato$NOMI_clean <- gsub("[,;'\"()\\-]", " ", senato$NOMI_clean) 
senato$NOMI_clean <- gsub("[/’_]", " ", senato$NOMI_clean) # aggiunto

senato$NOMI_clean <- gsub("\\s+", " ", senato$NOMI_clean) # Rimuovi spazi multipli


# Remove stopwords
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

senato <- as.data.frame(senato)

senato[6579,5] <- "consult@noi"
senato[2960,5] <- "doc/it"
senato[746,5] <- "di donna"
senato[773,5] <- "siamo così"
senato[982,5] <- "la malfa"
senato[255,5] <- "lorenzo calò il mattino"
senato[3344,5] <- "movimento facciamo la conta"
senato[2875,5] <- "movimento facciamo la conta"
senato[2876,5] <- "associazione a dj"
senato[2985,5] <- "associazione i jazz"
senato[3331,5] <- "associazione i jazz"
senato[2990,5] <- "movimento spettacolo vivo" #
senato[3003,5] <- "coordinamento la musica che gira"
senato[3479,5] <- "coordinamento la musica che gira"
senato[5805,5] <- "io sono oss"

# salvo il dataset corretto
write.csv(senato, "C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_fix.csv", row.names = FALSE)



# APPLY THE DICTIONARY

# carica senato
senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_fix.csv")

# combino i volumi del dizionario
dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")
dict10 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")

dizionario <- c(dizionario_pt1, dizionario_babele, dict10)


# text analysis

corpus <- corpus(senato, text_field = "NOMI_clean")
tok <- tokens(corpus)

dfm_result <- tokens_lookup(tok, dictionary = dizionario, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()

result <- as.tibble(dfm_result)

result <- result %>% mutate(length = ntoken(dfm_result),
                            sum = privati+istituzioni+soc_civile+rappr+centr_ricerca+altro+esperti+part_statali) %>%
  select(privati, istituzioni, soc_civile, rappr, centr_ricerca, altro, esperti, part_statali, length, sum)

testo_ricomposto <- sapply(tok, function(x) paste(x, collapse = " "))
result$text <- testo_ricomposto


# creo le etichette

result <- result %>%
  rowwise() %>%
  mutate(
    values = list(c(istituzioni, part_statali, rappr, soc_civile, esperti, centr_ricerca, privati, altro)),
    labels = list(c("istituzioni", "partecipate", "sindacati_e_organiz_di_rappr", "società_civile", "esperti", "centri_ricerca", "privati", "altro")),
    
    max_val = max(values), #quante corrispondenze ha avuto la categoria più rappresentata nella riga
    n_max = sum(values == max_val), #quante volte compare il numero massimo nella riga
    
    label = if (max_val == 0) {
      "NC"
    } else if (n_max > 1) {
      "pareggio"
    } else {
      labels[which.max(values)]
    }
  ) %>%
  ungroup() %>%
  select(istituzioni, part_statali, rappr, soc_civile, esperti, centr_ricerca, privati, altro, length, sum, text, label) 


# label in dataset
senato$LABEL <- result$label

# salvo il dataset con le label
write.csv(senato, "C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_label.csv", row.names = FALSE)











 

# Checkpoint
senato <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/senato/senato_label.csv")

senato <- senato[-5727, ] # duplicato alitalia

# ci sono altre stringhe da rinominare (per acronimi ambigui) e da splittare (perché ospitano più attori)
senato[553,2] <- "alleanza delle cooperative italiane"

# split 4285

# remove 2925

# 2881, fima è acronimo in comune, meglio forma estesa: federazione italiana mercanti arte





# Creazione id_audizione
senato$hearing_id <- paste(senato$COMMISSIONE, senato$DATA, senato$ATTO, sep = "_")

# Variabile temporale: T1-T2-T3
# Governo Conte I  (dal 01/06/2018 al 04/09/2019)
# Governo Conte II (dal 05/09/2019 al 13/02/2021)
# Governo Draghi   (dal 13/02/2021 al 22/10/2022)

# La variabile DATA è di tipo character; bisogna trasformarla in Date prima di classificare per range.

senato$DATA <- as.Date(senato$DATA)
class(senato$DATA)

senato$GOVERNMENT <- cut(
  senato$DATA,
  breaks = as.Date(c("2018/06/01", "2019/09/05", "2021/02/13", "2022/10/22")),
  labels = c("Conte_I", "Conte_II", "Draghi"),
  right = FALSE # è il discrimine per estremo compreso o escluso
)

# classificazione policy acts ed è fatta!












# Assegnazione categoria agli attori in "pareggio"

# Punto di partenza: il risultato dell'applicazione del dizionario

categorie <- c("istituzioni", "part_statali", "rappr", "soc_civile", "esperti", "centr_ricerca", "privati", "altro")

pareggi <- result %>%
  filter(label == "pareggio") %>%
  rowwise() %>%
  mutate(
    labels = paste(
      categorie[c_across(all_of(categorie)) > 0],
      collapse = " ; "
    )
  ) %>%
  ungroup() %>%
  select(text, labels)

pareggi <- unique(pareggi)


# Professori universitari - Esperti

pareggi <- pareggi %>%
  mutate(label = if_else(
    str_detect(text, regex("prof", ignore_case = TRUE)) &
      str_detect(text, regex("università|politecnico", ignore_case = TRUE)),
    "esperti",
    ""
    )) #60

# Pareggi tra istituzioni - partecipazioni statali sono partecipazioni


pareggi %>%
  filter(
    label != "esperti",
    labels == "istituzioni ; part_statali"
  ) %>% print(n = Inf)

# Sovrapposizioni da correggere in dizionario.

# Rimuovere dalla voce "istituzioni".

# "gestore servizi energetici", "gse", "sogin", "sogei", "società gestione impianti nucleari", "anas",
# "rete ferroviaria italiana", "rfi", "rai", "sport salute", "sose", "consip"
# "cgs"

# Rimuovere dalla voce "rappr"
# "camera commercio"


# Casi da valutare
# sviluppumbria, astral = agenzia regionale, aziende pubbliche?
# unioncamere
# consiglio nazionale giovani cng
# presidente consiglio nazionale ordine consulenti lavoro
# ispra = istituzione o centr_ricerca? esperti

# Refusi da splittare:
# associazione nazionale allevatori suini anas
# ministero interno federfarma



pareggi <- pareggi %>%
  mutate(
    label = ifelse(
      label != "esperti" &
        labels == "istituzioni ; part_statali" &
        !str_detect(text, "astral|sviluppumbria|associazione nazionale allevatori suini anas"),
      "partecipate",
      label
    )
  )


# commissario straordinario = governo - istituzione
pareggi <- pareggi %>%
  mutate(label = if_else(
    str_detect(text, "commissario straordinario"),
    "istituzioni", #4
    label
  ))

# Pareggi tra Istituzioni e organizzazioni di rappresentanza

pareggi %>%
  filter(label == "",
         labels == "istituzioni ; rappr") %>%
  print (n = Inf)

pareggi <- pareggi %>%
  mutate(
    label = case_when(
      label == "" & labels == "istituzioni ; rappr" &
        str_detect(text, "confederazione regioni province autonome|camera commercio|ocse") ~ "istituzioni",
      
      label == "" & labels == "istituzioni ; rappr" &
        str_detect(text, "centro studi") ~ "centr_ricerca",

      label == "" & labels == "istituzioni ; rappr" &
        !str_detect(text, "ministero interno federfarma|unioncamere|cng|presidente consiglio nazionale ordine consulenti lavoro") ~ "rappr",
      
      TRUE ~ label
    )
  )


# Pareggi tra istituzioni ; esperti

sub1 <- pareggi %>%
  filter(label == "" &
           labels == "istituzioni ; esperti") %>%
  select(text)
sub1$label <- ""
      
vec_ist <- "ministro|sindaco|ministero|mipaaf|corte costituzionale|vigili fuoco|dipartimento amministrazione penitenziaria|dipartimento finanze|dipartimento politiche europee|direzione generale|region|comune|corte conti|tribunale|consob|anac|garante protezione dati personali|commissione europea|banca d italia|upb|agenzia entrate|parco archeologico"

# ente istituzionale o para-istituzionale con potenziale expertise
ist_exp <- "usl|asl|cnr|consiglio nazionale ricerche|crea|iss|istituto superiore sanità|ispra|izs|aifa|agea|ismea|ersa|comitato nazionale bioetica|istituto zooprofilattico|policlinico|comitato edufin|dg connect"

vec_rappr <- "unione fori|anvu|anpci"


ruolo_esperto <- "esperto|ricercatrice|\\bingegner\\b|medico|\\barch\\b|alte professionalità|membro" 
ruolo_dirig <- "presidente|direttore|direzione|responsabile" 

sub1 <- sub1 %>%
  mutate(
    has_vec_ist = str_detect(text, regex(vec_ist, ignore_case = TRUE)),
    has_ist_exp = str_detect(text, regex(ist_exp, ignore_case = TRUE)),
    has_vec_rappr = str_detect(text, regex(vec_rappr, ignore_case = TRUE)),
    has_ruolo_esperto = str_detect(text, regex(ruolo_esperto, ignore_case = TRUE)),
    has_ruolo_dirig = str_detect(text, regex(ruolo_dirig, ignore_case = TRUE)),
    
    expertise_flag = if_else(
      has_ruolo_esperto |
        (has_ruolo_dirig & has_ist_exp) |
        (has_ruolo_esperto & has_vec_ist) |
        (has_vec_ist & has_ist_exp) |
        (has_ist_exp &
           !has_vec_ist &
           !has_vec_rappr &
           !has_ruolo_esperto &
           !has_ruolo_dirig),
      1,
      0
    ),
    
    label = case_when(
      has_vec_rappr ~ "rappr",
      
      has_ruolo_dirig & has_ist_exp ~ "istituzioni",
      
      has_ruolo_esperto & has_vec_ist ~ "istituzioni",
      
      has_ist_exp &
        !has_vec_ist &
        !has_vec_rappr &
        !has_ruolo_esperto &
        !has_ruolo_dirig ~ "istituzioni",
      
      has_ruolo_esperto ~ "esperti",
      
      !has_ruolo_esperto & has_vec_ist ~ "istituzioni",
      
      TRUE ~ ""
    )
  )

# Correzione manuale: crea (ruolo scientifico) -> esperti
sub1 <- sub1 %>% mutate(
  label = if_else(
    str_detect(text, "crea"), "esperti",
    label))
    




# Riportiamo i risultati in dataset pareggi, aggiorniamo variabili

sub1 <- sub1 %>% select(text, label, expertise_flag)

pareggi <- pareggi %>% 
  left_join(
    sub1 %>%
      select(text, label_sub = label, expertise_flag),
    by = "text"
  ) %>%
  mutate(
    label = if_else(!is.na(label_sub), label_sub, label)
  ) %>%
  select(text, labels, label, expertise_flag)


# carica di ambasciatore = istituzione + expertise se centro ricerca "ispi"

pareggi <- pareggi %>%
  mutate(
    label = case_when(
      str_detect(text, "\\bamb\\b|ambasciatore") ~ "istituzioni",
      TRUE ~ label
    ),
    
    expertise_flag = if_else(
      str_detect(text, "\\bamb\\b|ambasciatore") &
        str_detect(text, "ispi"),
      1,
      expertise_flag
    )
  )

# PARTE 2

pareggi2 <- pareggi %>% filter(label=="") # da qui su pareggi2

# Le fondazioni in pareggio
fond_exp <- c("gimbe", "fondazione ricerca salute")
fond_rappr <- c("fondazione sviluppo sostenibile") # expertise

# Pareggi istituzioni ; part_statali

pareggi2 %>% filter(str_detect(labels, "istituzioni ; part_statali")) %>% select(text)

ente_region <- c("astral", "sviluppumbria") # istituzioni
partecipate <- c("sport salute", "rfi", "anas spa", "gse", "società gestione impianti nucleari spa")


# commissari straordinari -> governo (istituzioni)


# Pareggi rappr ; esperti

rappr <- pareggi2 %>% filter(str_detect(labels, "rappr ; esperti")) %>% select(text) %>% print(n = Inf)

# Sottotipi di rappresentanza:

# 1. Sindacati / rappresentanza del lavoro (trade unions)
# 2. Ordini e associazioni professionali (professional associations)
# 3. Associazioni datoriali / categoria economica (business associations)

trade_unions <- rappr$text[c(9,12,13,14,15,25,27,29)]
# cida, confsal, llp, anaao assomed, uil

prof_asso <- rappr$text[c(1,2,3,4,7,8,11,16,17,24,26,28,31)]
# uncat, aicg, cndcec, consiglio nazionale dottori commercialisti, rpt, rete professioni tecniche,
# anai, anp,  fidaf, tsrm pstrp, assoprofessioni

business_asso <- rappr$text[c(5,6,10,18,19,21,22,23,26,30)]
# assonime, confedilizia, forma, asstel, anitec assinform, confindustria, fedagromercati, coldiretti, oice

# Esperti - consorzio interuniversitario
experts <- "cini"


# Pareggi - società civile

soc_civile <- pareggi2 %>% filter(str_detect(labels, "soc_civile")) %>% select(text, labels) %>% arrange(labels) %>% print(n = Inf)

# Società civile - istituzioni

# altroconsumo-cncu; legambiente-uni: si riferiscono a documenti sottoscritti da più attori.
# facciamo prevalere la funzione del documento comune -> società civile

società_civile <- soc_civile$text[c(1,2,3,5,10,11,12,17,18,19,23,24)]

istituzioni_exp <- soc_civile$text[4]
# irccs - ente pubblico di ricerca

categoria <- soc_civile$text[c(6,8,21)]
# fima (business_asso), aic (acronimo comune, qui coltivatori - business_asso), ncc donne

esperti <- soc_civile$text[c(15,16,20,22)]
# sifo, adapt

# 7 e 9 da splittare
pareggi2 <- pareggi2 %>%
  mutate(
    label = case_when(
      str_detect(text, str_c(c(fond_exp, experts, esperti), collapse = "|")) ~ "esperti",
      str_detect(text, str_c(c(società_civile), collapse = "|")) ~ "società civile",
      str_detect(text, str_c(c(ente_region,istituzioni_exp,"commissari straordinari"), collapse = "|")) ~ "istituzioni",
      str_detect(text, str_c(c(fond_rappr,trade_unions,prof_asso,business_asso,categoria), collapse = "|")) ~ "rappresentanza",
      str_detect(text, str_c(c(partecipate), collapse = "|")) ~ "partecipate",
      TRUE ~ ""
    ),
    expertise_flag = ifelse(
      str_detect(text, str_c(c(fond_exp, experts, esperti,fond_rappr,istituzioni_exp), collapse = "|")), 1,
      0
    )
  )

# PARTE 3

# Pareggi partecipate ; privati

pareggi2 %>% filter(str_detect(labels, "part_statali"), label=="") %>% select(text, labels) %>% print(n = Inf)

partecipate <- c(partecipate, "pagopa", "sace", "consap", "italia trasportoaereo", "trenitalia", "poste italiane",
                 "enav", "adr", "rete ferroviaria italiana", "enel", "snam", "terna")



last <- pareggi2 %>% filter(label=="") %>% select(text)

istituzioni <- c("unioncamere", "cng", "consiglio nazionale giovani", "consorzio bonifica toscana nord")

esperti <- c(esperti, last$text[c(5,17,18)])
# aogoi, sigo # società scientifiche

categoria <- c(categoria, "federmep", "tavolo autoconsumo efficienza energetica", "consiglio nazionale ordine consulenti lavoro", "adepp")
# federmep, tavolo autoconsumo efficienza energetica (business_asso) ; ordine consulenti lavoro, adeep (prof_asso)

civile <- "osservatorio malattie rare omar" # expertise

privati <- "search on media group"

pareggi2 <- pareggi2 %>%
  mutate(
    label = case_when(
      str_detect(text, str_c(c(esperti), collapse = "|")) ~ "esperti",
      str_detect(text, str_c(c(istituzioni), collapse = "|")) ~ "istituzioni",
      str_detect(text, str_c(c(categoria), collapse = "|")) ~ "rappresentanza",
      str_detect(text, str_c(c(civile), collapse = "|")) ~ "società civile",
      str_detect(text, str_c(c(partecipate), collapse = "|")) ~ "partecipate",
      str_detect(text, str_c(c(privati), collapse = "|")) ~ "privati",
      TRUE ~ label
    ),
    expertise_flag = ifelse(
      str_detect(text, str_c(c(esperti, "tavolo autoconsumo efficienza energetica", civile), collapse = "|")), 1,
      expertise_flag
    )
  )


# bisogna trasferire label in pareggi 2 in pareggi
# split rows con nomi aggregati + classificazione
# individuare documenti co-firmati su nomi sporchi
# aggiorna grafico


# 1.

pareggi <- pareggi %>% 
  left_join(
    pareggi2 %>%
      select(text, label2 = label, expertise_flag2 = expertise_flag),
    by = "text"
  ) %>%
  mutate(
    label = if_else(!is.na(label2), label2, label),
    expertise_flag = if_else(!is.na(expertise_flag2), expertise_flag2, expertise_flag)
  ) %>%
  select(-label2, -expertise_flag2)

pareggi <- pareggi %>%
  mutate(
    expertise_flag = if_else(label=="esperti", 1, expertise_flag),
    expertise_flag = if_else(is.na(expertise_flag), 0, expertise_flag),
    label = if_else(label=="rappr", "rappresentanza", label)
  )

# Save data
write.csv(pareggi, "C:/Users/SImone/Desktop/dati_progetto/pareggi.csv", row.names = FALSE)



senato1 <- senato %>% 
  left_join(
    pareggi %>%
      select(text, label, expertise_flag),
    by = c("NOMI_clean" = "text")
  ) %>%
  mutate(
    LABEL = if_else(!is.na(label), label, LABEL),
    expertise_flag = if_else(!is.na(expertise_flag), expertise_flag, NA)
  ) %>%
  select(-label)


senato1 <- senato1 %>% 
  left_join(
    cent_res1 %>%
      select(text, label1 = label, expertise_flag1 = expertise_flag),
    by = c("NOMI_clean" = "text")
  ) %>%
  mutate(
    LABEL = if_else(!is.na(label1), label1, LABEL),
    expertise_flag = if_else(!is.na(expertise_flag1), expertise_flag1, expertise_flag)
    ) %>%
  select(-label1, -expertise_flag1)

table(senato1$LABEL)
table(senato$LABEL)

senato1$LABEL <- ifelse(senato1$LABEL=="società_civile", "società civile",
                        ifelse(senato1$LABEL=="rappresentanza", "categoria",
                               ifelse(senato1$LABEL=="sindacati_e_organiz_di_rappr", "categoria",
                                      senato1$LABEL)))



to_split <- senato1[c(1022,2401,3499,4381,4810), ]
# in realtà non sono da splittare, ma docu unitari di più organizzazioni
senato1[1022, 6] <- "istituzioni"
senato1[2401, 6] <- "esperti"
senato1[3499, 6] <- "società civile"
senato1[4381, 6] <- "categoria"
senato1[4810, 6] <- "categoria"
# Numero osservazioni/documenti/memorie congiunte/co-firmate?

# Andiamo a vedere cosa c'è in label vuote, 1 centr_ricerca e i 24 Altro.
labels_to_check <- senato1 %>% filter(LABEL %in% c("", "altro", "centr_ricerca"))

# fix mispelling
senato1[282,5] <- "franca biglio, anpci"
senato1[737,5] <- "altroconsumo"

labels_to_check <- senato1 %>% filter(LABEL %in% c("", "altro", "centr_ricerca"))

categoria <- c("anpci", "centro studi cna marche")
civile <- c("sma", "altroconsumo", "ucoii", "chiesa apostolica italia", "assemblea rabbini", "arcidiocesi orotdossa d italia malta",
            "chiesa cristiana universale nuova gerusalemme", "assemblee dio italia", "ucid", "diritto apprendere",
            "cultura cattolica", "cei")
esperti <- labels_to_check$NOMI_clean[c(5,6)]
altro <- labels_to_check$NOMI_clean[c(13:21)]


labels_to_check <- labels_to_check %>%
  mutate(
    LABEL = case_when(
      str_detect(NOMI_clean, str_c(c(esperti), collapse = "|")) ~ "esperti",
      str_detect(NOMI_clean, str_c(c(categoria), collapse = "|")) ~ "categoria",
      str_detect(NOMI_clean, str_c(c(civile), collapse = "|")) ~ "società civile",
      str_detect(NOMI_clean, str_c(c(altro), collapse = "|")) ~ "altro",
      TRUE ~ LABEL
    ),
    expertise_flag = ifelse(
      str_detect(NOMI_clean, str_c(c(esperti), collapse = "|")), 1,
      0
    )
  )

senato1 <- senato1 %>% 
  left_join(
    labels_to_check %>%
      select(text = NOMI_clean, label2 = LABEL, expertise_flag2 = expertise_flag),
    by = c("NOMI_clean" = "text")
  ) %>%
  mutate(
    LABEL = if_else(!is.na(label2), label2, LABEL),
    expertise_flag = if_else(!is.na(expertise_flag2), expertise_flag2, expertise_flag)
  ) %>%
  select(-label2, -expertise_flag2)

table(senato1$LABEL)

write.csv(senato1, "C:/Users/SImone/Desktop/dati_progetto/senato1.csv", row.names = FALSE)








# CENTRI RICERCA #

# Riallochiamo i centri di ricerca per armonizazzione tassonomia

pareggi %>% filter(label=="", str_detect(labels, "centr_ricerca")) #post

cent_res <- result %>% filter(label=="centri_ricerca") %>% select(text) %>% unique()

# SOCIETA' SCIENTIFICHE
soc_scient_ita <- cent_res %>% filter(str_detect(text, "società")) %>% pull(text) %>% unname()

soc_scient_ita <- soc_scient_ita[c(1,2,5,6,7,8,9,13,14,15,16,18,19,20,21,22)]
soc_scient_ita <- c(soc_scient_ita, "società chimica italiana", "siped", "sisfa") # esperti con expertise (ovv)

org_rappr <- c("fisv", "simef") # expertise
org_rappr_no_exp <- "siae" # no expertise


# Scuole private
# Le business school e scuole di formazione vengono classificate come esperti se la stringa le presenta come
# soggetti formativi/di ricerca; vengono classificate come privati se prevale la natura di impresa commerciale.

school_exp <- c("luiss", "fondazione ipe")

# accademia italiana economia aziendale aidea

# Sottotipo Osservatori di policy
# Osservatori - esperti o società civile con expertise
osservatori <- cent_res %>% filter(str_detect(text, "osservatorio")) %>% pull(text) %>% unname()

osservatori_exp <- c("osservatorio sicurezza internazionale", "osservatorio conti pubblici", "osservatorio attuazione codice contratti pubblici italiadecide") #esperti
osservatori_civile <- c("omar", "osservatorio malattie rare") # expertise flag


# Think tank - esperti

think_tank <- c("fondazione agnelli", "euricse", "eurispes", "nodo gordio", "censis", "svimez",
  "centro studi internazionali", "centro studi politica internazionale", "cespi", "centro studi stasa", 
  "demetra centro studi", "centro studi economia reale", "centro studi procedure esecutive concorsuali cespec", "centro studi promotor",
  "istituto affari internazionali", "iai", "istituto bruno leoni", "istituto studi politica internazionale", "ispi",
  "carbon tracker initiative", "astril", "associazione tortuga", "associazione treellle", "itinerari previdenziali")

# il pattern "centro studi" funziona bene sui ns dati


# Le fondazioni in centri di ricerca

# Fondazione agnelli, fondazione ipe business school -solved

fond_ist <- "fondazione montagna sicura" # expertise
fond_rappr <- c(fond_rappr,"fondazione studi consulenti lavoro")# expertise
fond_exp <- c(fond_exp,"endisu") # expertise

# Media / testate specialistiche

# Testate specialistiche di policy/settore: info settoriale tecnica, dati, analisi di policy
# Le testate specialistiche di settore sono classificate come esperti, con expertise_flag = TRUE, quando la loro presenza è riconducibile
# alla produzione di conoscenza settoriale o analisi di policy.

news_exp <- c("limes", "rivista nuova secondaria", "mondo digitale", "tuttoscuola", "quotidiano energia", "automotive news europe", "vfr aviation")

# Media civici/advocacy/issue-based: parlano per una causa, gruppo sociale (pazienti, consumatori, terzo settore)
# Le testate o piattaforme informative legate a specifiche cause, gruppi sociali o interessi diffusi sono classificate come società_civile,
# con expertise_flag = TRUE solo quando producono conoscenza specializzata sul settore o sulla constituency di riferimento.

news_civile <- c("rivista salvagente", "repubblica stagisti")

# Media commerciali/broadcaster/piattaforme
# I broadcaster, piattaforme audiovisive e media commerciali, quando intervengono come operatori del mercato mediale/audiovisivo,
# sono classificati come privati (imprese).

media_prvt <- c("mediaset", "sky", "la7", "discovery", "viacom", "paramount global", "netflix", "prime video", "chili", "weshort", "flyeurope tv")

# Editoria privata
editoria_prvt <- c("terre mezzo editore", "lafeltrinelli", "epc")

# Telecomunicazioni private
telecom_prvt <- c("fastweb", "iliad")

# Altri esperti/professionisti
other_exp <- "lorenzo calò il mattino" # giornalista


# Parte 2

# Società scientifiche / associazioni disciplinari -> esperti

# Promuovono conoscenza, ricerca, cultura disciplinare e policy expertise

soc_scient_ita <- c(soc_scient_ita, "sidrea", "siedas", "aidap", "aidea", "associazione italiana intelligenza artificiale", "istea", "associazione italiana aracnologia",
                    "simg", "comitato glaciologico italiano", "inu", "istituto nazionale urbanistica", "associazione economia cultura")

# Centri sperimentali -> esperti

center_speriment <- c("olimpolli montagnani", "cersaa albenga")


# Think tank / policy research / centri studi -> esperti

# Producono analisi, proposte di policy, ricerca applicata o expertise settoriale

think_tank <- c(think_tank, "triageduepuntozero", "aware", "competere policies for sustainable development", "centro nazionale studi tartufo", "cetri tires")


# Enti di ricerca e formazione -> istituzioni

ent_naz_res <- c("stazione zoologica anton dohrn", "garr") # expertise


# Società civile / advocacy civica o ambientale

asso_civile <- "gruppo intervento giuridico" # expertise

# Approccio legale alla tutela ambientale. 


# Testate settore / informazione specialistica -> esperti

news_exp <- c(news_exp, "staffetta quotidiana") # info specializzata energia


# Rappresentanza tecnico-settoriale / coalizioni / cluster -> rappresentanza 

# Soggetti che aggregano stakeholder, filiere, categorie o reti settoriali. Producono anche conoscenza, ma la funzione prevalente è rappresentativa.

net_rappr <- c("cluster tecnologico nazionale energia", "ctne", "#vita", "vita valore innovazione terapie avanzate", "tavolo autoconsumo efficienza energetica", "centro nazionale studi tartufo") # expertise
ord_prof <- "ordine nazionale consulenti lavoro" # no exp


# Privati / imprese / operatori commerciali

# Istituto privato di studi / consulenza ; scuola professionale privata

ent_prvt <- c("scenari immobiliari", "alma srl scuola internazionale cucina italiana", "24ore business school") # expertise


cent_res1 <- cent_res %>%
  mutate(
    label = case_when(
      str_detect(text, str_c(c(soc_scient_ita, school_exp, osservatori_exp, think_tank, fond_exp, news_exp, other_exp, center_speriment), collapse = "|")) ~ "esperti",
      str_detect(text, str_c(c(osservatori_civile, news_civile, asso_civile), collapse = "|")) ~ "società civile",
      str_detect(text, str_c(c(fond_ist, ent_naz_res), collapse = "|")) ~ "istituzioni",
      str_detect(text, str_c(c(org_rappr, org_rappr_no_exp, fond_rappr, net_rappr, ord_prof), collapse = "|")) ~ "rappresentanza",
      str_detect(text, str_c(c(media_prvt, editoria_prvt, telecom_prvt, ent_prvt), collapse = "|")) ~ "privati",
      TRUE ~ ""
    ),
    expertise_flag = ifelse(
      str_detect(text, str_c(c(soc_scient_ita, school_exp, osservatori_exp, think_tank, fond_exp, news_exp, other_exp, center_speriment, osservatori_civile, asso_civile, fond_ist, ent_naz_res, org_rappr, fond_rappr, net_rappr, net_rappr), collapse = "|")), 1,
      0
    )
  )

# Save data
write.csv(cent_res1, "C:/Users/SImone/Desktop/dati_progetto/centri_ricerca.csv", row.names = FALSE)





































# SENATO fix
# Fondazioni senato

fond_sen <- senato %>% filter(str_detect(NOMI, "fondazione"), !str_detect(LABEL, "pareggio|centri_ricerca")) %>% select(NOMI, LABEL) %>% distinct()

# Controllo e riclassificazione

fond_exp <- c(fond_esp, "italia digitale", "astrid", "fondazione centro studi doc")
fond_rappr <- c(fond_rappr, "fondazione inarcassa", "fondazione nazionale ricerca commercialisti", "fondazione una") #expertise
fond_rappr_no_exp <- "fondazione platea"
fond_civile <- c("fondazione univerde", "fondazione scuola compagnia san paolo", "fondazione macula", "fondazione maddalena grassi", "fondazione telethon", "fondazione migrantes", "fondazione carolina") #expertise
fond_civile_no_exp <- c("fondazione promozione sociale", "fondazione bellisario", "fondazione de sanctis", "fondazione cineteca di bologna", "fondazione ottavio ziino roma lazio", "fondazione agostino de mari", "fondazione giovanni paolo ii", "fondazione homo viator")
fond_part <- "fondazione fs"

fond_ist <- c(fond_ist, "sbac", "fondazione agrion", "fondazione slala") #expertise



# exp: "nando pagnoncelli" ; docente/sondaggista
# da ist a rappr: anci, anpci, acif, uncem, associazione nazionale piccoli comuni, associazione borghi belli italia, associazione città olio
# rappresentanze di enti territoriali, non intervengono come singole amministrazioni pubbliche ma come organizzazioni rappresentative
# di una categoria territoriale.
asso_exp <- "med associazione italiana educazione media comunicazione" #formazione e ricerca nell'ambito della media education

# cism - acronimo comune (check after split row 3499)
# sifo - società scientifica -fix
# anmli - ist
# wikimedia / creative commons/ kyoto club - expertise



























act_x <- left_join(act_x, pareggi, by = c("NOMI_clean" = "text"))
act_x <- unique(act_x)

act_x2 <- act_x[, c(3,5,7)] # DATASET PAREGGI

write.csv(act_x, "C:/Users/SImone/Desktop/actors_x.csv", row.names = FALSE)



# elimina da civile
# fondazione gimbe, fondazione sviluppo sostenibile

# aggiungi in partecipate

act_x3 <- act_x2 %>% filter(!str_detect(labels, "esperti"))

act_x3 <- act_x3 %>% filter(!str_detect(labels, "istituzioni ; part_statali"))

# su act_x
# regola istituzioni ; privati prevale istituzioni
#         part_statali ; privati prevale part_statali
#         civile ; ricerca prevale ricerca


library(readxl)
camera <- read_excel("C:/Users/SImone/Desktop/rumatu/dati_progetto/Mappatura audizioni informali Camera (definitiva e con PNRR).xlsx")
esperti_camera <- camera %>% filter(`Tipologia soggetto`=="professori universitari/esperti/professionisti") %>% select(`Nome e cognome`) %>% unique()

# Quanto pesa la galassia afferente a Confindustria?
confindustria <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/confindustria/confind_list.RData")
confindustria <- unlist(confindustria) %>% unique()

dic_conf <- dictionary(list(confindustria = confindustria))
dfm_result_conf <- tokens_lookup(tok, dictionary = dic_conf, nested_scope = "dictionary", exclusive = FALSE) %>% dfm()
result_conf <- as.tibble(dfm_result_conf)
result_conf <- result_conf %>% select(confindustria)

