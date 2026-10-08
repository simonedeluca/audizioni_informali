#######################################
#             DIZIONARIO              #
# Aziende private , centri di ricerca #
#             ed esperti              #
#######################################


# CENTRI DI RICERCA

# Da script: società_civile

fond_senato <- senato %>% filter(str_detect(NOMI_clean, "fondazione")) %>% pull(NOMI_clean) %>% unique()

fond_ricerca <- fond_senato[c(2,4,10,24,29,30,38)]
fond_ricerca <- c(fond_ricerca, "fondazione ricerca salute")

comit_ricerca <- c("accademia georgofili", "comitato glaciologico italiano")

centri_ricerca <- c(fond_ricerca, comit_ricerca, "censis")


# PRIVATI

# Da script: società_civile

privati <- "forum pa"


# ESPERTI

experts <- c("prof*", "dott*", "avv*", "scrittore", "proto", "arch*", "ing", "ingegnere", "signor", "esperto")

# Save data
saveRDS(centri_ricerca, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/centri_ricerca.RData")
saveRDS(privati, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/privati.RData")
saveRDS(experts, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/experts.RData")





# Revisione

# Misclassificazioni in vettore privati da correggere


# Carico dizionario
dizionario_babele <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_babele.RData")
dizionario_pt1 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dizionario_pt1.RData")
dict10 <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict10.RData")

dizionario <- c(dizionario_pt1, dizionario_babele, dict10)

# Vettore privati
pvt <- dizionario[["privati"]]


# Da privati a partecipate: gme, gruppo hera, sea, cesi*
part <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_partecipate-revised.RData")
part <- unique(part)

part <- c(part, "gme", "gruppo hera", "sea", "cesi")

# flag expertise: cesi


# Da privati a istituzioni: accredia*
ist <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/institution-revised.RData")

ist <- c(ist, "accredia")

# flag expertise: accredia

pvt <- setdiff(pvt, ist)


# Da privati a categoria: ecopneus, erion, ecodom, polieco, igas, sistema trasporti, sistema impresa, ascomac, op daunia&bio, re mind, remind, mediacoop
cat <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/institutions/cat_revised_up.RData")

cat <- c(cat, "ecopneus", "erion", "ecodom", "polieco", "igas", "sistema trasporti", "sistema impresa", "ascomac", "op daunia&bio", "re mind", "remind", "mediacoop")

pvt <- setdiff(pvt, cat)


# Da privati a esperti: nando pagnoncelli
exp <- "nando pagnoncelli"

pvt <- setdiff(pvt, exp)


# Rimozione pattern da privati: 

to_remove <- c("gestore mercati energetici gme", "gruppo sea", "igas imprese gas", "sistema trasporti sistema impresa", "op daunia & bio")

pvt <- setdiff(pvt, to_remove)

# expertise: enama, mfsd, forum pa, istituto restauro roma irr, istituto internazionale elicicoltura cherasco, ambiente italia, elemens, althesys, euromedia research


# Integrazione da revisione categoria e civile

pvt <- c(pvt, "energia nazionale", "8puntozero", "proges", "coopculture")

# Integrazione da revisione categoria
vec_civile <- readRDS("C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/civile_update.RData")

vec_civile <- c(vec_civile, "touring club italiano", "associazione campeggiatori turistici d italia", "unpli",
                "convol", "csvnet", "link2007", "equogarantito", "acli", "enil", "federazione italiana caccia")
vec_civile <- c(vec_civile, "fima")





# Revisione categoria residua Altro

# vettori da dizionario
altro <- dizionario[["altro"]]
cr <- dizionario[["centr_ricerca"]]


# vettore da revisione società civile
vec_altro <- readRDS(file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/vec_altro.RData")

# vettori da revisione categoria
ss_cr <- c("associazione italiana studio prevenzione analisi crimine",
           "aistec", "associazione italiana psicologia giuridica", "federazione medico sportiva italiana",
           "commissione italiana insegnamento matematica", "ciim",
           "commissione didattica nazionale coordinamento società storiche",
           "associazione italiana arbitrato")

ss <- c("siml", "aifm", "fimeuc", "sivae", "airp", "goal") #expertise

com_episteme <- c(vec_altro, ss_cr, ss) 


altro <- c(altro, cr, com_episteme) %>% unique()
saveRDS(altro, "C:/Users/SImone/Desktop/altro.RData")


# Facciamo una cernita

# altro / think tank, centri studi, osservatori e istituti di ricerca non istituzionali
# scenari immobiliari, endisu

# altro / società scientifiche, associazioni accademiche e comunità tecnico-epistemiche
# associazione italiano arbitrato, federazione medico sportiva italiana

# altro / enti religiosi, confessionali e soggetti ecclesiali
# cultura cattolica


# Privati: Digitale, telecomunicazioni, piattaforme e tecnologia
pvt <- c(pvt, "prime video", "netflix", "chili", "iliad", "fastweb")

# Privati: Media, cultura, spettacolo, editoria e industrie creative
pvt <- c(pvt, "paramount global", "flyeurope tv", "mediaset", "sky", "weshort", "la7", "discovery", "viacom",
         "automotive news europe", "quotidiano energia", "staffetta quotidiana", "limes",
         "mattino", "terre mezzo editore", "lafeltrinelli")  

# flag: tuttoscuola, nuova secondaria, automotive news europe, quotidiano energia, staffetta quotidiana, limes  

intersect(pvt, altro)
altro <- setdiff(altro, pvt)


# Società civile
vec_civile <- c(vec_civile, "mondo digitale", "aware", "repubblica stagisti", "salvagente", "tuttoscuola", "nuova secondaria", "gruppo intervento giuridico")
# expertise: mondo digitale, tuttoscuola, nuova secondaria

vec_civile <- c(vec_civile, "sma", "diritto apprendere")

intersect(altro, vec_civile) # mancano i risultati di rivista
altro <- setdiff(altro, vec_civile)

vec_civile <- setdiff(vec_civile, "associazione nazionale musei enti locali istituzionali anmli")

# Categoria: ordini / reti / asso professionali
cat <- c(cat, "epc", "ordine nazionale consulenti lavoro", "fondazione studi consulenti lavoro",
         "cluster tecnologico nazionale energia ctne", "tavolo autoconsumo efficienza energetica", "#vita", "gruppo vita",
         "associazione italiana arbitrato", "simef società italiana mediatori familiari")
# expertise: ctne, tavolo autoconsumo efficienza energetica, #vita, gruppo vita, fondazione studi consulenti lavoro

cat <- c(cat, "olimpolli montagnani", "civita", "airp") # cluster / reti / settore

intersect(altro, cat) # alert su: liberi uguali / fondazione sviluppo sostenibile / associazione italiana arbitrato

cat <- cat[!cat %in% c("associazione italiana arbitrato", "liberi uguali")]
altro <- setdiff(altro, "fondazione sviluppo sostenibile")

altro <- setdiff(altro, cat)

# Istituzioni
ist <- c(ist, "garr", "stazione zoologica anton dohrn", "fondazione montagna sicura")
# expertise: garr, fondazione montagna sicura, stazione zoologica anton dohrn

intersect(altro, ist)
altro <- setdiff(altro, ist)


# Esperti
esperti <- c("padre", "don", "suor")



# Rimuovi da altro le varianti
altro <- altro[!altro %in% c("siae società italiana autori editori", "presidente società italiana autori editori siae",
                             "ente nazionale diritto studio fondazione endisu", "rivista",
                             "gruppo vita valore innovazione terapie avanzate", "padre occhetta", "don", "suor")]





# Revisione Esperti

# Procediamo diversamente. Invece di partire dal vettore già individuato, raggruppiamo i pattern individuati.
# prof*, dott*, arch*, avv* nel vecchio vettore davano luogo a falsi positivi

# I ruoli professionali e dirigenziali tecnico-scientifici determinano la categoria.
# I ruoli dirigenziali rappresentativi e i pattern deboli hanno carattere neutro.
# Se sono accompagnati dall'ente rappresentato, nella classificazione prevale la macrocategoria dell'ente.
# In caso contrario, si può procedere con l'assegnazione della categoria esperti dopo verifica manuale.


# Categoria

cat <- c(cat, "itsright")

esperti <- c("prof", "profssa", "professor", "professore", "professoressa", "professori", "professoresse", "docente", "ricercatore",
"ricercatrice", "esperto", "esperta", "avv", "avvocato", "ing", "arch", "architetto", "medico", "giornalista", "invitato speciale",
"scrittore", "consulente", "maestro", "m °", "proto", "suor", "don", "padre",
"direttore sanitario", "responsabile scientifico", "direttore tecnico", "dirigente medico", "responsabile laboratorio") # ruoli dirigenziali tecnici


neutro <- c("presidente", "pres", "vicepresidente", "vice presidente", "direttore", "dg", "segretario", "rappresentante", "portavoce",
"coordinatore", "responsabile", "amministratore", "ceo", "fondatore", "dott", "dottssa", "dottore", "dottoressa", "dottor", "signor")


# Salviamo i vettori revisionati direttamente in un nuovo dizionario

ist <- unique(ist)


dict_revised <- dictionary(list(
  istituzioni = ist,
  partecipate = part,
  categoria = cat,
  soc_civile = vec_civile,
  privati = pvt,
  esperti = esperti,
  altro = altro,
  neutro = neutro
))




saveRDS(dizionario, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_1.RData")
saveRDS(dict_revised, file = "C:/Users/SImone/Desktop/audizioni_informali/data/dictionary/dict_revised.RData")

