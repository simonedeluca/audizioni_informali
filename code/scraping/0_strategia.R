library("rvest")
library("dplyr")

# Sto cercando la strategia di scraping vincente

link <- "https://www.senato.it/Leg18/3572?current_page_29825=9" #random page in commissione 1
page <- read_html(link)

data <- page %>% html_nodes(".data em") %>% html_text()
atto <- page %>% html_nodes(".descrizione em") %>% html_text()
sub <- page %>% html_nodes(".descrizione a") %>% html_text() #to be splitted in "soggetto", "ente"

# Il vettore 'sub' contiene più di 10 elementi perché i soggetti sono estratti come singole entità
# invece di essere raggruppati nella rispettiva audizione.

# Meglio usare un'altro nodo.

sub1 <- page %>% html_nodes(".sublista_docs") %>% html_text()

# Cleaning the vector
x <- gsub("Memorie depositate dagli auditi", "", sub1)
x <- gsub("[\n]", "", x) #togli new line
library(stringr)
x <- str_trim(x, side = "both") # togli white space all'inizio e alla fine!
auditi <- x
# object-x contiene elementi che contano più di un soggetto. andrebbero 

table <- data.frame(commissione = "1", auditi, atto, data, stringsAsFactors = FALSE)
