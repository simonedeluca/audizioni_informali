library("dplyr")
library("stringr")

c14 <- read.csv("C:/Users/SImone/Desktop/audizioni_informali/data/raw_data/commissione14.csv", header = TRUE, stringsAsFactors = FALSE)

nomi <- c14$NOMI
nomi <- str_squish(nomi)

nomi <- gsub("Audizione", "", nomi) %>% str_trim(side = "left")
nomi_splitted <- str_split(nomi, ";", simplify = FALSE)

m <- matrix(nrow=95, ncol=1)
for (i in 1:95) {
  m[i,] <- length(nomi_splitted[[i]])
}

c14$times <- m[,1]

new_data <- c14[rep(1:nrow(c14), times = c14$times), ]
rownames(new_data) <- 1:nrow(new_data)

single_name <- unlist(nomi_splitted)

single_name <- str_trim(side = "both", single_name)
words2 <- "^di|^del|^della|^dell'|^delle|^dei|^da|^dai|^dal|^dall'|^dalla|^dallo|^dalle"
single_name <- gsub(words2, "", single_name) %>% str_trim(side = "left")

p <- str_extract(single_name, "-")
p <- which(p == "-")
x <- str_subset(single_name, "-")

y <- c(3,4,7,17)
for (i in y) {
  x[i] <- gsub("-", " ", x[i])
}

output <- c()
for (i in y) {
  h <- p[i]
  output <- c(output, h)
}

for (i in y) {
  single_name[output] <- x[y]
}
single_name[253] <- "dr. Castaldi, Ispettore generale capo dell'Ispettorato generale per i rapporti finanziari con l'UE Ministero dell'economia e delle finanze"

p <- str_extract(single_name, " e ")
p <- which(p == " e ")
x <- str_subset(single_name, " e ")

single_name[403] <- "AIE-UIC"

single_name <- str_squish(single_name)
new_data[,2] <- single_name

x <- new_data$DATA
x <- gsub("^\\s","0", x)
x <- str_split_fixed(x, " ", 3)
x <- as.data.frame(x)

x$month <- with(x, ifelse(V2 %in% "Gennaio", 1,
                          ifelse(V2 %in% "Febbraio", 2,
                                 ifelse(V2 %in% "Marzo", 3,
                                        ifelse(V2 %in% "Aprile", 4,
                                               ifelse(V2 %in% "Maggio", 5,
                                                      ifelse(V2 %in% "Giugno", 6,
                                                             ifelse(V2 %in% "Luglio", 7,
                                                                    ifelse(V2 %in% "Agosto", 8,
                                                                           ifelse(V2 %in% "Settembre", 9,
                                                                                  ifelse(V2 %in% "Ottobre", 10,
                                                                                         ifelse(V2 %in% "Novembre", 11,
                                                                                                ifelse(V2 %in% "Dicembre", 12,
                                                                                                       "NA")))))))))))))


date <- paste(x$V1,x$month,x$V3, sep= "-")
new_data <- subset(new_data, select = -c(DATA,times))
new_data$DATA <- as.Date(date, format="%d-%m-%Y")

new_data$COMMISSIONE <- gsub("ª", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(" e ", " ; ", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub(",", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("con", " ;", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Senato", "", new_data$COMMISSIONE)
new_data$COMMISSIONE <- gsub("Camera", "", new_data$COMMISSIONE) %>% str_squish()

write.csv(new_data, "C:/Users/SImone/Desktop/audizioni_informali/data/preprocessed_data/C14.csv", row.names = FALSE)
