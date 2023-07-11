library("dplyr")

x <- c(1:3,5:14)

data <- list()

for (i in x) {
  data[[i]] <- read.csv(paste0("C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/C",i,".csv"), header = TRUE, stringsAsFactors = FALSE)
  }

df <- bind_rows(data)

# OR

path <- paste0("C:/Users/pc/Desktop/Progetto Audizioni/data/preprocessed_data/C",1:14,".csv")
path <- path[-4]

data <- lapply(path, read.csv)

df <- bind_rows(data)

setwd("C:/Users/pc/Desktop")
library("rio")
export(df, "data.xlsx")


library("xlsx")
write.xlsx (df, "data.xlsx", sheetName="Dati", col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE, password=NULL)


