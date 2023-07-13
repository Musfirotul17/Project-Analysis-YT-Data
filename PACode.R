install.packages("tidyverse")
install.packages(c("tidyverse","readxl","xlsx","writexl","tidytext"))
install.packages(c("devtools","qdap","tm","stopwords"))
devtools::install_github("nurandi/katadasaR")
install.packages("stringr")

library(dplyr)
library(stringr)
library(tm)
library(stopwords)
library(katadasaR)
library(stringi)
library(qdapRegex)
library(igraph)
library(networkD3)
library(readxl)
library(tidytext)
library(writexl)
library(tidyr)

# kata normalisasi
old <- c(" ni "," tu "," org "," yg "," ga ","ambik"," klo "," ngga ","tdk", " nggak ", " ya ")
new <- c(" ini "," itu "," orang "," yang "," tidak ","ambil"," kalau "," tidak "," tidak ", " tidak "," iya ")

#butuh dihapus
hapus <- c("aja","erick","erik", "gak","nya","iya","orang","semoga","tohir")

fungsi_proses <- function(a){
  a %>%
    casefold() %>%
    rm_url(pattern = pastex("@rm_youtube_url","@rm_url")) %>%
    gsub('#\\S+','',.) %>%
    gsub('@\\S+','',.) %>%
    gsub('[^[:alpha:] ]','',.) %>%
    stri_trans_general("latin-ascii") %>%
    stri_replace_all_fixed(old,new,vectorize_all = FALSE) %>%
    removeWords(stopwords(language = "id", source = "nltk")) %>%
    removeWords(hapus)
}

data <- read.csv("Data.csv")

dtnew <- mutate(data,komentar_bersih=sapply(textDisplay,fungsi_proses))

dttoken <- dtnew %>%
  select(komentar_bersih) %>%
  unnest_tokens(kata,komentar_bersih) %>%
  count(kata) %>%
  top_n(12)


dt1 <- data %>% select(authorDisplayName,textDisplay)

dt1$authorDisplayName <- paste0('@',dt1$authorDisplayName)

dt2 <- dt1 %>%
  unite(teks,authorDisplayName,textDisplay,sep=" ")

dt3 <- str_extract_all(dt2$teks,"(@[[:alnum:]_]*)")
dt3 <- sapply(dt3,paste,collapse=" ")
dt3 <- data.frame(dt3)

dt3$count <- str_count(dt3$dt3,"\\S+")

dt4 <- dt3 %>% filter(count > 1)

dt5 <- dt4 %>%
  select(dt3) %>%
  unnest_tokens(username,dt3,token = "ngrams",n=2)

dt6 <- dt5 %>%
  separate(username,into=c("source","target"),sep=" ")

dt6$source <- paste0('@',dt6$source)
dt6$target <- paste0('@',dt6$target)

ig <- graph_from_data_frame(dt6,directed=FALSE)
plot(ig,layout=layout_with_kk,vertex.size=3,vertex.label=NA)

library(networkD3)
simpleNetwork(dt6)

write_graph(ig,"ajs.graphml",format="graphml")

