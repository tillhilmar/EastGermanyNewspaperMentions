library(quanteda)
library(tm)
#install.packages("tm.plugin.factiva")
library(tm.plugin.factiva)
#install.packages("RNewsflow")
library(RNewsflow)
library(quanteda.textstats)
library(data.table)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("XML")
library(XML)

getwd()

filenames <- list.files("WELT3", pattern="*.html", full.names=TRUE)

# source read by Factiva for each document. It creates a nested list
source_list <- lapply(filenames,FactivaSource,encoding = "UTF-8")

# last Factiva passage for each document
raw_list <- sapply(source_list,Corpus,list(language = NA))

n <- length(raw_list) 

# to check the number of list's element. This is to arrange number of within brakets below
# note: since nested list, one element can host more than one text (e.g. more than 1 text were checked and downloaded at once)

# Vector with all elements of nested list. To avoid writing each element, can copy and past from the .csv
corpus_vector <- c(raw_list[[	1	]],
                   raw_list[[	2	]],
                   raw_list[[	3	]],
                   raw_list[[	4	]],
                   raw_list[[	5	]],
                   raw_list[[	6	]],
                   raw_list[[	7	]],
                   raw_list[[	8	]],
                   raw_list[[	9	]],
                   raw_list[[	10	]],
                   raw_list[[	11	]],
                   raw_list[[	12	]],
                   raw_list[[	13	]],
                   raw_list[[	14	]],
                   raw_list[[	15	]],
                   raw_list[[	16	]],
                   raw_list[[	17	]],
                   raw_list[[	18	]],
                   raw_list[[	19	]],
                   raw_list[[	20	]],
                   raw_list[[	21	]],
                   raw_list[[	22	]],
                   raw_list[[	23	]])
# ... add more if needed

# Here the final CORPUS (comprising corpus of all documents) is done. Second line is to avoid duplicates later

corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'''"," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x," ' "," ")))
corpus_vector <- tm_map(corpus_vector,content_transformer(function(x,pattern) str_replace_all(x,"'"," ")))

corpus_vector_WELT1 <- corpus_vector
saveRDS(corpus_vector_WELT1,file = "corpus_vector_WELT1.Rdata")
#corpus_vector_WELT1 <- readRDS("corpus_vector_WELT1.Rdata")
corpus_vector_WELT2 <- corpus_vector
saveRDS(corpus_vector_WELT2,file = "corpus_vector_WELT2.Rdata")
#corpus_vector_WELT2 <- readRDS("corpus_vector_WELT2.Rdata")
corpus_vector_WELT3 <- corpus_vector
saveRDS(corpus_vector_WELT3,file = "corpus_vector_WELT3.Rdata")
#corpus_vector_WELT3 <- readRDS("corpus_vector_WELT3.Rdata")
corpus_vector_BILD <- corpus_vector
saveRDS(corpus_vector_BILD,file = "corpus_vector_BILD.Rdata")
#corpus_vector_BILD <- readRDS("corpus_vector_BILD.Rdata")
corpus_vector_ZEIT <- corpus_vector
saveRDS(corpus_vector_ZEIT,file = "corpus_vector_ZEIT.Rdata")
#corpus_vector_ZEIT <- readRDS("corpus_vector_ZEIT.Rdata")
corpus_vector_SZ1 <- corpus_vector
saveRDS(corpus_vector_SZ1,file = "corpus_vector_SZ1.Rdata")
#corpus_vector_SZ1 <- readRDS("corpus_vector_SZ1.Rdata")
corpus_vector_SZ2 <- corpus_vector
saveRDS(corpus_vector_SZ2,file = "corpus_vector_SZ2.Rdata")
#corpus_vector_SZ2 <- readRDS("corpus_vector_SZ2.Rdata")
corpus_vector_SZ3 <- corpus_vector
saveRDS(corpus_vector_SZ3,file = "corpus_vector_SZ3.Rdata")
#corpus_vector_SZ3 <- readRDS("corpus_vector_SZ3.Rdata")
corpus_vector_SPIEGEL1 <- corpus_vector
saveRDS(corpus_vector_SPIEGEL1,file = "corpus_vector_SPIEGEL1.Rdata")
#corpus_vector_SPIEGEL1 <- readRDS("corpus_vector_SPIEGEL1.Rdata")
corpus_vector_SPIEGEL2 <- corpus_vector
saveRDS(corpus_vector_SPIEGEL2,file = "corpus_vector_SPIEGEL2.Rdata")
#corpus_vector_SPIEGEL2 <- readRDS("corpus_vector_SPIEGEL2.Rdata")

corpus_vector_ALL <- c(corpus_vector_WELT1, corpus_vector_WELT2, corpus_vector_WELT3, corpus_vector_BILD, corpus_vector_ZEIT, corpus_vector_SZ1, corpus_vector_SZ2, corpus_vector_SZ3, corpus_vector_SPIEGEL1, corpus_vector_SPIEGEL2)
saveRDS(corpus_vector_ALL,file = "corpus_vector_ALL.Rdata")
#corpus_vector_ALL <- readRDS("corpus_vector_ALL.Rdata")

corpus_ALL <- corpus(corpus_vector_ALL)
docvars(corpus_ALL)

sapply(df_ALL$doc_id, function(x) length(unique(x)))

#remove duplicates
corpus_ALL_dfm <- dfm(corpus_ALL)
(sim <- textstat_proxy(corpus_ALL_dfm, method = "cosine", min_proxy = 0.99))
matrix2list <- function(x) {
  names(x@x) <- rownames(x)[x@i + 1]
  split(x@x, factor(x@j + 1, levels = seq(ncol(x)), labels = colnames(x)))
}
simil <- matrix2list(sim)
simil[lengths(simil) > 1]
class(simil)
library(stringi)
simil1 <- as.data.frame(t(stri_list2matrix(simil)))
#colnames(simil1) <- unique(unlist(sapply(simil, names)))
simil1$docnames <- seq.int(nrow(simil1))

todrop <- subset(simil1, select = docnames, subset = V1 < 1, drop = TRUE)
todrop <- as.data.frame(todrop)
todrop$todrop <- paste0("text", todrop$todrop)
todrop <- as.matrix(todrop)

corpus_ALL_clean <- corpus_subset(corpus_ALL, !docnames(corpus_ALL) %in% todrop)

corpus_ALL_clean$date <- as_date(corpus_ALL_clean$datetimestamp)

table(corpus_ALL_clean$origin)
corpus_ALL_clean$origin2 <- ifelse(docvars(corpus_ALL_clean,"origin") %in% c("Audio Video Foto Bild","BILD", "BILD am Sonntag", "BILD Berlin-Brandenburg", "BILD Dresden", "BILD D?sseldorf", "BILD Frankfurt", "BILD Frankfurt Rhein-Main","BILD Halle", "BILD Hamburg", "BILD Hannover","BILD K?ln","BILD Leipzig","BILD Magdeburg","BILD Mecklenburg-Vorpommern","BILD M?nchen","BILD Plus","BILD Rhein-Main","BILD Rhein-Neckar","BILD Ruhrgebiet","BILD Saarland","BILD Sachsen-Anhalt","BILD Stuttgart","BILD Th?ringen","bild.de"),"BILD Zeitung", 
                                   ifelse(docvars(corpus_ALL_clean,"origin") %in% c("Der Spiegel", "Spiegel Online","Spiegel Online (Deutsch)", "Spiegel Plus"),"Der SPIEGEL",
                                          ifelse(docvars(corpus_ALL_clean,"origin") %in% c("DIE ZEIT", "ZEIT Campus", "ZEIT Geschichte", "ZEIT Hamburg", "ZEIT im Osten", "ZEIT Magazin", "ZEIT online","ZEIT ?sterreich","ZEIT Schweiz","ZEIT Wissen","ZEITmagazin"),"Die ZEIT",
                                                 ifelse(docvars(corpus_ALL_clean,"origin") %in% c("S?ddeutsche Zeitung"),"S?ddeutsche Zeitung", 
                                                        ifelse(docvars(corpus_ALL_clean,"origin") %in% c("Die Welt", "Die Welt translated into English","Hamburger Abendblatt","Welt Aktuell","Welt am Sonntag","WELT online","WELT Online English News", "B.Z. am Sonntag"),"Die WELT",corpus_ALL_clean$origin)))))
table(corpus_ALL_clean$origin2)
saveRDS(corpus_ALL_clean, file ="corpus_ALL_clean.Rdata")
corpus_ALL_clean <- readRDS("corpus_ALL_clean.Rdata")

visegrad <- c("Polen", "polnisch*", "Tschechien", "tschechisch*", "Ungarn", "ungarisch*", "Slowakei", "slowakisch*")
#visegrad1 <- c("Polen", "polnisch", "Tschechien", "tschechisch", "Ungarn", "ungarisch", "Slowakei", "slowakisch")

subcorp_ALL <- corpus_subset(corpus_ALL_clean, grepl(paste(visegrad,collapse="|"), texts(corpus_ALL_clean)))
#subcorp_ALL <- corpus_subset(corpus_ALL_clean, grepl(paste(visegrad,collapse="|"), as.character(corpus_ALL_clean)))

corpus_ALL_clean$visegrad <- ifelse(docvars(corpus_ALL_clean, "id") %in% docvars(subcorp_ALL, "id"), "Erwähnung Polen, Tschechien, Slowakei oder Ungarn", "keine Erwähnung")
table(corpus_ALL_clean$visegrad)

#English:
corpus_ALL_clean$visegrad <- ifelse(docvars(corpus_ALL_clean, "id") %in% docvars(subcorp_ALL, "id"), "Mention of Poland, Czechia, Slovakia or Hungary", "No mention")


summary(docvars(subcorp_ALL))

df_ALL <- convert(corpus_ALL_clean, to = "data.frame", pretty = TRUE)

ls(df_ALL)
table(df_ALL$publisher)
table(df_ALL$origin2)


df_ALL %>%
  subset(origin2 == "BILD Zeitung") %>%
  summary(date)
df_ALL %>%
  subset(origin2 == "Die ZEIT") %>%
  summary(date)
df_ALL %>%
  subset(origin2 == "Die WELT") %>%
  summary(date)
df_ALL %>%
  subset(origin2 == "Süddeutsche Zeitung") %>%
  summary(date)
df_ALL %>%
  subset(origin2 == "Der SPIEGEL") %>%
  summary(date)


keyword_counts <- df_ALL %>%
  group_by(date, visegrad) %>%
  tally

wsjPal <- c('#F24D29',
            '#1C366B',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

wsjPal <- c('#111111',
            '#c3cbd0',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

keyword_counts$visegrad <- factor(keyword_counts$visegrad, levels=c("Erwähnung Polen, Tschechien, Slowakei oder Ungarn", "keine Erwähnung"))

gb <- keyword_counts %>% 
  ggplot(aes(x = date, y = n, group= visegrad, color = visegrad)) +
  geom_line(size = 0.3)+
  labs(x = "Date", y = "Erwähnung Polen, Tschechien, Slowakei oder Ungarn", color = "Erwähnung Polen, Tschechien, Slowakei oder Ungarn", fill = "Erwähnung Polen, Tschechien, Slowakei oder Ungarn", title = "Anzahl von Artikeln mit Keyword Ostdeutschland / ostdeutsch", subtitle = "n=42.845 Artikel - Süddeutsche Zeitung, Der Spiegel, Die WELT, Die ZEIT (ab 2008), BILD Zeitung (ab 2010)")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "24 month", date_labels= "%b-%Y", expand=c(0,0)) +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 8, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1))
gb

## English

keyword_counts$visegrad <- factor(keyword_counts$visegrad, levels=c("Mention of Poland, Czechia, Slovakia or Hungary", "No mention"))

gb <- keyword_counts %>% 
  ggplot(aes(x = date, y = n, group= visegrad, color = visegrad)) +
  geom_line(size = 0.3)+
  labs(x = "Date", y = "Mention of Poland, Czechia, Slovakia or Hungary", color = "Mention of Poland, Czechia, Slovakia or Hungary", fill = "Mention of Poland, Czechia, Slovakia or Hungary", title = "Number of articles that include the keyword East Germany / East German", subtitle = "n=42.845 articles - Süddeutsche Zeitung, Der Spiegel, Die WELT, Die ZEIT (from 2008), BILD Zeitung (from 2010)")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "24 month", date_labels= "%b-%Y", expand=c(0,0)) +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 11, family="sans"),
        plot.subtitle = element_text(size = 8, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1))
gb



keyword_counts <- df_ALL %>%
  group_by(date) %>%
  tally

wsjPal <- c('#F24D29',
            '#1C366B',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

wsjPal <- c('#111111',
            '#777777',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

#keyword_counts$visegrad <- factor(keyword_counts$visegrad, levels=c("Erwähnung Polen, Tschechien, Slowakei oder Ungarn", "keine Erwähnung"))

gb <- keyword_counts %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line(size = 0.3)+
  labs(x = "Date",  title = "Anzahl von Artikeln mit Keyword Ostdeutschland / ostdeutsch", subtitle = "n=42.845 Artikel - Süddeutsche Zeitung, Der Spiegel, Die WELT, Die ZEIT (ab 2008), BILD Zeitung (ab 2010)")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "24 month", date_labels= "%b-%Y", expand=c(0,0)) +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1))
gb



