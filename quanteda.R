if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('quanteda')){install.packages('quanteda');library('quanteda')}
wd <- 'G:/M&K_1/'
setwd(wd)

#Dataframe mit neuen DocIds anlegen

files_txt <- list.files(path = wd, 
           pattern = ".txt", 
           recursive = T, 
           full.names = T)
df_files_txt_eingelesen <- readtext::readtext(file = files_txt)
df_files_txt_eingelesen$pfad <- files_txt
df_files_txt_pfad <- data.frame(files_txt)
df_filenames <- setNames(do.call(rbind.data.frame, str_split(df_files_txt_pfad$files_txt, pattern = "/")),c("a","b","c","d","e"))
df_files_txt_eingelesen$jahr <- df_filenames$c
df_files_txt_eingelesen$ausgabe <- df_filenames$d
df_files_txt_eingelesen$jahr_ausgabe <- paste0(df_filenames$c,"_",df_filenames$d)
df_files_txt_eingelesen$filename <- df_filenames$e
df_files_txt_eingelesen$doc_id <- paste0(df_files_txt_eingelesen$jahr_ausgabe, "_",df_files_txt_eingelesen$filename)

#korpus anlegen
korpus <- corpus(df_files_txt_eingelesen, docid_field = "doc_id", text_field = "text")
docvars(korpus, field = "jahr") <- df_files_txt_eingelesen$jahr
docvars(korpus, field = "ausgabe") <- df_files_txt_eingelesen$ausgabe

#erste Auswertungen – Anzahl Seiten

pfad_resultateordner <- "C:/Users/Nicolas Saameli/OneDrive/UniLu/Wie der PC in die Schweiz kam/Masterarbeit/Resultate_R/"
korpus_stats <- summary(korpus, n= 1000000)
korpus_stats
korpus_stats_jahr_ausgabe <- korpus_stats %>%
  group_by(jahr_ausgabe) %>%
  summarise(anzahl_seiten=n())
write_csv(korpus_stats_jahr_ausgabe, path = paste0(pfad_resultateordner,"seiten_pro_jahr_ausgabe.csv"))

#Tokenisierung&dfm
tokens_alle <- tokens(korpus)
tokens_getrimmt <- tokens(korpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T)
tokens_getrimmt <- tokens_remove(tokens_getrimmt, min_nchar = 3, pattern = c(stopwords("german"), "dass"))
dfm_alle <- dfm(korpus)
dfm_getrimmt<- dfm(tokens_getrimmt)
topfeatures(dfm_getrimmt)

#meistverwendete wörter
meistverwendete_woerter <-textstat_frequency(dfm_getrimmt)
meistverwendete_woerter
write_csv(meistverwendete_woerter, path = paste0(pfad_resultateordner,"meistverwendete_woerter.csv"))
textplot_wordcloud(dfm_getrimmt, max_words = 200)
#frequenzen einzelner wörter

dfm_select(dfm_getrimmt, pattern = "ibm*")
