if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('quanteda')){install.packages('quanteda');library('quanteda')}
if(!require('quanteda.textmodels')){install.packages('quanteda.textmodels');library('quanteda.textmodels')}
if(!require('caret')){install.packages('caret');library('caret')}


pfad_resultateordner <- "C:/Users/Nicolas Saameli/OneDrive/UniLu/Wie der PC in die Schweiz kam/Masterarbeit/Resultate_R/"
load(file = "G:/texte_mk_eingelesen.Rmd")
korpus <- corpus(df_files_txt_eingelesen, docid_field = "doc_id", text_field = "text")
random_sample <- corpus_sample(korpus, 521)
df_random_sample <- as.data.frame(random_sample)
df_random_sample$rowname <- row.names(df_random_sample)
df_random_sample$werbung <- 0
df_random_sample$rowname
#pfad zum Bild hinzufügen
pfad_string_split <- setNames(do.call(rbind.data.frame, strsplit(df_random_sample$rowname, "_")),c("a","b","c"))
pfad_string_split$d <- "G:/M&K_1/"
pfad_string_split$e <- str_sub(pfad_string_split$c,0,5)
pfad_string_split$f <- "jpg"
pfad_string_split$neue_filenames <- paste0(pfad_string_split$d, 
                                           pfad_string_split$a,
                                           "/",
                                           pfad_string_split$b,
                                           "/",
                                           pfad_string_split$e,
                                           pfad_string_split$f)
df_random_sample$bild <- pfad_string_split$neue_filenames

#Loop für Klassifikation einrichten – nur ganzseitige Anzeigen, keine Stelleninserate, keine Kleinanzeigen
y <- 1


for (i in 1:length(df_random_sample$rowname)) {
  browseURL(df_random_sample$bild[y])
  auswahl <- menu(choices = c("Ja", "Nein"), title = "Werbung?")
  df_random_sample$werbung[y] <- auswahl
  y <- y+1
}
 
#df_speichern
pfad_random_sample <- paste0(pfad_resultateordner, "random_sample_bayes.csv")

write_csv(df_random_sample, path= paste0(pfad_random_sample))


#Bayes trainieren

random_sample <- corpus(random_sample, text_field = )
set.seed(300)
id_train <- sample(1:521, 400, replace = F)
random_sample$id_nummer <- 1:521

dfm_training <- corpus_subset(random_sample, id_nummer %in% id_train) %>%
  dfm(remove = stopwords("german"))

dfm_test <- corpus_subset(random_sample, !id_nummer %in% id_train) %>%
  dfm(remove = stopwords("german"))

textmodell <- textmodel_nb(dfm_training, dfm_training$werbung)
summary(textmodell)

dfm_matched <- dfm_match(dfm_test, features = featnames(dfm_training))

ist_es_werbung_manuell <- dfm_matched$werbung
ist_es_werbung_vorhersage <- predict(textmodell, newdata = dfm_matched)
ist_es_werbung_check <- table(ist_es_werbung_manuell, ist_es_werbung_vorhersage)
confusionMatrix(ist_es_werbung_check, mode = "everything")

#Bayes auf grosse DFM anwenden

dfm_alles <- dfm(korpus, remove = stopwords("german"))

dfm_alles_matched <- dfm_match(dfm_alles, features = featnames(dfm_training))
werbung_string <- predict(textmodell, newdata = dfm_alles_matched)
df_werbung_string <- data.frame(werbung_string)
df_files_txt_eingelesen$werbung <- df_werbung_string$werbung_string


df_files_txt_eingelesen$pfad_bild <- str_sub(df_files_txt_eingelesen$pfad,0 ,-4)
df_files_txt_eingelesen$pfad_bild <- paste0(df_files_txt_eingelesen$pfad_bild, "jpg")

z <- 1
for (i in 1:length(df_files_txt_eingelesen$werbung)) {
  if (df_files_txt_eingelesen$werbung[z] == 1 ){
    browseURL(df_files_txt_eingelesen$pfad_bild[z])
    Sys.sleep(1.5)
    
  }
  z <- z+1
  print(z)
  }

df_files_txt_eingelesen$werbung <-as.integer(df_files_txt_eingelesen$werbung)

write_csv(df_files_txt_eingelesen, path= paste0(pfad_resultateordner, "random_sample_bayes.csv"))



df_anzahl_werbungen <- df_files_txt_eingelesen %>%
  group_by(jahr_ausgabe)%>%
  filter(werbung == 1) %>%
  summarise(seiten = sum(werbung))


df_anzahl_werbungen$seiten
