if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('quanteda')){install.packages('quanteda');library('quanteda')}

wd <- 'G:/M&K_1'
setwd(wd)

#Dataframe mit neuen DocIds anlegen

files_txt <- list.files(path = wd, 
           pattern = ".txt", 
           recursive = T, 
           full.names = T, 
           include.dirs = F,)

df_files_txt_eingelesen <- readtext::readtext(file = files_txt)
df_files_txt_eingelesen$pfad <- files_txt
df_files_txt_pfad <- data.frame(files_txt)
df_filenames <- setNames(do.call(rbind.data.frame, str_split(df_files_txt_pfad$files_txt, pattern = "/")),c("a","b","c","d","e"))
df_files_txt_eingelesen$jahr <- df_filenames$c
df_files_txt_eingelesen$ausgabe <- df_filenames$d
df_files_txt_eingelesen$jahr_ausgabe <- paste0(df_filenames$c,"_",df_filenames$d)
df_files_txt_eingelesen$filename <- df_filenames$e
df_files_txt_eingelesen$doc_id <- paste0(df_files_txt_eingelesen$jahr_ausgabe, "_",df_files_txt_eingelesen$filename)

#Df abspeichern
save(df_files_txt_eingelesen, file = "G:/texte_mk_eingelesen.Rmd")
load(file = "G:/texte_mk_eingelesen.Rmd")

# seiten kombinieren und neu abspeichern

kombiniert <- aggregate(text ~ jahr_ausgabe, data = df_files_txt_eingelesen, paste, collapse = " ")
save(kombiniert, file = "G:/texte_mk_eingelesen_kombiniert.Rmd")

df_filenames_kombiniert <- setNames(do.call(rbind.data.frame, str_split(kombiniert$jahr_ausgabe, pattern = "_")),c("a","b"))
kombiniert$neuer_pfad <- paste0(wd, 
                                "/", 
                                df_filenames_kombiniert$a, 
                                "/", 
                                df_filenames_kombiniert$b, 
                                "/volltext_", 
                                kombiniert$jahr_ausgabe,
                                ".txt")
x <- 1
for (i in 1:58) {
  write(kombiniert$text[x], file = kombiniert$neuer_pfad[x])
  x <- x+1
}

#korpus anlegen



korpus <- corpus(df_files_txt_eingelesen, docid_field = "doc_id", text_field = "text")

docvars(korpus, field = "jahr") <- df_files_txt_eingelesen$jahr

docvars(korpus, field = "ausgabe") <- df_files_txt_eingelesen$ausgabe

korpus_kombiniert <- corpus(kombiniert, docid_field = "jahr_ausgabe", text_field = "text")


#erste Auswertungen – Anzahl Seiten

pfad_resultateordner <- "C:/Users/Nicolas Saameli/OneDrive/UniLu/Wie der PC in die Schweiz kam/Masterarbeit/Resultate_R/"

korpus_stats <- summary(korpus, n= 1000000)
korpus_kombiniert_stats <- summary(korpus_kombiniert, n = 1000000)
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

dfm_getrimmt_gruppiert <- dfm_group(dfm_getrimmt, groups = "jahr_ausgabe")

#meistverwendete wörter
topfeatures(dfm_getrimmt)
meistverwendete_woerter <-textstat_frequency(dfm_getrimmt)
meistverwendete_woerter
write_csv(meistverwendete_woerter, path = paste0(pfad_resultateordner,"meistverwendete_woerter.csv"))
textplot_wordcloud(dfm_getrimmt, max_words = 200)
#frequenzen einzelner wörter

konkordanz_zukunft <- kwic(korpus, "zukunft", case_insensitive = T)
write_csv(konkordanz_zukunft, path = paste0(pfad_resultateordner,"konkordanz_zukunft.csv"))

konkordanz_ibm <- kwic(korpus_kombiniert, "ibm*", case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n()) %>%
  mutate(prozent = treffer/(korpus_kombiniert_stats$Tokens/100), hersteller = "IBM")


write_csv(konkordanz_ibm, path = paste0(pfad_resultateordner,"konkordanz_ibm.csv"))

konkordanz_apple <- kwic(korpus_kombiniert, "apple*", case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())%>%
  mutate(prozent = treffer/(korpus_kombiniert_stats$Tokens/100), hersteller = "Apple")

write_csv(konkordanz_apple, path = paste0(pfad_resultateordner,"konkordanz_apple.csv"))

konkordanz_apple_ibm_kombiniert <- bind_rows(konkordanz_apple, konkordanz_ibm)
colnames(konkordanz_apple_ibm_kombiniert) <- c("Ausgabe", "IBM", "Apple")
write_csv(konkordanz_apple_ibm_kombiniert, path = paste0(pfad_resultateordner,"konkordanz_apple_ibm_kombiniert.csv"))


#Plot der absoluten Worthäufigkeit

plot_absolut <- ggplot(konkordanz_apple_ibm_kombiniert,
       aes(docname,treffer, group = hersteller, col = hersteller)) +
  geom_line(size = 0.7) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Nennung der Suchbegriffe in absoluten Zahlen") +
  xlab("Ausgabe") +
  ylab("Nennungen") +
  labs(colour = "Hersteller")

ggsave(plot = plot_absolut,device = "png",  filename = paste0(pfad_resultateordner, "plot_absolut.png"))

#Plot der relativen Worthäufigkeit

plot_relativ <- ggplot(konkordanz_apple_ibm_kombiniert,
       aes(docname,prozent, group = hersteller, col = hersteller)) +
  geom_line(size = 0.7) +
  theme_minimal()+  
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Nennungen der Suchbegriffe in Relation zur Anzahl Wörter pro Ausgabe") +
  xlab("Ausgabe") +
  ylab("Prozentanteil")+
  labs(colour = "Hersteller")

ggsave(plot = plot_relativ,device = "png",  filename = paste0(pfad_resultateordner, "plot_relativ.png"))

konkordanz_pc <- kwic(korpus_kombiniert, c("personal computer", "*pc"), case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())%>%
  mutate(prozent = treffer/(korpus_kombiniert_stats$Tokens/100), suchbegriff = "PC")

konkordanz_pc <- kwic(korpus_kombiniert, c("personal computer", "pc"), case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())%>%
  mutate(prozent = treffer/(korpus_kombiniert_stats$Tokens/100), suchbegriff = "PC")


#funktioniert nicht

konkordanz_mikros <- kwic(korpus_kombiniert, c("mikrocomputer", "mikros"), case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())%>%
  mutate(prozent = treffer/(korpus_kombiniert_stats$Tokens/100), suchbegriff = "Mikros")


plot_pc <- ggplot(konkordanz_pc,
                  aes(docname,prozent, group = suchbegriff, col = suchbegriff )) +
  geom_line(size = 0.7) +
  theme_minimal()+  
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Nennungen des Suchbegriffs in Relation zur Anzahl Wörter pro Ausgabe") +
  xlab("Ausgabe") +
  ylab("Prozentanteil")+
  labs(colour = "Nennungen")
plot_pc
