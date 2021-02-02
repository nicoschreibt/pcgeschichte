if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('quanteda')){install.packages('quanteda');library('quanteda')}

#aufräumen
rm(list=ls())
#einlesen
load("G:/texte_mk_eingelesen_kombiniert.Rmd")
#suchen
korpus <- corpus(kombiniert)
texte.tokens <- tokens(korpus)
docnames(texte.tokens) <- kombiniert$jahr_ausgabe
ibm <- kwic(texte.tokens, pattern = "IBM", case_insensitive = T) %>% group_by(docname) %>% summarise(nennungen = n())
compaq <- kwic(texte.tokens, pattern = "compaq", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
apple <- kwic(texte.tokens, pattern = "apple", case_insensitive = T) %>% group_by(docname) %>% summarise(nennungen = n())
commodore <- kwic(texte.tokens, pattern = "commodore", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
hp <- kwic(texte.tokens, pattern = "Hewlett-Packard", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
toshiba <- kwic(texte.tokens, pattern = "toshiba", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
olivetti <- kwic(texte.tokens, pattern = "olivetti", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
philips <- kwic(texte.tokens, pattern = "philips", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())
honeywell <- kwic(texte.tokens, pattern = "honeywell", case_insensitive = T)%>% group_by(docname) %>% summarise(nennungen = n())



ic <- merge(ibm, compaq, all = T, by = "docname")
ica <- merge(ic, apple, all = T, by = "docname")
icac <- merge(ica, commodore, all = T, by = "docname")
icach <- merge(icac, hp, all = T, by = "docname")
icacht<- merge(icach, toshiba, all = T, by = "docname")
icachto<- merge(icacht, olivetti, all = T, by = "docname")
icachtop<- merge(icachto, philips, all = T, by = "docname")
icachtoph<- merge(icachtop, honeywell, all = T, by = "docname")
icachtoph[is.na(icachtoph)] <- "0"
colnames(icachtoph) <-c("jahr_ausgabe","ibm", "compaq", "apple", "commodore", "hp", "toshiba", "olivetti", "philips", "honeywell")
nennungen <- icachtoph
write_csv(nennungen, file = 'C:/Users/Nicolas Saameli/OneDrive/UniLu/Wie der PC in die Schweiz kam/Masterarbeit/Resultate_R/nennungen.csv')
