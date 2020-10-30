if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('quanteda')){install.packages('quanteda');library('quanteda')}
rm(list=ls())

load("G:/texte_mk_eingelesen_kombiniert.Rmd")
korpus <- corpus(kombiniert)


konkordanz_einfach <- kwic(korpus, "Komfort", case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())
konkordanz_einfach
konkordanz_komfort <- kwic(korpus, "Komfort", case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())
konkordanz_bedienerfreundlich <- kwic(korpus, "Bedienerfreundlich", case_insensitive = T) %>%
  group_by(docname) %>%
  summarise(treffer = n())
konkordanz_bedienerfreundlich$treffer
