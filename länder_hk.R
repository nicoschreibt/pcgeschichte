if (!require('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if (!require('newsmap')) {install.packages('newsmap')};library('newsmap')
if (!require('maps')) {install.packages('maps')};library('maps')
if (!require('quanteda')) {install.packages('quanteda')};library('quanteda')
if (!require('readtext')) {install.packages('readtext')};library('readtext')

rm(list=ls())
texte <- list.files('G:/M&K_1', recursive = T, include.dirs = F, pattern = "volltext", full.names = T) %>%
  map_df(~readtext(.))


