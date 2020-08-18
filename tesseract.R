if (!require('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if (!require('tesseract')) {install.packages('tesseract')};library('tesseract')
if (!require('magick')) {install.packages('magick')};library('magick')
if (!require('pdftools')) {install.packages('pdftools')};library('pdftools')

#Files auflisten – str_subset wird nach dem ersten Mal obsolet
ordner_liste <- list.dirs("G:/M&K") %>%
  str_subset("/..../.")

#Filenames zusammensetzen

files <- data.frame(filemitpfad=list.files("G:/M&K", full.names = T, include.dirs = T, recursive = T))


files$filemitpfad <- as.character(files$filemitpfad)

#Files aufsplitten – dieser Schritt wird nach dem ersten mal obsolet

files_aufgesplitet <- setNames(do.call(rbind.data.frame, strsplit(files$filemitpfad, "/")),c("a","b","c","d","e"))
files_aufgesplitet$f <- paste0(files_aufgesplitet$c,"_",files_aufgesplitet$d)
files_aufgesplitet <- files_aufgesplitet %>% group_by(f) %>% mutate(g=1:n())
files_aufgesplitet$h <- files_aufgesplitet$g +2

files_neuenamen <- paste0(files_aufgesplitet$a,
                          "/",
                          files_aufgesplitet$b,
                          "/",
                          files_aufgesplitet$c,
                          "/",
                          files_aufgesplitet$d,
                          "/",
                          files_aufgesplitet$h,
                          ".jpg")

file.copy(files$filemitpfad,files_neuenamen)

x <- str_subset(files$filemitpfad, pattern = "hobbyund") 
x
unlink(x)

#files bearbeiten – im Moment in kopiertem Testordner

setwd("C:/Users/Nicolas Saameli/Desktop/1")
files_test <- list.files()

images_test <- lapply(files_test, image_read)



images_test[[3]]
y <- 3

for (i in length(images_test)) {
 images_test[[i]] %>%
   image_convert(type = 'Grayscale') %>%
   image_write(format = 'pdf', 
                path = paste0("C:/Users/Nicolas Saameli/Desktop/1/S",y,".pdf"))
y <- y+1
print(y)
}

y
dataframes_aufgesplittet
files

filesliste <- rbind.data.frame(
  lapply(
    ordner_liste, function(
      ordner_oeffnen){
  list.files(
    ordner_liste)
}),ordner_liste)

filesliste





files <- list.files(pattern = ".jpg")
filesmitpfad <- paste0(ordner,"/",files)
filesmitpfad
seiten <- length(filesmitpfad)+2
df_bilder <- data.frame(filesmitpfad,3:seiten,jahr,nummer)
df_bilder




df_bilder

filesmitpfad


input <- image_read("C:/Users/Nicolas Saameli/Desktop/1979/1/hobbyundkleincomputer_1979_44.jpg")
input

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '300x300') %>%
  tesseract::ocr() 



install.packages("installr")

library(installr)

updateR()
