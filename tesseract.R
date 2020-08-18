if (!require('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if (!require('tesseract')) {install.packages('tesseract')};library('tesseract')
if (!require('magick')) {install.packages('magick')};library('magick')
if (!require('pdftools')) {install.packages('pdftools')};library('pdftools')
if (!require('gtools')) {install.packages('gtools')};library('gtools')



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
files_test <- list.files() %>%
  mixedsort()
images_test <- lapply(files_test, image_read)
laenge_test <- length(images_test)
arbeitsliste_filenames <- str_sub(files_test, ,-5)
z <- 1
df <- data.frame()
while(z <= laenge_test ) {
  file <-  images_test[[z]] %>%
   image_convert(type = 'Grayscale') %>%
   image_write(format = 'png')
    text <- tesseract::ocr(file, engine = tesseract("deu"))
    write(text, file = paste0("C:/Users/Nicolas Saameli/Desktop/1/S",arbeitsliste_filenames[z],".txt"))
    images_test[[z]] %>%
    image_convert(type = 'Grayscale') %>%
    image_write(format = 'pdf', 
                     path = paste0("C:/Users/Nicolas Saameli/Desktop/1/S",arbeitsliste_filenames[z],".pdf"))
    filename <- paste0("C:/Users/Nicolas Saameli/Desktop/1/S",arbeitsliste_filenames[z],".txt")
    seite <- arbeitsliste_filenames[z]
    neue_daten <- cbind.data.frame(seite, filename, text)
    df <- rbind.data.frame(df,neue_daten)
z <- z+1
}


