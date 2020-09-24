if (!require('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if (!require('tesseract')) {install.packages('tesseract')};library('tesseract')
if (!require('magick')) {install.packages('magick')};library('magick')
if (!require('pdftools')) {install.packages('pdftools')};library('pdftools')
if (!require('gtools')) {install.packages('gtools')};library('gtools')

#Filenames zusammensetzen

wd <- "G:/M&K_1/1979/2"
setwd(wd)
files <- data.frame(filemitpfad=list.files(wd, full.names = T))
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

x <- str_subset(files$filemitpfad, pattern = "image") 
unlink(x)


#files bearbeiten – im Moment in kopiertem Testordner

setwd(wd)
files_test <- list.files(recursive = T, pattern = ".jpg") %>%
  mixedsort()

arbeitsliste_filenames_ohnetyp_ohnepfad <- str_sub(files_test,start = 1,end = -4)
arbeitsliste_filenames_ohnetyp <- paste0(wd, "/",arbeitsliste_filenames_ohnetyp_ohnepfad)
arbeitsliste_filenames_ohnetyp

#OCR durchführen

laenge_test <- length(files_test)
z <- 1
df <- data.frame()
tesseract_download('deu')


while(z <= laenge_test ) {
  x <- paste0(wd,"/",files_test[z])
  pdfpfad <- paste0(arbeitsliste_filenames_ohnetyp[z],"pdf")
  txtpfad <- paste0(arbeitsliste_filenames_ohnetyp[z],"txt")
  images_test <- image_read(x)
  
      file <-  images_test %>%
   image_convert(type = 'Grayscale') %>%
   image_write(format = 'png')
    
    text <- tesseract::ocr(file, engine = tesseract("deu"))
    
    write(text, file = txtpfad)
    
    images_test %>%
    image_convert(type = 'Grayscale') %>%
    image_write(format = 'pdf', 
                     path = pdfpfad)
    neue_daten <- cbind.data.frame(pdfpfad, text)
    df <- rbind.data.frame(df,neue_daten)
z <- z+1
}


#neue namen

files <- list.files(wd, full.names = T, recursive = T)
files_aufgesplitet_a <- setNames(do.call(rbind.data.frame, strsplit(files, "/")),c("a","b","c","d","e"))
files
laenge_files_aufgesplitet_a <- length(files_aufgesplitet_a$a)
laenge_files_aufgesplitet_a

files_aufgesplitet_a$f <- stringr::str_length(files_aufgesplitet_a$e)

files_aufgesplitet_a$g <- 8 - files_aufgesplitet_a$f
files_aufgesplitet_a$g
files_aufgesplitet_a$h <- ""

y <-1

while (y <= laenge_files_aufgesplitet_a) {
  if (files_aufgesplitet_a$g[y] == 1) {
  files_aufgesplitet_a$h[y] <- "0"  
  } else if (files_aufgesplitet_a$g[y] == 2) {
    files_aufgesplitet_a$h[y] <- "00"  
  } else if (files_aufgesplitet_a$g[y] == 3) {
    files_aufgesplitet_a$h[y] <- "000"  
  }
y <- y+1

}

files_neuenamen_def <- paste0(files_aufgesplitet_a$a,
                          "/",
                          files_aufgesplitet_a$b,
                          "/",
                          files_aufgesplitet_a$c,
                          "/",
                          files_aufgesplitet_a$d,
                          "/",
                          files_aufgesplitet_a$h,
                          files_aufgesplitet_a$e)
files_neuenamen_def
file.rename(files,files_neuenamen_def)

#pdfs kombinieren

dirs <- list.dirs(wd)
laenge <- stringr::str_length(dirs) %>%
  as.character()
df_dirs <- data.frame(dirs, laenge)
dirs_filter <- df_dirs %>%
  filter(str_detect(laenge, "15"))

i <- 1
laenge_dirs <- length(dirs_filter$dirs)

while (i <= laenge_dirs) {
  pdf_files <- list.files(dirs_filter$dirs[i], full.names = T, pattern = "pdf")
  pdf_combine(pdf_files, output = paste0(dirs_filter$dirs[i], "/kombiniert.pdf"))
  i <- i+1  
}
#kombinierte Files umbenennen
files_kombiniert <- list.files(wd, full.names = T, , pattern = "kombiniert", recursive = T)
files_kombiniert_split <- setNames(do.call(rbind.data.frame, strsplit(files_kombiniert, "/")),c("a","b","c","d","e"))
neue_filenames_titel <- paste0("mikroundkleincomputer_", files_kombiniert_split$c, "_", files_kombiniert_split$d)
files_kombiniert_split$f <- neue_filenames_titel
neue_filenames <- paste0(files_kombiniert_split$a,
                         "/",
                         files_kombiniert_split$b,
                         "/",
                         files_kombiniert_split$c,
                         "/",
                         files_kombiniert_split$d,
                         "/",
                         files_kombiniert_split$f,
                         ".pdf")
file.rename(files_kombiniert,neue_filenames)

#files in arbeitsordner verschieben, um mit ScanTailor zu bearbeiten

wd <- "G:/M&K_1"
arbeitsordner <- "E:/arbeitsordner/"

files <- data.frame(filemitpfad=list.files(wd, full.names = T, include.dirs = T, recursive = T, pattern = ".jpg"))
files$filemitpfad <- as.character(files$filemitpfad)
files_aufgesplitet <- setNames(do.call(rbind.data.frame, strsplit(files$filemitpfad, "/")),c("a","b","c","d","e"))
files_aufgesplitet$f <- paste0(
  arbeitsordner,
  files_aufgesplitet$c,
  "_",
  files_aufgesplitet$d,
  "_",
  files_aufgesplitet$e
)
arbeitspfad <- files_aufgesplitet$f
file.copy(files$filemitpfad, arbeitspfad)


to <- "E:/arbeitsordner_txt/"

files <- list.files(path = wd, full.names = T, include.dirs = F, recursive = T, pattern = ".txt")

txt_files <- data.frame(filemitpfad=list.files(wd, full.names = T, include.dirs = T, recursive = T, pattern = ".txt"))
txt_files$filemitpfad <- as.character(txt_files$filemitpfad)
files_aufgesplitet <- setNames(do.call(rbind.data.frame, strsplit(txt_files$filemitpfad, "/")),c("a","b","c","d","e"))
files_aufgesplitet$f <- paste0(
  to,
  files_aufgesplitet$c,
  "_",
  files_aufgesplitet$d,
  "_",
  files_aufgesplitet$e
)
arbeitspfad_to <- files_aufgesplitet$f
file.copy(txt_files$filemitpfad, arbeitspfad_to)

#tesseract verbesserung

deu <- tesseract(language = "deu", options = list(tessedit_pageseg_mode = 1))
file <- list.files()
y <- ocr(file, engine = deu)
z <- str_replace_all(y, "\\s+", " ")
#Wichtig: Leerschlag nach \\- 
#Kollateralschaden: "5oder 10-MByte-Plattenspeicher"-Kopplung
z_1 <- str_replace_all(z, "\\- ", "")

#neue Filenamen vorbereiten

files_arbeitsordner <- data.frame(list.files("E:/arbeitsordner", full.names = T, recursive = T))
files_arbeitsordner_aufgesplitet <- setNames(do.call(rbind.data.frame, strsplit(files_arbeitsordner$list.files..E..arbeitsordner...full.names...T..recursive...T., "/")),c("a","b","c","d"))
files_arbeitsordner_aufgesplitet$e <- str_sub(files_arbeitsordner_aufgesplitet$d,1,-5)
files_arbeitsordner_aufgesplitet$f <- paste0(files_arbeitsordner_aufgesplitet$e, ".txt")
to <- "E:/arbeitsordner_txt/"
ziel_txt <- paste0(to, files_arbeitsordner_aufgesplitet$f)

#OCR durchführen verbessert

x <-1
deu <- tesseract(language = "deu", options = list(tessedit_pageseg_mode = 1))

while (x <= 5214) {

  file <- files_arbeitsordner$list.files..E..arbeitsordner...full.names...T..recursive...T.[x]
  text <- ocr(file, engine = deu)
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_replace_all(text, "\\- ", "")
  write(text, file = ziel_txt[x])
  print(x)
  x <- x+1
}


#Files zurückkopieren

wd <- "G:/M&K_1/"
to <- "E:/arbeitsordner_txt/"

files_txt_alt <- list.files(path = wd, full.names = T, include.dirs = F, recursive = T, pattern = ".txt")

unlink(files_txt_alt)
files_txt_neu <- data.frame(list.files(to, pattern = ".txt", full.names = F))
files_txt_neu_move <- list.files(to, pattern = ".txt", full.names = T)
files_txt_neu_move
files_txt_neu_split_df <- setNames(do.call(rbind.data.frame, stringr::str_split(files_txt_neu$list.files.to..pattern.....txt...full.names...F., pattern = "_")),c("a","b","c"))
files_txt_neu_move
neue_filenames <- paste0(wd, files_txt_neu_split_df$a, "/", files_txt_neu_split_df$b, "/",files_txt_neu_split_df$c )
neue_filenames
file.copy(from = files_txt_neu_move, to = neue_filenames)
