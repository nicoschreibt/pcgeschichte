if(!require('tidyverse')){install.packages('tidyverse');library('tidyverse')}
if(!require('rvest')){install.packages('rvest');library('rvest')}
url.unfertig <- "https://www.ibm.com/ibm/history/history/year_"
html <- ".html"
x <- 1914
df <- data.frame()
for(i in 1:90){
  url <- paste0(url.unfertig,x,html)
  url.eingelesen <- xml2::read_html(url)
  angestellte <- url.eingelesen %>%
  rvest::html_nodes('#ibm-merchandising-module+ .ibm-container p:nth-child(1)') %>%
    html_text()%>%
    toString()
  
  umsatz <- url.eingelesen %>%
    rvest::html_nodes('#ibm-content-sidebar .ibm-container+ .ibm-container p') %>%
    html_text()%>%
    toString()
  row <- cbind(x, angestellte, umsatz)
  print(row)
  df <- rbind.data.frame(df, row)
  x <- x+1
}
df$angestellte <- parse_number(df$angestellte)
umsatz.bearbeitet <- parse_number(df$umsatz)

umsatz.bearbeitet[1:43] <- umsatz.bearbeitet[1:43]*1000000
umsatz.bearbeitet[44:90] <- umsatz.bearbeitet[44:90]*1000000000
df$umsatz <- umsatz.bearbeitet/1000000
write_csv(df, file = 'C:/Users/Nicolas Saameli/OneDrive/UniLu/Wie der PC in die Schweiz kam/Masterarbeit/Resultate_R/ibm_umsatz_mitarbeiter.csv') 
