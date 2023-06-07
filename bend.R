#učitavanje biblioteke za čišćenje podataka
library(tidyr)
#učitavanje biblioteke za efikasno i intuitivno manipulisanje podacima
library(dplyr)
library(stringr)

#install.packages("rvest")
library(rvest)

#učitavanje HTML stranice sa Wikipedie
url_wiki <- "https://en.wikipedia.org/wiki/List_of_songs_recorded_by_the_Rolling_Stones"
page_wiki <- read_html(url_wiki)

html_table_nodes <- html_nodes(page_wiki, "table.wikitable.sortable")
# Bira prvu tabelu
table_html <- html_table_nodes[[1]]  
# Konvertuje HTML kod tabele u data frame
table <- html_table(table_html, fill = TRUE)  
#uklanjamo poslednju kolonu iz table
table <- table[,-ncol(table)]

#sredjujemo Na vrednosti u pocetnom df sa svim pesmama
table[table == "" | table == "–"] <- NA
table

# zamenjujemo "Five by Five (EP) (UK)12 X 5 (US)" u "12 X 5" u "Album name" koloni
table$"Original release" <- gsub(".*12 ?X ?5.*", "12 X 5", table$"Original release", perl = TRUE)

#menjamo naziv kolone Original release u Album name
colnames(table)[colnames(table) == 'Original release'] <- 'Album name'

#uklanjamo navodnike iz kolone "Title"
table$Title <-  gsub("\"", "", table$Title)

#instaliranje i učitavanje "magrittr" biblioteke radi korišćenja pipe operatora (%>%)
#pipe operator omogućava ulančavanje funkcija, izlaz iz jedne funkcije je ulaz u drugu
#install.packages("magrittr")
library(magrittr)

#ubacujemo tip albuma u df( studio, live i bootlegs)
table$album_type <- NA
position <- which(names(table) == "Album name")
# premestamo novu kolonu na poziciju posle "Original release"
table <- dplyr::select(table, 1:position,album_type, (position+1):ncol(table))  

#preimenuj kolonu album_type
names(table)[names(table) == "album_type"] <- "Album type"
#brisemo On Air album
table <- table[!grepl("On Air", table$`Album name`), ]
#brisemo Four Flicks album
table <- table[!grepl("Four Flicks", table$`Album name`), ]
#brisemo Forty Licks album
table <- table[!grepl("Forty Licks", table$`Album name`), ]
#učitavamo sve albume
######################################################
#ucitavamo album 12 X 5
url_album_12_x_5 <- "https://www.allmusic.com/album/12-x-5-mw0000650698"
html_album_12_x_5 <- read_html(url_album_12_x_5)
album_12_x_5 <-  html_table(html_album_12_x_5, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_12_x_5 <- album_12_x_5[-1]
#brišemo poslednju kolonu
album_12_x_5 <- album_12_x_5[, -ncol(album_12_x_5)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_12_x_5)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_12_x_5)[names(album_12_x_5) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_12_x_5[album_12_x_5 == ""] <- NA

#dodajemo u početnu tabelu kolone Song duration i Track number
table$track_number <- NA
position <- which(names(table) == "Album type")
# premeštamo novu kolonu na poziciju posle "Album type"
table <- dplyr::select(table, 1:position,track_number, (position+1):ncol(table))  
#preimenuj kolonu album_type
names(table)[names(table) == "track_number"] <- "Track number"

#na isti način dodajemo i kolonu Song duration
table$song_duration <- NA
position <-  which(names(table) == "Track number")
# premestamo novu kolonu na poziciju posle "Track number"
table <- dplyr::select(table, 1:position,song_duration, (position+1):ncol(table))
#preimenuj kolonu song_duration
names(table)[names(table) == "song_duration"] <- "Song duration"

#preimenujemo kolonu Title/Composer u Title
names(album_12_x_5)[names(album_12_x_5) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_12_x_5
#zamena višestrukih razmaka jednim razmakom
album_12_x_5$Title <- gsub("\\s+", " ",album_12_x_5$Title)
#uklanjanje znakova novog reda
album_12_x_5$Title <- gsub("\n", "", album_12_x_5$Title) 
#uklanjamo Composer vrednosti iz kolone Title
album_12_x_5$"Title" <- gsub("/.*", "", album_12_x_5$"Title")

composers <- c("Chuck Berry", "Mick Jagger", "Walter Brown", "Jerry Ragovoy", "Bobby Womack", "Arthur Resnick", "Robert Bateman", "Eleanor Broadwater","Keith Richards")
for (composer in composers) {
  album_12_x_5$Title <- gsub(composer, "", album_12_x_5$Title)
  
}
pesme <- album_12_x_5$Title
#petlja koja prolazi kroz sve pesme iz albuma 12x5 i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_12_x_5[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_12_x_5[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "12 X 5"
}
remove(url_album_12_x_5)
remove(html_album_12_x_5)
remove(album_12_x_5)
################################################################################################
#učitavamo album Aftermath
url_album_aftermath <- "https://www.allmusic.com/album/aftermath-mw0000191599"
html_album_aftermath <- read_html(url_album_aftermath)
album_aftermath <-  html_table(html_album_aftermath, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_aftermath <- album_aftermath[-1]
#brišemo poslednju kolonu
album_aftermath <- album_aftermath[, -ncol(album_aftermath)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_aftermath)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_aftermath)[names(album_aftermath) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_aftermath[album_aftermath == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_aftermath)[names(album_aftermath) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_aftermath
#zamena višestrukih razmaka jednim razmakom
album_aftermath$Title <- gsub("\\s+", " ",album_aftermath$Title)
#uklanjanje znakova novog reda
album_aftermath$Title <- gsub("\n", "", album_aftermath$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_aftermath$"Title" <- gsub("/.*", "", album_aftermath$"Title")

for (composer in composers) {
  album_aftermath$Title <- gsub(composer, "", album_aftermath$Title)
}

#preimenjujemo pesmu Goin' home
table$Title <- ifelse(table$Title == "Goin' Home", "Going Home", table$Title)

pesme <- album_aftermath$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Aftermath i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_aftermath[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_aftermath[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Aftermath"
}
remove(url_album_aftermath)
remove(html_album_aftermath)
remove(album_aftermath)
######################################################################
#učitavamo album Out of Our Heads
# zamenjujemo "Out" u "Out of our Heads" koloni Album names df table
#table$`Album name` <- gsub("Out", "Out of our Heads", table$`Album name`, perl = TRUE)

url_album_out <- "https://www.allmusic.com/album/out-of-our-heads-mw0000191520"
html_album_out <- read_html(url_album_out)
album_out <-  html_table(html_album_out, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_out <- album_out[-1]
#brišemo poslednju kolonu
album_out <- album_out[, -ncol(album_out)]

#preimenujemo sada prvu kolonu u Track number
colnames(album_out)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_out)[names(album_out) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_out[album_out == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_out)[names(album_out) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_out
#zamena višestrukih razmaka jednim razmakom
album_out$Title <- gsub("\\s+", " ",album_out$Title)
#uklanjanje znakova novog reda
album_out$Title <- gsub("\n", "", album_out$Title) 
#uklanjamo Composer vrednosti iz kolone Title
album_out$"Title" <- gsub("/.*", "", album_out$"Title")

#dodavanje novih kompozitora u composers
composers <- c(composers, "Don Covay", "Marvin Gaye", "Roosevelt Jamison", "Sam Cooke", "Ellas McDaniel", "David Hassinger", "Nanker Phelge")

for (composer in composers) {
  album_out$Title <- gsub(composer, "", album_out$Title)
}
pesme <- album_out$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Out of Our Heads i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_out[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_out[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Out of Our Heads"
}
remove(url_album_out)
remove(html_album_out)
remove(album_out)
###########################################
#učitavamo album The Rolling Stones No 2.
url_album_no2 <- "https://www.allmusic.com/album/the-rolling-stones-no-2-mw0001209960"
html_album_no2 <- read_html(url_album_no2)
album_no2 <-  html_table(html_album_no2, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_no2 <- album_no2[-1]
#brišemo poslednju kolonu
album_no2 <- album_no2[, -ncol(album_no2)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_no2)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_no2)[names(album_no2) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_no2[album_no2 == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_no2)[names(album_no2) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_no2
#zamena višestrukih razmaka jednim razmakom
album_no2$Title <- gsub("\\s+", " ",album_no2$Title)
#uklanjanje znakova novog reda
album_no2$Title <- gsub("\n", "", album_no2$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_no2$"Title" <- gsub("/.*", "", album_no2$"Title")

composers <- c(composers, "Bert Berns","Arthur Butler", "Don Raye","Muddy Waters","Naomi Neville")

for (composer in composers) {
  album_no2$Title <- gsub(composer, "", album_no2$Title)
}

pesme <- album_no2$`Title`
#petlja koja prolazi kroz sve pesme iz albuma The Rolling Stones No 2.  i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_no2[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_no2[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "The Rolling Stones No.2"
}
remove(url_album_no2)
remove(html_album_no2)
remove(album_no2)
##########################################################################
#učitavamo album Some girls
url_album_somegirls <- "https://www.allmusic.com/album/some-girls-mw0000191642"
html_album_somegirls <- read_html(url_album_somegirls)
album_somegirls <-  html_table(html_album_somegirls, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_somegirls <- album_somegirls[-1]
#brišemo poslednju kolonu
album_somegirls <- album_somegirls[, -ncol(album_somegirls)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_somegirls)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_somegirls)[names(album_somegirls) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_somegirls[album_somegirls == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_somegirls)[names(album_somegirls) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_no2
#zamena višestrukih razmaka jednim razmakom
album_somegirls$Title <- gsub("\\s+", " ",album_somegirls$Title)
#uklanjanje znakova novog reda
album_somegirls$Title <- gsub("\n", "", album_somegirls$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_somegirls$"Title" <- gsub("/.*", "", album_somegirls$"Title")

composers <- c(composers, "Barrett Strong")

for (composer in composers) {
  album_somegirls$Title <- gsub(composer, "", album_somegirls$Title)
}

pesme <- album_somegirls$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Some girls  i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_somegirls[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_somegirls[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Some Girls"
}
#brisemo Some Girls(reissue) redove
table <- table[!grepl("Some Girls \\(reissue\\)", table$`Album name`), ]
remove(url_album_somegirls)
remove(html_album_somegirls)
remove(album_somegirls)
##########################################################################
#učitavamo album Emotional Rescue
url_album_emotional <- "https://www.allmusic.com/album/emotional-rescue-mw0000650700"
html_album_emotional <- read_html(url_album_emotional)
album_emotional <-  html_table(html_album_emotional, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_emotional <- album_emotional[-1]
#brišemo poslednju kolonu
album_emotional <- album_emotional[, -ncol(album_emotional)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_emotional)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_emotional)[names(album_emotional) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_emotional[album_emotional == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_emotional)[names(album_emotional) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_emotional
#zamena višestrukih razmaka jednim razmakom
album_emotional$Title <- gsub("\\s+", " ",album_emotional$Title)
#uklanjanje znakova novog reda
album_emotional$Title <- gsub("\n", "", album_emotional$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_emotional$"Title" <- gsub("/.*", "", album_emotional$"Title")


for (composer in composers) {
  album_emotional$Title <- gsub(composer, "", album_emotional$Title)
}

pesme <- album_emotional$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Emotional Rescue  i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_emotional[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_emotional[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Emotional Rescue"
}
remove(url_album_emotional)
remove(html_album_emotional)
remove(album_emotional)
##########################################################################
#učitavamo album Tattoo You
url_album_tattoo <- "https://www.allmusic.com/album/tattoo-you-mw0000191643"
html_album_tattoo <- read_html(url_album_tattoo)
album_tattoo <-  html_table(html_album_tattoo, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_tattoo <- album_tattoo[-1]
#brišemo poslednju kolonu
album_tattoo <- album_tattoo[, -ncol(album_tattoo)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_tattoo)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_tattoo)[names(album_tattoo) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_tattoo[album_tattoo == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_tattoo)[names(album_tattoo) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_tattoo
#zamena višestrukih razmaka jednim razmakom
album_tattoo$Title <- gsub("\\s+", " ",album_tattoo$Title)
#uklanjanje znakova novog reda
album_tattoo$Title <- gsub("\n", "", album_tattoo$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_tattoo$"Title" <- gsub("/.*", "", album_tattoo$"Title")


for (composer in composers) {
  album_tattoo$Title <- gsub(composer, "", album_tattoo$Title)
}

pesme <- album_tattoo$`Title`
#petlja koja prolazi kroz sve pesme iz albuma  Tattoo You i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_tattoo[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_tattoo[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "Tattoo You"
}
#brisemo Tattoo You (reissue)
table <- table[!grepl("Tattoo You \\(reissue\\)", table$`Album name`), ]
remove(url_album_tattoo)
remove(html_album_tattoo)
remove(album_tattoo)
##########################################################################
#učitavamo album Still Life
url_album_stlife <- "https://www.allmusic.com/album/still-life-mw0000191521"
html_album_stlife <- read_html(url_album_stlife)
album_stlife <-  html_table(html_album_stlife, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_stlife <- album_stlife[-1]
#brišemo poslednju kolonu
album_stlife <- album_stlife[, -ncol(album_stlife)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_stlife)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_stlife)[names(album_stlife) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_stlife[album_stlife == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_stlife)[names(album_stlife) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_tattoo
#zamena višestrukih razmaka jednim razmakom
album_stlife$Title <- gsub("\\s+", " ",album_stlife$Title)
#uklanjanje znakova novog reda
album_stlife$Title <- gsub("\n", "", album_stlife$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_stlife$"Title" <- gsub("/.*", "", album_stlife$"Title")

composers <- c(composers,"Billy Strayhorn","Warren \"Pete\" Moore","Francis Scott Key","Eddie Cochran")

for (composer in composers) {
  album_stlife$Title <- gsub(composer, "", album_stlife$Title)
}

pesme <- album_stlife$`Title`
#petlja koja prolazi kroz sve pesme iz albuma  Still Life i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_stlife[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_stlife[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Still Life"
}
remove(url_album_stlife)
remove(html_album_stlife)
remove(album_stlife)
##########################################################################
#učitavamo album Undercover
url_album_under <- "https://www.allmusic.com/album/undercover-mw0000195500"
html_album_under <- read_html(url_album_under)
album_under <-  html_table(html_album_under, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_under <- album_under[-1]
#brišemo poslednju kolonu
album_under <- album_under[, -ncol(album_under)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_under)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_under)[names(album_under) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_under[album_under == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_under)[names(album_under) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_under
#zamena višestrukih razmaka jednim razmakom
album_under$Title <- gsub("\\s+", " ",album_under$Title)
#uklanjanje znakova novog reda
album_under$Title <- gsub("\n", "", album_under$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_under$"Title" <- gsub("/.*", "", album_under$"Title")

for (composer in composers) {
  album_under$Title <- gsub(composer, "", album_under$Title)
}

pesme <- album_under$`Title`
#petlja koja prolazi kroz sve pesme iz albuma  Undercover i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_under[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_under[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "Undercover"
}
remove(url_album_under)
remove(html_album_under)
remove(album_under)
##########################################################################
#učitavamo album Dirty Work
url_album_dirty <- "https://www.allmusic.com/album/dirty-work-mw0000191517"
html_album_dirty <- read_html(url_album_dirty)
album_dirty<-  html_table(html_album_dirty, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_dirty <- album_dirty[-1]
#brišemo poslednju kolonu
album_dirty <- album_dirty[, -ncol(album_dirty)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_dirty)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_dirty)[names(album_dirty) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_dirty[album_dirty == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_dirty)[names(album_dirty) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_dirty
#zamena višestrukih razmaka jednim razmakom
album_dirty$Title <- gsub("\\s+", " ",album_dirty$Title)
#uklanjanje znakova novog reda
album_dirty$Title <- gsub("\n", "", album_dirty$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_dirty$"Title" <- gsub("/.*", "", album_dirty$"Title")

composers <- c(composers,"Earl Nelson","Lindon Roberts")

for (composer in composers) {
  album_dirty$Title <- gsub(composer, "", album_dirty$Title)
}

pesme <- album_dirty$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Dirty Work i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  #indeks <- which(grepl(paste0("(?i)", pesme[i]), table$Title))
  table[indeks,]$`Track number` <- album_dirty[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_dirty[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "Dirty Work"
}
remove(url_album_dirty)
remove(html_album_dirty)
remove(album_dirty)
##########################################################################
#učitavamo album Steel Wheels
url_album_steel <- "https://www.allmusic.com/album/steel-wheels-mw0000653629"
html_album_steel <- read_html(url_album_steel)
album_steel<-  html_table(html_album_steel, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_steel <- album_steel[-1]
#brišemo poslednju kolonu
album_steel <- album_steel[, -ncol(album_steel)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_steel)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_steel)[names(album_steel) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_steel[album_steel == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_steel)[names(album_steel) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_dirty
#zamena višestrukih razmaka jednim razmakom
album_steel$Title <- gsub("\\s+", " ",album_steel$Title)
#uklanjanje znakova novog reda
album_steel$Title <- gsub("\n", "", album_steel$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_steel$"Title" <- gsub("/.*", "", album_steel$"Title")

for (composer in composers) {
  album_steel$Title <- gsub(composer, "", album_steel$Title)
}

pesme <- album_steel$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Steel Wheels i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  #indeks <- which(grepl(paste0("(?i)", pesme[i]), table$Title))
  table[indeks,]$`Track number` <- album_steel[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_steel[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "Steel Wheels"
}
remove(url_album_steel)
remove(html_album_steel)
remove(album_steel)
##########################################################################
#učitavamo album Flashpoint
url_album_flash <- "https://www.allmusic.com/album/flashpoint-mw0000262612"
html_album_flash <- read_html(url_album_flash)
album_flash<-  html_table(html_album_flash, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_flash <- album_flash[-1]
#brišemo poslednju kolonu
album_flash <- album_flash[, -ncol(album_flash)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_flash)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_flash)[names(album_flash) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_flash[album_flash == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_flash)[names(album_flash) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_flash$Title <- gsub("\\s+", " ",album_flash$Title)
#uklanjanje znakova novog reda
album_flash$Title <- gsub("\n", "", album_flash$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_flash$"Title" <- gsub("/.*", "", album_flash$"Title")

composers <-c(composers,"Chester Burnett")
for (composer in composers) {
  album_flash$Title <- gsub(composer, "", album_flash$Title)
}

pesme <- album_flash$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Flashpoint i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  #indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  indeks <- which(grepl(tolower(gsub("\\s+\\(Intro\\)|\\s+", "", pesme[i])), tolower(gsub("\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_flash[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_flash[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Flashpoint"
}
remove(url_album_flash)
remove(html_album_flash)
remove(album_flash)
##########################################################################
#učitavamo album Voodoo Lounge
url_album_vodo <- "https://www.allmusic.com/album/voodoo-lounge-mw0000178891"
html_album_vodo <- read_html(url_album_vodo)
album_vodo<-  html_table(html_album_vodo, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_vodo <- album_vodo[-1]
#brišemo poslednju kolonu
album_vodo <- album_vodo[, -ncol(album_vodo)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_vodo)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_vodo)[names(album_vodo) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_vodo[album_vodo == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_vodo)[names(album_vodo) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_vodo$Title <- gsub("\\s+", " ",album_vodo$Title)
#uklanjanje znakova novog reda
album_vodo$Title <- gsub("\n", "", album_vodo$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_vodo$"Title" <- gsub("/.*", "", album_vodo$"Title")


for (composer in composers) {
  album_vodo$Title <- gsub(composer, "", album_vodo$Title)
}

pesme <- album_vodo$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Voodoo Lounge i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_vodo[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_vodo[i, ]$`Song duration`
  #table[indeks,]$`Album name` <- "Voodoo Lounge"
}
remove(url_album_vodo)
remove(html_album_vodo)
remove(album_vodo)
##########################################################################
#učitavamo album Stripped
url_album_stripe <- "https://www.allmusic.com/album/stripped-mw0000173022"
html_album_stripe<- read_html(url_album_stripe)
album_stripe<-  html_table(html_album_stripe, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_stripe <- album_stripe[-1]
#brišemo poslednju kolonu
album_stripe <- album_stripe[, -ncol(album_stripe)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_stripe)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_stripe)[names(album_stripe) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_stripe[album_stripe == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_stripe)[names(album_stripe) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_stripe$Title <- gsub("\\s+", " ",album_stripe$Title)
#uklanjanje znakova novog reda
album_stripe$Title <- gsub("\n", "", album_stripe$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_stripe$"Title" <- gsub("/.*", "", album_stripe$"Title")

composers <-c(composers,"Bob Dylan","Buddy Holly","Robert Johnson","Willie Dixon")

for (composer in composers) {
  album_stripe$Title <- gsub(composer, "", album_stripe$Title)
}

#table$Title <- gsub("\\(live\\)", "", table$Title)

pesme <- album_stripe$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Stripped i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  #indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_stripe[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_stripe[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Stripped"
}
remove(url_album_stripe)
remove(html_album_stripe)
remove(album_stripe)
##########################################################################
#učitavamo album Bridges of Babylon
url_album_bridges <- "https://www.allmusic.com/album/bridges-to-babylon-mw0000026729"
html_album_bridges<- read_html(url_album_bridges)
album_bridges<-  html_table(html_album_bridges, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_bridges <- album_bridges[-1]
#brišemo poslednju kolonu
album_bridges <- album_bridges[, -ncol(album_bridges)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_bridges)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_bridges)[names(album_bridges) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_bridges[album_bridges == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_bridges)[names(album_bridges) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_bridges$Title <- gsub("\\s+", " ",album_bridges$Title)
#uklanjanje znakova novog reda
album_bridges$Title <- gsub("\n", "", album_bridges$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_bridges$"Title" <- gsub("/.*", "", album_bridges$"Title")

composers <- c(composers,"Pierre de Beauport")

for (composer in composers) {
  album_bridges$Title <- gsub(composer, "", album_bridges$Title)
}

pesme <- album_bridges$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Bridges of Babylon i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_bridges[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_bridges[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Bridges to Babylon"
}
remove(url_album_bridges)
remove(html_album_bridges)
remove(album_bridges)
##########################################################################
#učitavamo album No Security
url_album_nosecure <- "https://www.allmusic.com/album/no-security-mw0000600850"
html_album_nosecure<- read_html(url_album_nosecure)
album_nosecure<-  html_table(html_album_nosecure, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_nosecure <- album_nosecure[-1]
#brišemo poslednju kolonu
album_nosecure <- album_nosecure[, -ncol(album_nosecure)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_nosecure)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_nosecure)[names(album_nosecure) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_nosecure[album_nosecure == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_nosecure)[names(album_nosecure) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_nosecure$Title <- gsub("\\s+", " ",album_nosecure$Title)
#uklanjanje znakova novog reda
album_nosecure$Title <- gsub("\n", "", album_nosecure$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_nosecure$"Title" <- gsub("/.*", "", album_nosecure$"Title")

composers <- c(composers,"Jesse","Marianne Faithfull")

for (composer in composers) {
  album_nosecure$Title <- gsub(composer, "", album_nosecure$Title)
}
pesme <- album_nosecure$`Title`
#petlja koja prolazi kroz sve pesme iz albuma No Security i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  #indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_nosecure[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_nosecure[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "No Security"
}
remove(url_album_nosecure)
remove(html_album_nosecure)
remove(album_nosecure)
##########################################################################
#učitavamo album Live Licks
url_album_licks <- "https://www.allmusic.com/album/live-licks-mw0000139531"
html_album_licks<- read_html(url_album_licks)
album_licks<-  html_table(html_album_licks, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_licks <- album_licks[-1]
#brišemo poslednju kolonu
album_licks <- album_licks[, -ncol(album_licks)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_licks)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_licks)[names(album_licks) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_licks[album_licks == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_licks)[names(album_licks) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_licks$Title <- gsub("\\s+", " ",album_licks$Title)
#uklanjanje znakova novog reda
album_licks$Title <- gsub("\n", "", album_licks$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_licks$"Title" <- gsub("/.*", "", album_licks$"Title")

for (composer in composers) {
  album_licks$Title <- gsub(composer, "", album_licks$Title)
}

pesme <- album_licks$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Live Licks i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_licks[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_licks[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Live Licks"
}
remove(url_album_licks)
remove(html_album_licks)
remove(album_licks)
##########################################################################
#učitavamo album A Bigger Bang
url_album_bang <- "https://www.allmusic.com/album/a-bigger-bang-mw0000408720"
html_album_bang<- read_html(url_album_bang)
album_bang<-  html_table(html_album_bang, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_bang <- album_bang[-1]
#brišemo poslednju kolonu
album_bang <- album_bang[, -ncol(album_bang)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_bang)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_bang)[names(album_bang) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_bang[album_bang == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_bang)[names(album_bang) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_bang$Title <- gsub("\\s+", " ",album_bang$Title)
#uklanjanje znakova novog reda
album_bang$Title <- gsub("\n", "", album_bang$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_bang$"Title" <- gsub("/.*", "", album_bang$"Title")

for (composer in composers) {
  album_bang$Title <- gsub(composer, "", album_bang$Title)
}

pesme <- album_bang$`Title`
#petlja koja prolazi kroz sve pesme iz albuma A Bigger Bang i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_bang[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_bang[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "A Bigger Bang"
}
remove(url_album_bang)
remove(html_album_bang)
remove(album_bang)
##########################################################################
#učitavamo album Shine A Light
url_album_shine <- "https://www.allmusic.com/album/shine-a-light-mw0000496913"
html_album_shine<- read_html(url_album_shine)
album_shine<-  html_table(html_album_shine, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_shine <- album_shine[-1]
#brišemo poslednju kolonu
album_shine <- album_shine[, -ncol(album_shine)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_shine)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_shine)[names(album_shine) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_shine[album_shine == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_shine)[names(album_shine) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_shine$Title <- gsub("\\s+", " ",album_shine$Title)
#uklanjanje znakova novog reda
album_shine$Title <- gsub("\n", "", album_shine$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_shine$"Title" <- gsub("/.*", "", album_shine$"Title")

composers <- c(composers,"McKinley Morganfield")
for (composer in composers) {
  album_shine$Title <- gsub(composer, "", album_shine$Title)
}

album_shine$Title <- gsub("&", "and", album_shine$Title)

pesme <- album_shine$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Shine a Light i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_shine[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_shine[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Shine a Light"
}
remove(url_album_shine)
remove(html_album_shine)
remove(album_shine)
##########################################################################
#učitavamo album Blue & Lonesome
url_album_blue <- "https://www.allmusic.com/album/blue-lonesome-mw0002988671"
html_album_blue<- read_html(url_album_blue)
album_blue<-  html_table(html_album_blue, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_blue <- album_blue[-1]
#brišemo poslednju kolonu
album_blue <- album_blue[, -ncol(album_blue)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_blue)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_blue)[names(album_blue) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_blue[album_blue == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_blue)[names(album_blue) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_blue$Title <- gsub("\\s+", " ",album_blue$Title)
#uklanjanje znakova novog reda
album_blue$Title <- gsub("\n", "", album_blue$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_blue$"Title" <- gsub("/.*", "", album_blue$"Title")

composers <- c(composers,"Walter Jacobs","Chester Burnett","Walter Jacobs","Samuel Maghett",
               "Miles Grayson","Eddie Taylor","Otis Hicks","Ewart Abner")

for (composer in composers) {
  album_blue$Title <- gsub(composer, "", album_blue$Title)
}

pesme <- album_blue$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Blue & Lonesome i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_blue[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_blue[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Blue & Lonesome"
}
remove(url_album_blue)
remove(html_album_blue)
remove(album_blue)
##########################################################################
#učitavamo album The Rolling Stones, Now!
url_album_now <- "https://www.allmusic.com/album/the-rolling-stones-now%21-mw0000191522"
html_album_now<- read_html(url_album_now)
album_now<-  html_table(html_album_now, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_now <- album_now[-1]
#brišemo poslednju kolonu
album_now <- album_now[, -ncol(album_now)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_now)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_now)[names(album_now) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_now[album_now == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_now)[names(album_now) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_now$Title <- gsub("\\s+", " ",album_now$Title)
#uklanjanje znakova novog reda
album_now$Title <- gsub("\n", "", album_now$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_now$"Title" <- gsub("/.*", "", album_now$"Title")
composers <- c(composers,"Barbara Lynn Ozen")

for (composer in composers) {
  album_now$Title <- gsub(composer, "", album_now$Title)
}

pesme <- album_now$`Title`
#petlja koja prolazi kroz sve pesme iz albuma The Rolling Stones, Now! i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_now[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_now[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "The Rolling Stones, Now!"
}
remove(url_album_now)
remove(html_album_now)
remove(album_now)
##########################################################################
#učitavamo album December's Children (And Everybody's)
url_album_dece <- "https://www.allmusic.com/album/decembers-children-and-everybodys-mw0000195494"
html_album_dece<- read_html(url_album_dece)
album_dece<-  html_table(html_album_dece, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_dece <- album_dece[-1]
#brišemo poslednju kolonu
album_dece <- album_dece[, -ncol(album_dece)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_dece)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_dece)[names(album_dece) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_dece[album_dece == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_dece)[names(album_dece) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_dece$Title <- gsub("\\s+", " ",album_dece$Title)
#uklanjanje znakova novog reda
album_dece$Title <- gsub("\n", "", album_dece$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_dece$"Title" <- gsub("/.*", "", album_dece$"Title")
composers <- c(composers,"Sonny Bono","Arthur Alexander","McKinley Morganfield","Hank Snow")

for (composer in composers) {
  album_dece$Title <- gsub(composer, "", album_dece$Title)
}
pesme <- album_dece$`Title`
#petlja koja prolazi kroz sve pesme iz albuma December's Children (And Everybody's) i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_dece[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_dece[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "December's Children (And Everybody's)"
}
remove(url_album_dece)
remove(html_album_dece)
remove(album_dece)
##########################################################################
#učitavamo album Got Live If You Want It!
url_album_want <- "https://www.allmusic.com/album/got-live-if-you-want-it%21-mw0000191640"
html_album_want<- read_html(url_album_want)
album_want<-  html_table(html_album_want, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_want <- album_want[-1]
#brišemo poslednju kolonu
album_want <- album_want[, -ncol(album_want)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_want)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_want)[names(album_want) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_want[album_want == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_want)[names(album_want) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_want$Title <- gsub("\\s+", " ",album_want$Title)
#uklanjanje znakova novog reda
album_want$Title <- gsub("\n", "", album_want$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_want$"Title" <- gsub("/.*", "", album_want$"Title")

composers <- c(composers,"Jerry Butler")
for (composer in composers) {
  album_want$Title <- gsub(composer, "", album_want$Title)
}

pesme <- album_want$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Got Live If You Want It! i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(grepl(tolower(gsub("\\s+\\(To Stop Now\\)|\\s+", "", pesme[i])), tolower(gsub("\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_want[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_want[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Got Live If You Want It!"
}
remove(url_album_want)
remove(html_album_want)
remove(album_want)
##########################################################################
#učitavamo album Between the Buttons
url_album_betbut <- "https://www.allmusic.com/album/between-the-buttons-mw0000191516"
html_album_betbut<- read_html(url_album_betbut)
album_betbut<-  html_table(html_album_betbut, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_betbut <- album_betbut[-1]
#brišemo poslednju kolonu
album_betbut <- album_betbut[, -ncol(album_betbut)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_betbut)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_betbut)[names(album_betbut) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_betbut[album_betbut == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_betbut)[names(album_betbut) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_betbut$Title <- gsub("\\s+", " ",album_betbut$Title)
#uklanjanje znakova novog reda
album_betbut$Title <- gsub("\n", "", album_betbut$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_betbut$"Title" <- gsub("/.*", "", album_betbut$"Title")

for (composer in composers) {
  album_betbut$Title <- gsub(composer, "", album_betbut$Title)
}
pesme <- album_betbut$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Between the Buttons i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_betbut[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_betbut[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Between the Buttons"
}
remove(url_album_betbut)
remove(html_album_betbut)
remove(album_betbut)
##########################################################################
#učitavamo album Flowers
url_album_flower <- "https://www.allmusic.com/album/flowers-mw0000195495"
html_album_flower<- read_html(url_album_flower)
album_flower<-  html_table(html_album_flower, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_flower <- album_flower[-1]
#brišemo poslednju kolonu
album_flower <- album_flower[, -ncol(album_flower)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_flower)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_flower)[names(album_flower) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_flower[album_flower == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_flower)[names(album_flower) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_flower$Title <- gsub("\\s+", " ",album_flower$Title)
#uklanjanje znakova novog reda
album_flower$Title <- gsub("\n", "", album_flower$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_flower$"Title" <- gsub("/.*", "", album_flower$"Title")

composers <- c(composers,"Smokey Robinson")

for (composer in composers) {
  album_flower$Title <- gsub(composer, "", album_flower$Title)
}
#sredjujemo pesmu u tableli album_flower
album_flower[11,]$Title <-"Ride On, Baby"

#string1_clean <- gsub("[[:punct:]]", "", string1)
pesme <- album_flower$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Flowers i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  #indeks <- which(grepl(tolower(gsub("[[:punct:]]", "", pesme[i])), tolower(gsub("[[:punct:]]", "", table$Title))))
  table[indeks,]$`Track number` <- album_flower[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_flower[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Flowers"
}
remove(url_album_flower)
remove(html_album_flower)
remove(album_flower)
##########################################################################
#učitavamo album Their Satanic Majesties Request
url_album_satanic <- "https://www.allmusic.com/album/their-satanic-majesties-request-mw0000650705"
html_album_satanic<- read_html(url_album_satanic)
album_satanic<-  html_table(html_album_satanic, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_satanic <- album_satanic[-1]
#brišemo poslednju kolonu
album_satanic <- album_satanic[, -ncol(album_satanic)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_satanic)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_satanic)[names(album_satanic) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_satanic[album_satanic == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_satanic)[names(album_satanic) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_satanic$Title <- gsub("\\s+", " ",album_satanic$Title)
#uklanjanje znakova novog reda
album_satanic$Title <- gsub("\n", "", album_satanic$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_satanic$"Title" <- gsub("/.*", "", album_satanic$"Title")
composers <- c(composers,"Bill Wyman")

for (composer in composers) {
  album_satanic$Title <- gsub(composer, "", album_satanic$Title)
}
pesme <- album_satanic$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Their Satanic Majesties Request i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_satanic[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_satanic[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Their Satanic Majesties Request"
}
remove(url_album_satanic)
remove(html_album_satanic)
remove(album_satanic)
##########################################################################
#učitavamo album Beggars Banquet
url_album_banq<- "https://www.allmusic.com/album/beggars-banquet-mw0000195493"
html_album_banq<- read_html(url_album_banq)
album_banq<-  html_table(html_album_banq, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_banq <- album_banq[-1]
#brišemo poslednju kolonu
album_banq <- album_banq[, -ncol(album_banq)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_banq)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_banq)[names(album_banq) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_banq[album_banq == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_banq)[names(album_banq) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_banq$Title <- gsub("\\s+", " ",album_banq$Title)
#uklanjanje znakova novog reda
album_banq$Title <- gsub("\n", "", album_banq$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_banq$"Title" <- gsub("/.*", "", album_banq$"Title")
composers <- c(composers,"Robert Wilkins")

for (composer in composers) {
  album_banq$Title <- gsub(composer, "", album_banq$Title)
}
pesme <- album_banq$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Beggars Banquet i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_banq[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_banq[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Beggars Banquet"
}
remove(url_album_banq)
remove(html_album_banq)
remove(album_banq)
##########################################################################
#učitavamo album Let It Bleed
url_album_bleed<- "https://www.allmusic.com/album/let-it-bleed-mw0000191519"
html_album_bleed<- read_html(url_album_bleed)
album_bleed<-  html_table(html_album_bleed, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_bleed <- album_bleed[-1]
#brišemo poslednju kolonu
album_bleed <- album_bleed[, -ncol(album_bleed)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_bleed)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_bleed)[names(album_bleed) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_bleed[album_bleed == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_bleed)[names(album_bleed) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_bleed$Title <- gsub("\\s+", " ",album_bleed$Title)
#uklanjanje znakova novog reda
album_bleed$Title <- gsub("\n", "", album_bleed$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_bleed$"Title" <- gsub("/.*", "", album_bleed$"Title")

for (composer in composers) {
  album_bleed$Title <- gsub(composer, "", album_bleed$Title)
}
pesme <- album_bleed$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Let It Bleed i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_bleed[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_bleed[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Let It Bleed"
}
remove(url_album_bleed)
remove(html_album_bleed)
remove(album_bleed)
##########################################################################
#učitavamo album Get Yer Ya-Ya's Out!
url_album_yer<- "https://www.allmusic.com/album/get-yer-ya-yas-out%21-mw0000191518"
html_album_yer<- read_html(url_album_yer)
album_yer<-  html_table(html_album_yer, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_yer <- album_yer[-1]
#brišemo poslednju kolonu
album_yer <- album_yer[, -ncol(album_yer)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_yer)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_yer)[names(album_yer) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_yer[album_yer == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_yer)[names(album_yer) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_yer$Title <- gsub("\\s+", " ",album_yer$Title)
#uklanjanje znakova novog reda
album_yer$Title <- gsub("\n", "", album_yer$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_yer$"Title" <- gsub("/.*", "", album_yer$"Title")

for (composer in composers) {
  album_yer$Title <- gsub(composer, "", album_yer$Title)
}
pesme <- album_yer$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Get Yer Ya-Ya's Out! i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(grepl(tolower(gsub("\\s+", "", pesme[i])), tolower(gsub("\\s+\\(live\\)|\\s+", "", table$Title))))
  table[indeks,]$`Track number` <- album_yer[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_yer[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Get Yer Ya-Ya's Out!"
}
remove(url_album_yer)
remove(html_album_yer)
remove(album_yer)
##########################################################################
#učitavamo album Sticky Fingers
url_album_fingers<- "https://www.allmusic.com/album/sticky-fingers-mw0000195498"
html_album_fingers<- read_html(url_album_fingers)
album_fingers<-  html_table(html_album_fingers, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_fingers <- album_fingers[-1]
#brišemo poslednju kolonu
album_fingers <- album_fingers[, -ncol(album_fingers)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_fingers)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_fingers)[names(album_fingers) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_fingers[album_fingers == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_fingers)[names(album_fingers) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_fingers$Title <- gsub("\\s+", " ",album_fingers$Title)
#uklanjanje znakova novog reda
album_fingers$Title <- gsub("\n", "", album_fingers$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_fingers$"Title" <- gsub("/.*", "", album_fingers$"Title")

composers <- c(composers,"Rev. Gary Davis")
for (composer in composers) {
  album_fingers$Title <- gsub(composer, "", album_fingers$Title)
}
pesme <- album_fingers$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Sticky Fingers i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_fingers[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_fingers[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Sticky Fingers"
}
remove(url_album_fingers)
remove(html_album_fingers)
remove(album_fingers)
##########################################################################
#učitavamo album Exile on Main St.
url_album_exile<- "https://www.allmusic.com/album/exile-on-main-st-mw0000191639"
html_album_exile<- read_html(url_album_exile)
album_exile<-  html_table(html_album_exile, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_exile <- album_exile[-1]
#brišemo poslednju kolonu
album_exile <- album_exile[, -ncol(album_exile)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_exile)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_exile)[names(album_exile) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_exile[album_exile == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_exile)[names(album_exile) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_flash
#zamena višestrukih razmaka jednim razmakom
album_exile$Title <- gsub("\\s+", " ",album_exile$Title)
#uklanjanje znakova novog reda
album_exile$Title <- gsub("\n", "", album_exile$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_exile$"Title" <- gsub("/.*", "", album_exile$"Title")
composers  <- c(composers,"Slim Harpo")

for (composer in composers) {
  album_exile$Title <- gsub(composer, "", album_exile$Title)
}
pesme <- album_exile$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Exile on Main St. i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_exile[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_exile[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Exile on Main St."
}
#brisemo Exile on Main St.(reissue)
table <- table[!grepl("Exile on Main St. \\(reissue\\)", table$`Album name`), ]
remove(url_album_exile)
remove(html_album_exile)
remove(album_exile)
##########################################################################
#učitavamo album Goats Head Soup
url_album_goats<- "https://www.allmusic.com/album/goats-head-soup-mw0000650701"
html_album_goats<- read_html(url_album_goats)
album_goats<-  html_table(html_album_goats, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_goats <- album_goats[-1]
#brišemo poslednju kolonu
album_goats <- album_goats[, -ncol(album_goats)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_goats)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_goats)[names(album_goats) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_goats[album_goats == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_goats)[names(album_goats) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_goats
#zamena višestrukih razmaka jednim razmakom
album_goats$Title <- gsub("\\s+", " ",album_goats$Title)
#uklanjanje znakova novog reda
album_goats$Title <- gsub("\n", "", album_goats$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_goats$"Title" <- gsub("/.*", "", album_goats$"Title")


for (composer in composers) {
  album_goats$Title <- gsub(composer, "", album_goats$Title)
}
#promeni pesmu Star Star(Starfucker) u Star Star
table$`Title` <- gsub(".*(Starfucker).*", "Star Star", table$`Title`)

#menjamo Dancing With Mr D  u Dancing With Mr. D
album_goats[1,]$Title <-"Dancing With Mr. D "
album_goats[7,]$Title <- "Hide Your Love "
pesme <- album_goats$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Goats Head Soup i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_goats[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_goats[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Goats Head Soup"
}
#brisemo Goats Head Soup(reissue)
table <- table[!grepl("Goats Head Soup \\(reissue\\)", table$`Album name`), ]
remove(url_album_goats)
remove(html_album_goats)
remove(album_goats)
##########################################################################
#učitavamo album It's Only Rock 'n' Roll
url_album_onlyroll<- "https://www.allmusic.com/album/its-only-rock-n-roll-mw0000195496"
html_album_onlyroll<- read_html(url_album_onlyroll)
album_onlyroll<-  html_table(html_album_onlyroll, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_onlyroll <- album_onlyroll[-1]
#brišemo poslednju kolonu
album_onlyroll <- album_onlyroll[, -ncol(album_onlyroll)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_onlyroll)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_onlyroll)[names(album_onlyroll) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_onlyroll[album_onlyroll == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_onlyroll)[names(album_onlyroll) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_onlyroll
#zamena višestrukih razmaka jednim razmakom
album_onlyroll$Title <- gsub("\\s+", " ",album_onlyroll$Title)
#uklanjanje znakova novog reda
album_onlyroll$Title <- gsub("\n", "", album_onlyroll$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_onlyroll$"Title" <- gsub("/.*", "", album_onlyroll$"Title")

composers <- c(composers,"Eddie Holland")
for (composer in composers) {
  album_onlyroll$Title <- gsub(composer, "", album_onlyroll$Title)
}

pesme <- album_onlyroll$`Title`
#petlja koja prolazi kroz sve pesme iz albuma It's Only Rock 'n' Roll i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_onlyroll[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_onlyroll[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "It's Only Rock 'n' Roll"
}
remove(url_album_onlyroll)
remove(html_album_onlyroll)
remove(album_onlyroll)
##########################################################################
#učitavamo album Black and Blue
url_album_black<- "https://www.allmusic.com/album/black-and-blue-mw0000191638"
html_album_black<- read_html(url_album_black)
album_black<-  html_table(html_album_black, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_black <- album_black[-1]
#brišemo poslednju kolonu
album_black <- album_black[, -ncol(album_black)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_black)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_black)[names(album_black) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_black[album_black == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_black)[names(album_black) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_onlyroll
#zamena višestrukih razmaka jednim razmakom
album_black$Title <- gsub("\\s+", " ",album_black$Title)
#uklanjanje znakova novog reda
album_black$Title <- gsub("\n", "", album_black$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_black$"Title" <- gsub("/.*", "", album_black$"Title")

composers <- c(composers,"Eric Donaldson")
for (composer in composers) {
  album_black$Title <- gsub(composer, "", album_black$Title)
}

pesme <- album_black$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Black and Blue i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_black[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_black[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Black and Blue"
}
remove(url_album_black)
remove(html_album_black)
remove(album_black)
##########################################################################
#učitavamo album The Rolling Stones, Now!
url_album_now<- "https://www.allmusic.com/album/the-rolling-stones-now%21-mw0000191522"
html_album_now<- read_html(url_album_now)
album_now<-  html_table(html_album_now, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_now <- album_now[-1]
#brišemo poslednju kolonu
album_now <- album_now[, -ncol(album_now)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_now)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_now)[names(album_now) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_now[album_now == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_now)[names(album_now) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_onlyroll
#zamena višestrukih razmaka jednim razmakom
album_now$Title <- gsub("\\s+", " ",album_now$Title)
#uklanjanje znakova novog reda
album_now$Title <- gsub("\n", "", album_now$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_now$"Title" <- gsub("/.*", "", album_now$"Title")
composers <- c(composers," Barbara Lynn Ozen")

for (composer in composers) {
  album_now$Title <- gsub(composer, "", album_now$Title)
}
album_now[10,]$Title <- "Oh, Baby (We Got a Good Thing Going)"
pesme <- album_now$`Title`
#petlja koja prolazi kroz sve pesme iz albuma The Rolling Stones, Now! i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_now[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_now[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "The Rolling Stones, Now!"
}
remove(url_album_now)
remove(html_album_now)
remove(album_now)
##########################################################################
#učitavamo album Rolling Stones Chronicles
url_album_chronicals<- "https://www.allmusic.com/album/rolling-stones-chronicles-mw0003989400"
html_album_chronicals<- read_html(url_album_chronicals)
album_chronicals<-  html_table(html_album_chronicals, fill = TRUE)[[1]]

#brišemo prvu kolonu
album_chronicals <- album_chronicals[-1]
#brišemo poslednju kolonu
album_chronicals_now <- album_chronicals[, -ncol(album_chronicals)]
#preimenujemo sada prvu kolonu u Track number
colnames(album_chronicals)[1] <- "Track number"
#preimenujemo kolonu Time u Song duration
names(album_chronicals)[names(album_chronicals) == "Time"] <- "Song duration"
#proveravamo sve Na vrednosti
album_chronicals[album_chronicals == ""] <- NA

#preimenujemo kolonu Title/Composer u Title
names(album_chronicals)[names(album_chronicals) == "Title/Composer"] <- "Title"

#sredjujemo kolonu Title u album_onlyroll
#zamena višestrukih razmaka jednim razmakom
album_chronicals$Title <- gsub("\\s+", " ",album_chronicals$Title)
#uklanjanje znakova novog reda
album_chronicals$Title <- gsub("\n", "", album_chronicals$Title) 

#uklanjamo Composer vrednosti iz kolone Title
album_chronicals$"Title" <- gsub("/.*", "", album_chronicals$"Title")

for (composer in composers) {
  album_chronicals$Title <- gsub(composer, "", album_chronicals$Title)
}

pesme <- album_chronicals$`Title`
#petlja koja prolazi kroz sve pesme iz albuma Rolling Stones Chronicles i dodaje duzinu trajanja pesama i njihov broj na albumu u data frame table
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", table$Title))== tolower(gsub("\\s+", "", pesme[i])))
  table[indeks,]$`Track number` <- album_chronicals[i,]$`Track number`
  table[indeks,]$`Song duration` <- album_chronicals[i, ]$`Song duration`
  table[indeks,]$`Album name` <- "Rolling Stones Chronicles"
}
remove(url_album_chronicals)
remove(html_album_chronicals)
remove(album_chronicals)
#################################################
#brisemo album GRR! 
table <- subset(table, !(table$`Album name` %in% c("GRRR! (Super Deluxe)", "GRRR!","Through the Past, Darkly (Big Hits Vol. 2)")))
################################################################################
#ucitavam record label-e za sve albume
url_record <- "https://www.allmusic.com/artist/the-rolling-stones-mn0000894465/discography"
html_record<- read_html(url_record)
record_label<-  html_table(html_record, fill = TRUE)[[1]]
#brisemo visak kolone
record_label <- record_label[-c(1,2,6,7,8)]

#u pocetnu tabelu dodajemo kolonu Record Label
table$record_label <- NA
# premeštamo novu kolonu na poziciju posle "Album name"
position <- which(names(table) == "Album name")
table <- dplyr::select(table, 1:position,record_label, (position+1):ncol(table))  
#preimenuj kolonu album_type
names(table)[names(table) == "record_label"] <- "Record Label"
all_albums_records <- record_label$Album

for (i in 1:length(all_albums_records)) {
  indeks <- which(tolower(gsub("\\s+", "", table$`Album name`))== tolower(gsub("\\s+", "", all_albums_records[i])))
  table[indeks,]$`Record Label`<- record_label[i,]$Label
  
}
#obrisi red sa Totally Stripped imenom albuma
table <- table[table$`Album name` != "Totally Stripped", ]

table <- table[!is.na(table$`Record Label`),]

# izdvoji tabelu "Studio albums"
url_wiki_albums <- "https://en.wikipedia.org/wiki/The_Rolling_Stones_discography#Albums"
page_wiki_albums <- read_html(url_wiki_albums)
studio_albums_table <- page_wiki_albums %>% html_nodes("h3:contains('Studio albums') + table.wikitable") %>% html_table(fill = TRUE) %>% .[[1]]

#brisemo (UK) i (US) zagrade
studio_albums_table$Title <- gsub("\\s*\\(.*", "", studio_albums_table$Title)
studio_albums_table <- studio_albums_table[-c(1,nrow(studio_albums_table)),]
studio_albums_table[6,]$Title <- "December's Children (And Everybody's)"
studio_albums_table[19,]$Title <- "It's Only Rock 'n' Roll"
studio_albums_table$Title <- trimws(studio_albums_table$Title)



#studio_albums_table[6,]$Title <- "Between the Buttons"
studio_albums <- studio_albums_table$Title
for (i in 1:length(studio_albums)) {
  indeks <- which(tolower(gsub("\\s+", "", table$`Album name`))== tolower(gsub("\\s+", "", studio_albums[i])))
  table[indeks,]$`Album type`<- "Studio"
}

# izdvoji tabelu "Live albums"
live_albums_table <- page_wiki_albums %>% html_nodes("h3:contains('Live albums') + table.wikitable") %>% html_table(fill = TRUE) %>% .[[1]]
live_albums_table <- live_albums_table[-c(1,nrow(live_albums_table)),]

live_albums_table$Title <- gsub("!.*", "!", live_albums_table$Title)
live_albums_table$Title <- trimws(live_albums_table$Title)

live_albums <- live_albums_table$Title
for (i in 1:length(live_albums)) {
  indeks <- which(tolower(gsub("\\s+", "", table$`Album name`))== tolower(gsub("\\s+", "", live_albums[i])))
  table[indeks,]$`Album type`<- "Live"
}
# izdvoji tabelu "Compilation"

compilation_albums_table <- page_wiki_albums %>% html_nodes("h3:contains('Compilation albums') + table.wikitable") %>% html_table(fill = TRUE) %>% .[[1]]
compilation_albums_table <- compilation_albums_table[-1,]
compilation_albums_table$Title <- trimws(compilation_albums_table$Title)

#brisemo (UK) i (US) zagrade
compilation_albums_table$Title <- gsub("\\s*\\(.*", "", compilation_albums_table$Title)

compilation_albums <- compilation_albums_table$Title
for (i in 1:length(compilation_albums)) {
  indeks <- which(tolower(gsub("\\s+", "", table$`Album name`))== tolower(gsub("\\s+", "", compilation_albums[i])))
  table[indeks,]$`Album type`<- "Compilation"
}

table$`Album name` <- trimws(table$`Album name`)
#brisemo te redove
table <- table[table$`Album name` != "bootleg recording/outtake", ]
table <- table[table$`Album name` != "Metamorphosis", ]

#ucitavanje csv fajla sa Kaggle-a
kaggle <- read.csv("../rolling_stones_spotify.csv")
kaggle$name <- gsub("-.*","",kaggle$name)
kaggle$name <- gsub("/.*","",kaggle$name)

#brisemo 1, 3,4
kaggle <- kaggle[,-c(1,3:7)]
#preimenjujemo 1. kolonu u Title
kaggle <- kaggle %>% rename(Title = name)
kaggle$Title <- gsub("\\s{2,}", " ", kaggle$Title)
table$Title <- gsub("\\s{2,}", " ", table$Title)

kaggle$Title <- str_trim(kaggle$Title)

kaggle_summary <- kaggle %>%
  group_by(Title) %>%
  summarise_at(vars(-group_cols()), mean)


library(stringr)
joined_dataset <- left_join(table, kaggle_summary, by = c("Title" = "Title")) %>%
  mutate(Title = str_to_lower(Title))


joined_dataset$Title <- str_to_upper(joined_dataset$Title)

# Kreirajte novi data frame sa redovima gde je vrednost u koloni acousticness NA
na_rows <- subset(joined_dataset, !complete.cases(acousticness))
na_rows <- inner_join(na_rows,kaggle_summary, by = "Title")%>%
  mutate(Title = str_to_lower(Title))

na_rows <- na_rows %>% select(-matches("\\.x$"))
na_rows <- na_rows %>% rename_with(~str_replace(.x, ".y$", ""), ends_with(".y"))


pesme <- na_rows$Title
#petlja koja prolazi kroz sve pesme
for (i in 1:length(pesme)) {
  indeks <- which(tolower(gsub("\\s+", "", joined_dataset$Title))== tolower(gsub("\\s+", "", pesme[i])))
  print(indeks)
  print( joined_dataset[indeks,]$acousticness )
  joined_dataset[indeks,]$acousticness <- na_rows[i,]$acousticness
  joined_dataset[indeks,]$danceability <- na_rows[i,]$danceability
  joined_dataset[indeks,]$energy <- na_rows[i,]$energy
  joined_dataset[indeks,]$instrumentalness <- na_rows[i,]$instrumentalness
  joined_dataset[indeks,]$liveness <- na_rows[i,]$liveness
  joined_dataset[indeks,]$loudness <- na_rows[i,]$loudness
  joined_dataset[indeks,]$speechiness <- na_rows[i,]$speechiness
  joined_dataset[indeks,]$tempo <- na_rows[i,]$tempo
  joined_dataset[indeks,]$valence <- na_rows[i,]$valence
  #joined_dataset[indeks,]$popularity <- na_rows[i,]$popularity
  joined_dataset[indeks,]$duration_ms <- na_rows[i,]$duration_ms
}

#########################################################################
#ucitavamo podatke sa Britanske liste singlova
#Učitavamo HTML kod stranice
url_peak <- "https://www.officialcharts.com/artist/28195/rolling-stones/"
page_peak <- read_html(url_peak)

#Izvlaci tabelu sa stranice
table_peak <- page_peak %>% html_nodes(".artist-products") %>% html_table()
df_peak <- as.data.frame(table_peak[[1]])
#brisemo 6 i sedmu kolonu
df_peak <- df_peak[,-c(6,7)]


# Prikazujemo prvih nekoliko redova tabele
head(table_peak)
#sredjujemo kolonu Title
df_peak$`Title, Artist` <- gsub("\\r.*", "", df_peak$`Title, Artist`)

#sredjujemo kolonu Date
df_peak$Date <- gsub("\\r.*?(?=\\d)", "", df_peak$Date,perl = TRUE)
df_peak$Date <- gsub("\\s+", " ", df_peak$Date)

df_peak <- df_peak[grepl("[0-9]", df_peak$Date), ]
names(df_peak)[2] <- "Title"


library(diffobj)
diffPrint(str_to_lower(df_peak[19,]$Title),str_to_lower(joined_dataset[18,]$Title))

joined_dataset_final <- left_join(joined_dataset, df_peak, by = "Title")

# Zamena NA vrednosti u poslednje 4 kolone sa /
joined_dataset_final[, (ncol(joined_dataset_final)-3):ncol(joined_dataset_final)] <- apply(joined_dataset_final[, (ncol(joined_dataset_final)-3):ncol(joined_dataset_final)], 2, function(x) ifelse(is.na(x), "/ ", x))

#pesme na chart-ovima 1963-1979
url_charts  <- "https://en.wikipedia.org/wiki/The_Rolling_Stones_discography#Singles"
page_charts <- read_html(url_charts)

table_1963 <- page_charts %>% html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[11]") %>% html_table(fill = TRUE) %>% .[[1]]
#brisemo 3,4,5 kolone i poslednji red
table_1963 <- table_1963[-nrow(table_1963),-c(3,4,5)]
#sredjujemo kolonu Single
table_1963$Single <- gsub('"', '', table_1963$Single)
table_1963$Single <- trimws(table_1963$Single)
table_1963 <- table_1963 %>% rename(Title = Single)

table_1963$Title <- str_to_upper(table_1963$Title)
joined_dataset_final <- left_join(joined_dataset_final, table_1963, by = "Title")

#pesme na chart-ovima 1980-1993
table_1980 <- page_charts %>% html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[12]") %>% html_table(fill = TRUE) %>% .[[1]]
#brisemo 3,15,16 kolonu i poslednji red
table_1980 <- table_1980[-nrow(table_1980),-c(3,15,16)]
#sredjujemo kolonu Single
table_1980$Single <- gsub('"', '', table_1980$Single)
table_1980$Single <- trimws(table_1980$Single)
table_1980 <- table_1980 %>% rename(Title = Single)

table_1980$Title <- str_to_upper(table_1980$Title)
joined_dataset_final <- left_join(joined_dataset_final, table_1980, by ="Title")

joined_dataset_final <- joined_dataset_final %>%
  mutate(`UK[5]` = ifelse(is.na(`UK[5].y`), `UK[5].x`, `UK[5].y`),
         `US[6]` = ifelse(is.na(`US[6].y`), `US[6].x`, `US[6].y`),
         `GER[10]` =  ifelse(is.na(`GER[10].y`), `GER[10].x`, `GER[10].y`),
         `NLD[12]` =  ifelse(is.na(`NLD[12].y`), `NLD[12].x`, `NLD[12].y`),
         `FRA` =  ifelse(is.na(FRA.y), FRA.x, FRA.y),
         `SWI[59]` =  ifelse(is.na(`SWI[59].y`), `SWI[59].x`, `SWI[59].y`),
         `CAN` =  ifelse(is.na(CAN.y), CAN.x, CAN.y),
         `Certification` =  ifelse(is.na(Certification.y), Certification.x, Certification.y)
         ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

#pesme na chart-ovima 1994-2006
table_1994 <- page_charts %>% html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[13]") %>% html_table(fill = TRUE) %>% .[[1]]
#brisemo 3. i poslednje 4 kolone, kao i poslednji red
table_1994 <- table_1994[-nrow(table_1994),-c(3,14,15,16,17)]
#sredjujemo kolonu Single
table_1994$Single <- gsub('"', '', table_1994$Single)
table_1994$Single <- trimws(table_1994$Single)
table_1994 <- table_1994 %>% rename(Title = Single)

table_1994$Title <- str_to_upper(table_1994$Title)
joined_dataset_final <- left_join(joined_dataset_final, table_1994, by ="Title")

joined_dataset_final <- joined_dataset_final %>%
  mutate(`UK[5]` = ifelse(is.na(`UK[5].y`), `UK[5].x`, `UK[5].y`),
         `US[6]` = ifelse(is.na(`US[6].y`), `US[6].x`, `US[6].y`),
         `GER[10]` =  ifelse(is.na(`GER[10].y`), `GER[10].x`, `GER[10].y`),
         `NLD[12]` =  ifelse(is.na(`NLD[12].y`), `NLD[12].x`, `NLD[12].y`),
         `FRA` =  ifelse(is.na(FRA.y), FRA.x, FRA.y),
         `SWI[59]` =  ifelse(is.na(`SWI[59].y`), `SWI[59].x`, `SWI[59].y`),
         `CAN` =  ifelse(is.na(CAN.y), CAN.x, CAN.y),
         `POL` =  ifelse(is.na(POL.y), POL.x, POL.y),
         
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

joined_dataset_final <- joined_dataset_final %>%
  mutate(Certification = ifelse(is.na(Certification) | Certification == "", "—", Certification))

joined_dataset_final <- joined_dataset_final %>%
  select(-"Release date")

joined_dataset_final <- joined_dataset_final %>%
  mutate(`US Cash` = ifelse(is.na(`US Cash`), "—", `US Cash`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`US Rec World` = ifelse(is.na(`US Rec World`) | `US Rec World` == "","—", `US Rec World`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`AUS[7][61]` = ifelse(is.na(`AUS[7][61]`), "—", `AUS[7][61]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`UK[5]` = ifelse(is.na(`UK[5]`), "—", `UK[5]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`GER[10]` = ifelse(is.na(`GER[10]`), "—", `GER[10]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`NLD[12]` = ifelse(is.na(`NLD[12]`), "—", `NLD[12]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(FRA = ifelse(is.na(FRA), "—", FRA))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`SWI[59]` = ifelse(is.na(`SWI[59]`), "—", `SWI[59]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(POL = ifelse(is.na(POL), "—", POL))

joined_dataset_final <- joined_dataset_final %>%
  mutate(`US[6]` = ifelse(is.na(`US[6]`), "—", `US[6]`))

joined_dataset_final <- joined_dataset_final %>%
  mutate(CAN = ifelse(is.na(CAN) | CAN == "", "—", CAN))

joined_dataset_final <- joined_dataset_final %>%
  select(-"Certification", "Certification")

#install.packages("spotifyr")
library(spotifyr)

# Postavite svoje API ključeve
Sys.setenv(SPOTIFY_CLIENT_ID = "e1707a9784094b098e6c302f4a6222b9")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "058710aee06347fea415a46f5f2bc883")

access_token <- get_spotify_access_token()

# URL Spotify playliste koju želite analizirati
playlist_url <- "https://open.spotify.com/playlist/0AJyevsLShqGFnmxlxDDrL"

# Izdvajanje ID playliste iz URL-a
playlist_id <- sub("^.+/([[:alnum:]]+)$", "\\1", playlist_url)

all_tracks <- NULL
offset <- 0
limit <- 50

#library(dplyr)
repeat {
  # Dohvati trenutnu stranicu numera
  tracks <- get_playlist_tracks(playlist_id, limit = limit, offset = offset)
  
  # Ako nema više numera, prekini petlju
  if (length(tracks) == 0) {
    break
  }
  # Pretvori stranicu numera u data frame i dodaj je u all_tracks
  tracks_df <- as.data.frame(tracks)
  all_tracks <- bind_rows(all_tracks, tracks_df)
  # Pomeri offset za sledeću stranicu
  offset <- offset + limit
}
track_ids <- all_tracks$track.id
#track_name <- all_tracks$track.name
# Lista za čuvanje rezultata
all_audio_features <- list()

# Iteriranje kroz svaki track ID
for (track_id in track_ids) {
  # Dohvatanje audio feature-a za trenutni track ID
  audio_feature <- get_track_audio_features(track_id)
  
  # Dohvatanje imena pesme za trenutni track ID
  track <- get_track(track_id)
  track_name <- track$name
  
  # Dodavanje audio feature-a i imena pesme u listu
  audio_feature$Track_ID <- track_id
  audio_feature$Track <- track_name
  all_audio_features[[track_id]] <- audio_feature
}

# Pretvaranje liste u data frame
all_audio_features_df <- do.call(rbind, all_audio_features)

row_nums <- seq_len(nrow(all_audio_features_df))
all_audio_features_df$Row <- row_nums

all_audio_features_df$Track <- str_to_upper(all_audio_features_df$Track)
all_audio_features_df$Track <- sub(" -.*", "", all_audio_features_df$Track)
all_audio_features_df$Track <- trimws(all_audio_features_df$Track)

joined_dataset_final$Title <- gsub("\\(LIVE\\)", "", joined_dataset_final$Title)

pesme <- all_audio_features_df$Track
#update dataset
for (i in 1:length(all_audio_features_df$Track)) {
  indeks <- which(trimws(joined_dataset_final$Title) == pesme[i])
  joined_dataset_final[indeks,]$acousticness <- all_audio_features_df[i,]$acousticness
  joined_dataset_final[indeks,]$danceability <- all_audio_features_df[i,]$danceability
  joined_dataset_final[indeks,]$energy <- all_audio_features_df[i,]$energy
  joined_dataset_final[indeks,]$instrumentalness <- all_audio_features_df[i,]$instrumentalness
  joined_dataset_final[indeks,]$liveness <- all_audio_features_df[i,]$liveness
  joined_dataset_final[indeks,]$loudness <- all_audio_features_df[i,]$loudness
  joined_dataset_final[indeks,]$speechiness <- all_audio_features_df[i,]$speechiness
  joined_dataset_final[indeks,]$tempo <- all_audio_features_df[i,]$tempo
  joined_dataset_final[indeks,]$valence <- all_audio_features_df[i,]$valence
  joined_dataset_final[indeks,]$duration_ms <- all_audio_features_df[i,]$duration_ms
}


#druga spotify playlista
playlist_url2 <- "https://open.spotify.com/playlist/30KEBso4y7A8gZaZzccfes"

# Izdvajanje ID playliste iz URL-a
playlist_id <- sub("^.+/([[:alnum:]]+)$", "\\1", playlist_url2)

all_tracks <- NULL
offset <- 0
limit <- 50

repeat {
  # Dohvati trenutnu stranicu numera
  tracks <- get_playlist_tracks(playlist_id, limit = limit, offset = offset)
  
  # Ako nema više numera, prekini petlju
  if (length(tracks) == 0) {
    break
  }
  
  # Pretvori stranicu numera u data frame i dodaj je u all_tracks
  tracks_df <- as.data.frame(tracks)
  all_tracks <- bind_rows(all_tracks, tracks_df)
  
  # Pomeri offset za sledeću stranicu
  offset <- offset + limit
}

track_ids <- all_tracks$track.id
#track_name <- all_tracks$track.name

# Lista za čuvanje rezultata
all_audio_features <- list()

# Iteriranje kroz svaki track ID
for (track_id in track_ids) {
  # Dohvatanje audio feature-a za trenutni track ID
  audio_feature <- get_track_audio_features(track_id)
  
  # Dohvatanje imena pesme za trenutni track ID
  track <- get_track(track_id)
  track_name <- track$name
  
  # Dodavanje audio feature-a i imena pesme u listu
  audio_feature$Track_ID <- track_id
  audio_feature$Track <- track_name
  all_audio_features[[track_id]] <- audio_feature
}

# Pretvaranje liste u data frame
all_audio_features_df2 <- do.call(rbind, all_audio_features)

# Ukloni sve pre "- " i sam "- " iz vrednosti u koloni "Track"
all_audio_features_df2$Track <- sub(" -.*", "", all_audio_features_df2$Track)
all_audio_features_df2$Track <- trimws(all_audio_features_df2$Track)
all_audio_features_df2$Track <- str_to_upper(all_audio_features_df2$Track)

pesme <- all_audio_features_df2$Track

#update dataset
for (i in 1:length(all_audio_features_df2$Track)) {
  indeks <- which(joined_dataset_final$Title == pesme[i])
  joined_dataset_final[indeks,]$acousticness <- all_audio_features_df2[i,]$acousticness
  joined_dataset_final[indeks,]$danceability <- all_audio_features_df2[i,]$danceability
  joined_dataset_final[indeks,]$energy <- all_audio_features_df2[i,]$energy
  joined_dataset_final[indeks,]$instrumentalness <- all_audio_features_df2[i,]$instrumentalness
  joined_dataset_final[indeks,]$liveness <- all_audio_features_df2[i,]$liveness
  joined_dataset_final[indeks,]$loudness <- all_audio_features_df2[i,]$loudness
  joined_dataset_final[indeks,]$speechiness <- all_audio_features_df2[i,]$speechiness
  joined_dataset_final[indeks,]$tempo <- all_audio_features_df2[i,]$tempo
  joined_dataset_final[indeks,]$valence <- all_audio_features_df2[i,]$valence
  joined_dataset_final[indeks,]$duration_ms <- all_audio_features_df2[i,]$duration_ms
}

joined_dataset_final$Title <- str_to_title(joined_dataset_final$Title,"word")
joined_dataset_final$popularity <- NULL

joined_dataset_final <- joined_dataset_final %>%
  mutate(`British charts` = ifelse(trimws(Date) != "/", "Yes", "No"))
joined_dataset_final$`British charts` <- as.factor(joined_dataset_final$`British charts`)

joined_dataset_final <- joined_dataset_final %>%
  relocate(`British charts`, .before = Date)

write.csv(joined_dataset_final, file = "stones.csv", row.names = F)
#write.csv(joined_dataset_final, file = "stones2.csv", row.names = F)

rm(list = ls()[!ls() %in% "joined_dataset_final"])

