###############################################
# PPT 4 -  Web Scraping using R: a case study
##############################################

# Intall the package rvest
install.packages('rvest')
# Loading the rvest package
library('rvest')
# Specifying the url for desired website to be scraped
url<- 'https://www.imdb.com/search/title/?year=2022&amp&title_type=feature' 
# Reading the HTML code from the website
webpage <- read_html(url)

#Scraping movies tittles
title_htlm <- html_nodes(webpage, ".lister-item-header a")
titles <- html_text(title_htlm)
titles <-ifelse(titles=="",NA,titles)
titles <-na.exclude(titles)
titles

# Scraping movies synopsies
synop_htlm <- html_nodes(webpage, ".text-muted+ .text-muted , .ratings-bar+ .text-muted")
synop <- html_text(synop_htlm)
synop <-ifelse(synop =="",NA,synop)
synop <-na.exclude(synop)
synop <-gsub("[\r\n]", "", synop)
synop

#From scraping data to a structured database
movies<-data.frame(titles,synop)
View(movies)

#create a loop to import into the database rates, dates, genre, Direction
# Ratings 
rating_htlm <- html_nodes(webpage, ".ratings-imdb-rating strong")
rating <- html_text(rating_htlm)
rating <-ifelse(rating=="",NA,rating)
rating <-na.exclude(rating)
rating

movies <- cbind(movies,rating)
View(movies)

#Dates
dates_htlm <- html_nodes(webpage, ".text-muted.unbold")
dates <- html_text(dates_htlm)
dates <-ifelse(dates=="",NA,dates)
dates <-na.exclude(dates)
dates <-gsub("[(),I,II,V]","", dates)
dates

movies <- cbind(movies,dates)
View(movies)

#Genre
genre_htlm <- html_nodes(webpage, ".lister-item-header+ .text-muted")
genre <- html_text(genre_htlm)
genre <-ifelse(genre=="",NA,genre)
genre <-na.exclude(genre)
genre <-gsub("[]","", genre)
genre

movies <- cbind(movies,dates)
View(movies)

##############################################################################
#                             CORRECTION
##############################################################################
#scrap imbd movies from webpage

install.packages('rvest')
library('httr')
library('rvest')

#save html page
url <- 'https://www.imdb.com/search/title/?year=2022&amp&title_type=feature'

#read html page
webpage <- read_html(url)

#selecting titles and adding them to a vector 

title_html <- html_nodes(webpage, ".lister-item-header a")
titles <- html_text(title_html)
titles   <- ifelse(titles=="",NA, titles)
titles <- na.exclude(titles)
titles

#add the synop
synop_html <- html_nodes(webpage,".text-muted+ .text-muted , .ratings-bar+ .text-muted")
synop <- html_text(synop_html)
synop <- ifelse(synop=="",NA,synop)
synop<-na.exclude(synop)
synop<-gsub("[\r\n]", "", synop)
synop


#add the year 
year_html <- html_nodes(webpage,".text-muted.unbold")
year <- html_text(year_html)
year <- ifelse(year=="",NA,year)
year<-na.exclude(year)
year<-gsub("[(I,V)]", "", year)
year


#duration and genre
duration_html <- html_nodes(webpage,".lister-item-header+ .text-muted")
duration <- html_text(duration_html)
duration<-gsub("[\n ]", "", duration)
duration


#director and cast
cast_html <- html_nodes(webpage,".text-muted~ .text-muted+ p , .ratings-bar~ .text-muted+ p")
cast <- html_text(cast_html)
cast <- ifelse(cast=="",NA,cast)
cast<-na.exclude(cast)
cast<-gsub("[(\n)]", "", cast)
cast


df <- data.frame(titles,synop,cast,duration,year)
View(df)

