###############################################
# Web Scraping using R: a case study
##############################################

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

