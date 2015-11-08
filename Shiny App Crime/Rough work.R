library(xlsx)
library(dplyr)
library(stringdist)

getwd()
setwd("C:/Users/Steve/Documents/Programming/R/Shiny App Crime")

US_States<-c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
             "District of Columbia","Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas",
             "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
             "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
             "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
             "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
             "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
US_States_low<-tolower(US_States)

state<-"california"
sdist<-stringdist(a=tolower(state),b=US_States_low)
state<-US_States[which(sdist==min(sdist))][1]




# diststate<-tolower(state)
# state<-gsub(" ","_",state)
# gsub(" ","_",tolower(state))

# capitalize the first letter of words
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

# Download the data
# temp<-tempfile() # Create a temporary file
# fileURL<-paste0("https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2011/crime-in-the-u.s.-2011/tables/table8statecuts/table_8_offenses_known_to_law_enforcement_",gsub(" ","_",tolower(state)),"_by_city_2011.xls/output.xls")
# download.file(fileURL,destfile=temp,mode="wb") # Download file
# df<-read.xlsx2(temp,sheetIndex = 1, startRow=5, header=TRUE) # function might not be stable for reading subsets of rows
# fileURL<-"https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/table-8/table_8_offenses_known_to_law_enforcement_by_state_by_city_2013.xls"
# df<-read.xlsx(temp,sheetIndex = 1,startRow=4,header=TRUE,rowIndex=4:10)
# unlink(temp) # Remove temp file


# Download the data
fileName<-paste0("data/",gsub(" ","_",tolower(state)),"_by_city_2013.xls")
download.file(fileURL,destfile=fileName,mode="wb") # Download file
df<-read.xlsx2(fileName,sheetIndex = 1, startRow=5, header=TRUE) # function might not be stable for reading subsets of rows
#fileURL<-"https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/table-8/table_8_offenses_known_to_law_enforcement_by_state_by_city_2013.xls"
#df<-read.xlsx(temp,sheetIndex = 1,startRow=4,header=TRUE,rowIndex=4:10)
#unlink(temp) # Remove temp file


head(df)
dim(df)
help(download.file)

df<-mutate(df,Population=as.numeric(levels(Population))[Population])
maxp<-max(df$Population) # max population in the state
maxp_city<-as.character(df$City[df$Population==maxp][1]) # associated city
#state<-simpleCap(gsub("_"," ",state))
paste0(maxp_city," has the largest population of all cities in ", state, ", ", maxp," people live there.")







