# The file downloads data sets that the app will use.
# The working directory must be set to the location of the App
setwd("C:/.../Shiny App Crime2/")

US_States<-c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
             "District of Columbia","Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas",
             "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
             "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
             "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
             "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
             "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

# The URL is different for Pennsylvania and Rhode Island
for (i in 1:50){
    state<-gsub(" ","_",tolower(US_States[i]))
    if (state=="pennsylvania"){
        fileURL<-"http://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/table-8/table-8-state-cuts/table-8-pennsylvania/output.xls"
    }
    else if (state=="rhode_island"){
        fileURL<-"http://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/table-8/table-8-state-cuts/table_8_offenses_known_to_law_enforcement_rhode-island_by_city_2013.xls/output.xls"
    }
    else{
        fileURL<-paste0("https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/table-8/table-8-state-cuts/table_8_offenses_known_to_law_enforcement_",state,"_by_city_2013.xls/output.xls")
    }
    fileName<-paste0("data/",state,"_by_city_2013.xls")
    download.file(fileURL,destfile=fileName,mode="wb") # Download file
}



