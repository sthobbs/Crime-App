# Things I want to improve:
    # (maybe) interpolate missing values (esp chicago)
    # a button to clear input (and maybe return to default figures)
    # improve spell checker
        # (maybe) combine string dist with max overlap
        # include state abbreviations
    # (maybe) combine the two input sections into one (how?)
    # add a tab for notes: maybe a help tab (maybe with pictures)

# Load packages
library(xlsx)
library(dplyr)
library(stringdist)
library(googleVis)
library(reshape2) 
library(rCharts) # downloaded from github not CRAN (see rCharts documentation for how to download)
options(RCHART_LIB = 'nvd3')


US_States<-c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
             "Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas",
             "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
             "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey",
             "New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
             "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
             "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
US_States_low<-tolower(US_States) # US states in lower case
US_abrv<-c("al","ak","az","ar","ca","co","ct","de","fl","ga","id","il","in","ia","ks","ky","la","me",
           "md","ma","mi","mn","ms","mo","mt","ne","nv","nh","nj","nm","ny","nc","nd","oh","ok","or",
           "pa","ri","sc","sd","tn","tx","ut","vt","va","wa","wv","wi","wy")


# Initial loading and processing of data
df_list<-list() # list of 49 data frames, one for each state (minus Hawaii)
df_list2<-list() # list of 49 data frames, one for each state (minus Hawaii) for rates
vc<-array(dim=c(length(US_States),6)) # data frame of violent crime by state
vc<-data.frame(vc) 
vc[,1]=US_States
for (i in 1:length(US_States)){
    state<-US_States[i]
    fileName<-paste0("data/",gsub(" ","_",tolower(state)),"_by_city_2013.xls")
    df<-read.xlsx2(fileName,sheetIndex = 1, startRow=5, header=TRUE) # function might not be stable for reading subsets of rows
    df<-df[,1:13] # removing the extra columns in Maine
    names(df)<-c("City","Population","Violent.crime","Murder","Rape1","Rape2","Robbery",
                 "Aggravated.assult","Property.crime","Burglary","Larceny","Motor.vehicle.theft","Arson")
    while(as.character(df[nrow(df),2])==""){
        df<-df[-nrow(df),]
    }
    df<-df %>% # turn factor variables into numeric variables
        mutate(Population=as.numeric(levels(Population))[Population]) %>%
        mutate(Violent.crime=as.numeric(levels(Violent.crime))[Violent.crime]) %>%
        mutate(Murder=as.numeric(levels(Murder))[Murder]) %>%
        mutate(Rape1=as.numeric(levels(Rape1))[Rape1]) %>%
        mutate(Rape2=as.numeric(levels(Rape2))[Rape2]) %>%
        mutate(Rape1=ifelse(is.na(Rape1),0,Rape1)) %>%
        mutate(Rape2=ifelse(is.na(Rape2),0,Rape2)) %>%
        mutate(Rape=Rape1+Rape2) %>%
        mutate(Robbery=as.numeric(levels(Robbery))[Robbery]) %>%
        mutate(Aggravated.assult=as.numeric(levels(Aggravated.assult))[Aggravated.assult]) %>%
        mutate(Property.crime=as.numeric(levels(Property.crime))[Property.crime]) %>%
        mutate(Burglary=as.numeric(levels(Burglary))[Burglary]) %>%
        mutate(Larceny=as.numeric(levels(Larceny))[Larceny]) %>%
        mutate(Motor.vehicle.theft=as.numeric(levels(Motor.vehicle.theft))[Motor.vehicle.theft]) %>%
        mutate(Arson=as.numeric(levels(Arson))[Arson]) %>%
        mutate(Arson=ifelse(is.na(Arson),0,Arson)) %>%
        mutate(City=as.character(City))
    names(df)<-c("City","Population","Violent Crime","Murder","Rape1","Rape2","Robbery",
                 "Assault","Property Crime","Burglary","Larceny","Motor Vehicle Theft","Arson","Rape")
    for (j in 1:nrow(df)){ # remove numbers (footnotes) from the end of cities
        while (substr(df$City[j],nchar(df$City[j]),nchar(df$City[j])) %in% c("0","1","2","3","4","5","6","7","8","9",","," ")){
            df$City[j]<-substr(df$City[j],0,nchar(df$City[j])-1)
        }
    }
    df_list[[i]]<-df
    vc[i,2:6]<-colSums(df[,c(3,4,14,7,8)],na.rm=TRUE)
    names(df)<-c("City","Population","Violent.crime","Murder","Rape1","Rape2","Robbery",
                 "Aggravated.assult","Property.crime","Burglary","Larceny","Motor.vehicle.theft","Arson","Rape")
    rounddigits<-0
    df <- df %>% # convert to rates relative to population
        mutate(Violent.crime=round(Violent.crime*100000/Population,rounddigits)) %>%
        mutate(Murder=round(Murder*100000/Population,rounddigits)) %>%
        mutate(Rape1=round(Rape1*100000/Population,rounddigits)) %>%     
        mutate(Rape2=round(Rape2*100000/Population,rounddigits)) %>%
        mutate(Robbery=round(Robbery*100000/Population,rounddigits)) %>%
        mutate(Aggravated.assult=round(Aggravated.assult*100000/Population,rounddigits)) %>%
        mutate(Property.crime=round(Property.crime*100000/Population,rounddigits)) %>%
        mutate(Burglary=round(Burglary*100000/Population,rounddigits)) %>%
        mutate(Larceny=round(Larceny*100000/Population,rounddigits)) %>%
        mutate(Motor.vehicle.theft=round(Motor.vehicle.theft*100000/Population,rounddigits)) %>%
        mutate(Arson=round(Arson*100000/Population,rounddigits)) %>%
        mutate(Rape=round(Rape*100000/Population,rounddigits))
    names(df)<-c("City","Population","Violent Crime","Murder","Rape1","Rape2","Robbery",
                 "Assault","Property Crime","Burglary","Larceny","Motor Vehicle Theft","Arson","Rape")
    df_list2[[i]]<-df
}
vc2<-vc # data frame of violent crime by state (rates)
for (i in 1:length(US_States)){
    state_pop<-sum(df_list[[i]]$Population,na.rm=T)
    for (j in 2:6){
        vc2[i,j]<-round(vc2[i,j]*100000/state_pop,rounddigits)
    }
}


# Notes:
# some missing values have been set to zero,
    # most notably aggravated assult in Chicago
    # along with many rape and arson numbers are missing
# no data on hawaii



# Initial Map and table
table.height<-262
vc<-arrange(vc,X2=desc(X2))
names(vc)<-c("State",names(df)[c(3,4,14,7,8)])
vcmap <- gvisGeoChart(vc, "State", "Violent Crime",
                         options=list(region="US", displayMode="regions",
                                      resolution="provinces", width=350, height=table.height))
vc_table<-cbind(1:nrow(vc),vc)
names(vc_table)[1]<-c(" ")
vc_table_fig<-gvisTable(vc_table, options=list(height=table.height,width=490))
vcmerge<-gvisMerge(vcmap,vc_table_fig,horizontal=T)

# Initial Map and table (for rates)
vc2<-arrange(vc2,X2=desc(X2))
names(vc2)<-c("State",names(df)[c(3,4,14,7,8)])
vc2map <- gvisGeoChart(vc2, "State", "Violent Crime",
                      options=list(region="US", displayMode="regions",
                                   resolution="provinces", width=350, height=table.height))
vc2_table<-cbind(1:nrow(vc2),vc2)
names(vc2_table)[1]<-c(" ")
vc2_table_fig<-gvisTable(vc2_table, options=list(height=table.height,width=490))
vc2merge<-gvisMerge(vc2map,vc2_table_fig,horizontal=T)



# Initial Multibar Chart
Longvc<-melt(vc[1:5,-2],"State") # subset and reformat data
names(Longvc)[c(2,3)]<-c("Crime","Offences") # add column names
n1 <- nPlot(Offences ~ State, group = 'Crime', data = Longvc, type = 'multiBarChart')
n1$xAxis(axisLabel = 'State') # label x-axis
n1$yAxis(axisLabel = "Offences", width=70) # label y-axis
n1$chart(margin = list(left = 68)) # shift plot right to enlarge left margen (>63)
n1$set(width = 750, height=300) # set plot dimensions

# Initial Multibar Chart (for rates)
Longvc2<-melt(vc2[1:5,-2],"State") # subset and reformat data
names(Longvc2)[c(2,3)]<-c("Crime","Offences") # add column names
n12 <- nPlot(Offences ~ State, group = 'Crime', data = Longvc2, type = 'multiBarChart')
n12$xAxis(axisLabel = 'State') # label x-axis
n12$yAxis(axisLabel = "Offences per 100000", width=70) # label y-axis
n12$chart(margin = list(left = 68)) # shift plot right to enlarge left margen (>63)
n12$set(width = 750, height=300) # set plot dimensions




shinyServer(
    function(input, output){
        buttonPressed <- eventReactive(input$goButton,{ # code stops here until action button is pressed
            state<-tolower(input$text1) 
            city<-tolower(input$text2)
            if (min(stringdist(state,US_abrv))==0){ # states can be specified with abbreviations
               state<-US_States[which(stringdist(state,US_abrv)==0)] # Capital first letter so correction message comes up
            }
            if (state==""){ # 4 cases
                if(city==""){ # no state or city
                    if (input$radio==2){ # if we want rates
                        subtitle<-"All figures show the number of offences per 100000 people."
                        bars<-n12
                        mergeplot<-vc2merge
                    }
                    else{ # if we want absolute numbers
                        subtitle=""
                        bars<-n1
                        mergeplot<-vcmerge
                    }
                    return(list("Enter a State and/or City for localized figures.",bars,mergeplot,subtitle))
                } 
                else{ # case where just a city is entered
                    # choose the city with the min string distance, break ties by population
                    min_d<-stringdist(city,tolower(df_list[[1]][1,1])) # minimum string distance between city and actual cities (pick first city distance for inital value)
                    min_d_max_pop<-df_list[[1]][1,2] # associated population
                    city_guess <- list(df_list[[1]][1,1],1) # guess of what city they meant with state number i
                    for (i in 1:length(US_States)){
                        dist_vec<-stringdist(city,tolower(df_list[[i]][,1])) # distances between city and all cities in state i
                        min_dist<-min(dist_vec) # the minimum distance in state i
                        if (min_dist>min_d){
                            next
                        }
                        min_dist_vec<-which(dist_vec==min_dist) # city indexes which tie for minimum distance
                        city_pops<-df_list[[i]][min_dist_vec,2] # associated populations of min distances
                        if (min_dist==min_d & max(city_pops)<=min_d_max_pop){
                            next
                        }
                        else{ # there is a city which is a better match ie. min_dist<min_d or (min_dist==min_d & max(city_pops)>min_d_max_pop)
                            city_ind<-min_dist_vec[order(city_pops,decreasing=TRUE)[1]] # city index with min distance, ties conssidered
                            min_d<-stringdist(city,tolower(df_list[[i]][city_ind,1])) # minimum string distance between city and actual cities (pick first city distance for inital value)
                            min_d_max_pop<-df_list[[i]][city_ind,2]
                            city_guess <- list(df_list[[i]][city_ind,1],i)
                        }
                    }
                    correction<-paste0("Did you mean ", city_guess[[1]],", ",US_States[city_guess[[2]]], "?")
                    if (input$radio==2){ # if we want rates
                        subtitle<-"All figures show the number of offences per 100000 people."
                        df<-df_list2[[city_guess[[2]]]]
                        ylabel<-"Offences per 100000"
                        plottitle<-paste0("Crime Rate in ",city_guess[[1]],", ",US_States[city_guess[[2]]])
                        
                    }
                    else{ # if we want absolute numbers
                        subtitle<-""
                        df<-df_list[[city_guess[[2]]]]
                        ylabel<-"Offences"
                        plottitle<-paste0("Crime in ",city_guess[[1]],", ",US_States[city_guess[[2]]])
                    }
                    df<-df[,c(1,2,3,4,14,7,8,9,10,11,12,13)]
                    names(df)[11]<-"Car Theft"
                    Longdf<-melt(df[df$City==city_guess[[1]],-c(2:3)],"City")
                    Longdf<-Longdf[,2:3]
                    names(Longdf)<-c("Crime","Offences")
                    Longdf[,3]<-c(rep("Violent",4),rep("Non-violent",5))
                    names(Longdf)[3]<-c("Violence")
                    Longdf<-mutate(Longdf,Crime=as.character(Crime))
                    
                    n2 = nPlot(Offences~Crime,data = Longdf, type ="multiBarChart")# "discreteBarChart")
                    n2$chart(showControls = F)
                    n2$set(width = 750, height=table.height) # set plot dimensions
                    n2$chart(reduceXTicks = FALSE)
                    #n2$xAxis(staggerLabels = TRUE)
                    n2$yAxis(axisLabel = ylabel, width=70) # label y-axis
                    n2$chart(margin = list(left = 68)) # shift plot right to enlarge left margen (>63)
    
                    n2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
                    n2$set(title = plottitle)

                    
                    df<-arrange(df,Population=desc(Population))
                    df_table<-cbind(1:nrow(df),df[,-3]) # table
                    names(df_table)[1:2]<-c(" ",paste0('Cities in ',US_States[city_guess[[2]]]))
                    df_table_fig<-gvisTable(df_table, options=list(height=table.height,width=840))
                    
                    return(list(correction,n2,df_table_fig,subtitle))
                }
            }
            else{ # case where a state has been entered
                # state typos are only based on string distance and not population, tie breaks are alphebetical.
                sdist<-stringdist(state,US_States_low) # String distance to correct spelling errors
                min_dist <- min(sdist)
                state_ind<-which(sdist==min_dist)[1] # state index with min distance city
                if(city==""){ # case where a state has been entered but a city is not
                    if (min_dist==0){ # if there was no error than don't put the error message
                        correction<-""
                    }
                    else{
                        correction<-paste0("Did you mean ", US_States[state_ind], "?")
                    }
                    if (input$radio==2){ # if we want rates
                        subtitle<-"All figures show the number of offences per 100000 people."
                        df<-df_list2[[state_ind]]
                        ylabel<-"Offences per 100000"
                        mapplot<-vc2map
                    }
                    else{ # if we want absolute numbers
                        subtitle<-""
                        df<-df_list[[state_ind]]
                        ylabel<-"Offences"
                        mapplot<-vcmap
                    }
                    df<-df[,c(1,2,4,14,7,8)] # format data
                    df<-arrange(df,desc(Population)) # sort by Population
                    Longdf<-melt(df[1:5,-2],"City") # subset and reformat data
                    names(Longdf)[c(2,3)]<-c("Crime","Offences") # add column names
                    m1 <- nPlot(Offences ~ City, group = 'Crime', data = Longdf, type = 'multiBarChart')
                    m1$xAxis(axisLabel = paste0('Largest Cities in ', US_States[state_ind])) # label x-axis
                    m1$yAxis(axisLabel = ylabel, width=70) # label y-axis
                    m1$chart(margin = list(left = 68)) # shift plot right to enlarge left margen (>63)
                    m1$set(width = 750, height=300) # set plot dimensions
                    
                    df_table<-cbind(1:nrow(df),df)
                    names(df_table)[1:2]<-c(" ",paste0('Cities in ', US_States[state_ind]))
                    df_table_fig<-gvisTable(df_table, options=list(height=table.height,width=490))
                    plotmerge<-gvisMerge(mapplot,df_table_fig,horizontal=T)
                    return(list(correction,m1,plotmerge,subtitle))
                }
                else{ # case where a state and city are entered
                    # Find best match for state, then best match for city within state
                    
                    dist_vec<-stringdist(city,tolower(df_list[[state_ind]][,1])) # distances between city input and all cities in the state
                    min_dist2<-min(dist_vec) # the minimum distance
                    min_dist_vec<-which(dist_vec==min_dist2) # city indexes which tie for minimum distance
                    city_pops<-df_list[[state_ind]][min_dist_vec,2] # associated populations of min distances
                    city_ind<-min_dist_vec[order(city_pops,decreasing=TRUE)[1]] # city index with min distance, ties considered
                    city_guess <- df_list[[state_ind]][city_ind,1]
                    if (min_dist==0 & min_dist2==0){ # if there was no error than don't put the error message
                        correction=""
                    }
                    else{
                        correction<-paste0("Did you mean ", city_guess,", ",US_States[state_ind], "?")
                    }
                    if (input$radio==2){ # if we want rates
                        subtitle<-"All figures show the number of offences per 100000 people."
                        df<-df_list2[[state_ind]]
                        ylabel<-"Offences per 100000"
                        plottitle<-paste0("Crime Rate in ",city_guess,", ",US_States[state_ind])
                        
                    }
                    else{ # if we want absolute numbers
                        subtitle<-""
                        df<-df_list[[state_ind]]
                        ylabel<-"Offences"
                        plottitle<-paste0("Crime in ",city_guess,", ",US_States[state_ind])
                    }
                    df<-df[,c(1,2,3,4,14,7,8,9,10,11,12,13)]
                    names(df)[11]<-"Car Theft"
                    Longdf<-melt(df[df$City==city_guess,-c(2:3)],"City")
                    Longdf<-Longdf[,2:3]
                    names(Longdf)<-c("Crime","Offences")
                    Longdf[,3]<-c(rep("Violent",4),rep("Non-violent",5))
                    names(Longdf)[3]<-c("Violence")
                    Longdf<-mutate(Longdf,Crime=as.character(Crime))
                    
                    n2 = nPlot(Offences~Crime,data = Longdf, type ="multiBarChart")# "discreteBarChart")
                    n2$chart(showControls = F)
                    n2$set(width = 750, height=table.height) # set plot dimensions
                    n2$chart(reduceXTicks = FALSE)
                    #n2$xAxis(staggerLabels = TRUE)
                    n2$yAxis(axisLabel = ylabel, width=70) # label y-axis
                    n2$chart(margin = list(left = 68)) # shift plot right to enlarge left margen (>63)
                    n2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
                    n2$set(title = plottitle)
                    
                    df<-arrange(df,Population=desc(Population))
                    df_table<-cbind(1:nrow(df),df[,-3]) # table
                    names(df_table)[1:2]<-c(" ",paste0('Cities in ',US_States[state_ind]))
                    df_table_fig<-gvisTable(df_table, options=list(height=table.height,width=840))
                    
                    return(list(correction,n2,df_table_fig,subtitle))
                }
            }
        })
        output$text1 <- renderText({
           correction<-buttonPressed()[[1]] # Did you mean...?
        })
        output$text2 <- renderText({
            buttonPressed()[[4]]
        })
        output$gvis1 <- renderGvis(
            if (input$goButton==0){return(vcmerge)} # initial output before the button has been pressed
            else{
                buttonPressed()[[3]]
            }
        )
        output$chart1 <- renderChart2({ # need to use renderChart2 for nvd3
            if (input$goButton==0){return(n1)}
            else{
                buttonPressed()[[2]]
            }
        })

    }
)