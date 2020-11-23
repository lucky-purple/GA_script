#Extraction part based on http://www.tatvic.com/blog/google-analytics-data-extraction-in-r/
#If doubts check the basic steps on that  URL
#If there are problems with the token go here and create a new one: 
#https://console.developers.google.com/project
#Dimensions and Metrics availability on: 
#https://developers.google.com/analytics/devguides/reporting/core/dimsmets#mode=api
#https://cran.r-project.org/web/packages/RGoogleAnalytics/RGoogleAnalytics.pdf
#https://ga-dev-tools.appspot.com/query-explorer/
###################################################################################
#Clean
rm(list = ls())
########################### INPUTS ################################################
#SET UP YOUT DIRECTORY:
setwd("YOUR PATH") 
#SET UP FILE NAME
file_output <- "R_GA_Output.csv"
###################################################################################
#URL Breaker - choose yes or no - FOR NOW, only makes sense if LP, SP and EP (or less) are in ALL dimensions
ub <- "no"
#Deduplicator - choose yes or no - Only makes sense  if ALL dimensions are not NULL and equal across extraction
dd <- "no" #Moreover it will ignore time, so there is no point in putting time as a breakdown on the extraction .csv file
###################################################################################
#Set cronometer
ptm <- proc.time()
#Check Packages
packages <- c("RGoogleAnalytics","plyr","shiny","zoo","Hmisc","data.table",
              "quantmod","ggplot2", "maps","mapproj","mapdata","devtools","rsconnect","splitstackshape")
for (i in packages) {if (!(require(i, character.only=T, quietly=T))) {
  install.packages(i); library(i, character.only=T)}}

#Get Extraction file in:
extraction <- subset(read.csv("Extraction.csv"), Extract=="yes")
emails <- as.vector(unique(extraction$Email))

#Start Loop
#Check Tokens and Extract Data
ga.final <- list(NULL)
for(i in 1:length(emails)){
  if(file.exists(paste("./token_file -", emails[i])) == T){
    load(paste("./token_file -", emails[i]))
    ValidateToken(token)} else {
      credentials <- subset(read.csv("Credentials.csv"), Email==emails[i])
      token <- Auth(paste(unlist(credentials[2])), paste(unlist(credentials[3])))
      save(token, file=paste("./token_file -", emails[i]))
    }
  extract <- subset(extraction, Email == emails[i])
  ga.final[[i]] <- list(NULL)
  #setwd("H:/")
  #Create variables
  uu <- NULL
  for(j in 1:nrow(extract)){
    time <- as.character(extract[j,3])
    start.date <- as.Date(extract[j,4], "%d/%m/%Y")
    end.date <- as.Date(extract[j,5], "%d/%m/%Y")                          
    dimensions <- unlist(strsplit(as.character(extract[j,6]),","))            
    metrics <- unlist(strsplit(as.character(extract[j,7]),","))
    filters <- unlist(strsplit(as.character(extract[j,8]),","))
    sort <- unlist(strsplit(as.character(extract[j,9]),","))   
    segments <- unlist(strsplit(as.character(extract[j,10]),","))
    max.results <- as.vector(extract[j,11])                                 
    start.index <- as.vector(extract[j,12])                                 
    table.id <- as.vector(extract[j,13])
    GA.View <- as.vector(extract[j,14])
    #Sort out the blanks into NULL vectors for the query
    if(identical(filters, character(0))==T){filters <- NA}
    if(identical(sort, character(0))==T){sort <- NA}
    if(identical(segments, character(0))==T){segments <- NA}
    if(identical(max.results, character(0))==T){max.results <- NA}
    if(identical(start.index, character(0))==T){start.index <- NA}
    if(is.na(filters)==T){filters <- NULL}
    if(is.na(sort)==T){sort <- NULL}
    if(is.na(segments)==T){segments <- NULL}
    if(is.na(max.results)==T){max.results <- NULL}
    if(is.na(start.index)==T){start.index <- NULL}
    #Transform dates and others according to option specified in "time"
    if(time=="month"){ ##!!
      start.m <- as.numeric(substr(start.date,6,7))
      end.m <- as.numeric(substr(end.date,6,7))
      start.y <- as.numeric(substr(start.date,1,4))
      end.y <- as.numeric(substr(end.date,1,4))
      if(end.y - start.y > 1){
        m <- c(seq(start.m,12), rep(seq(1:12), end.y - start.y - 1),seq(1,end.m))
        y <- c(rep(start.y, 12 - start.m + 1),
               sort(rep(seq(start.y + 1, end.y - 1), 12)), rep(end.y, end.m))
      } else {
        if(end.y - start.y == 1){
          m <- c(seq(start.m,12),seq(1,end.m))
          y <- c(rep(start.y, 12 - start.m + 1),rep(end.y, end.m))
        } else {
          if(end.y - start.y == 0){
            m <- seq(start.m, end.m)
            y <- rep(start.y, length(m))
          }}}
      sD <- as.Date(paste(y, "-",m,"-01", sep=""))
      sD <- as.character(rep(sD,length(table.id)))
      eD <- as.character(as.Date(as.numeric(as.Date(sD)) +  monthDays(sD) - 1))
    } else {
      if(time!="no"){
        sD <- as.Date(seq(
          as.numeric(as.Date(start.date)), as.numeric(as.Date(end.date)),by = as.numeric(time)))
        sD <- as.character(rep(sD,length(table.id)))
        eD <- as.character(as.Date(sD) + as.numeric(time) - 1)
      } else{
        sD <- as.character(rep(start.date, length(table.id)))
        eD <- as.character(rep(end.date, length(table.id)))
        }
    }
    #Get the Sessions & Transactions for each Dimension sorted by the Metrics chosen
    #Create the Query Builder object so that the query parameters are validated
    #Extract the data and store it in a list with data-frames
    dimensions.2 <- NULL #Helpfull when we call the De-duplication script
    query.list <- list(NULL)
    for(k in 1:length(sD)){
      query.list <- Init(start.date = sD[k], end.date = eD[k],
                              dimensions = dimensions,metrics = metrics,
                              filters = filters, sort = sort, segments = segments,
                              max.results = max.results, start.index = start.index,
                              table.id = table.id)
      
      #  ga.data <- tryCatch(tryCatch(
      #  GetReportData(QueryBuilder(query.list), token, split_daywise=F, paginate_query=T), #!#!#!#! HERE: T or F!!!!! 
      #  silent=T, 
      #  error = function(e) GetReportData(QueryBuilder(query.list), token, split_daywise=F, paginate_query=F)),
      #  silent=T, error = function(e) t(data.frame(rep(NA, length(dimensions) + length(metrics)))))
       
        ga.data <- tryCatch(tryCatch(
        GetReportData(QueryBuilder(query.list), token, split_daywise=F, paginate_query=T), #!#!#!#! HERE: T or F!!!!! 
        silent=T, 
        error = function(e) (
          tryCatch(GetReportData(QueryBuilder(query.list), token, split_daywise=F, paginate_query=F), 
                   silent=F,
                   error = function(e) tryCatch(GetReportData(
                     QueryBuilder(query.list), token, split_daywise=F, paginate_query=F), silent=F,
                     error = function(e) GetReportData(QueryBuilder(query.list), 
                                                       token, split_daywise=F, paginate_query=F)))
        )
        ),
        silent=T, error = function(e) t(data.frame(rep(NA, length(dimensions) + length(metrics)))))
            
      nn <- nrow(ga.data)
      ga.final <- data.frame(cbind(rep(GA.View, nn), rep(table.id, nn),
                                   rep(sD[k], nn),rep(eD[k], nn), ga.data))
      names(ga.final) <- c("GA.View","View.ID","Start.Date","End.Date", 
                           gsub("ga:", "", dimensions), gsub("ga:", "", metrics))
      ############################################################################
      ## URL Breaker #############################################################
      if(ub == "yes"){
        x <- ga.final
        #source(paste(path, "URL.Breaker.R", sep=""))
        source("URL.Breaker.R")
        ga.final <- x
      }
      ############################################################################
      ############################################################################
      #Stack them up!
      if(is.null(uu) == F){ga.final <- rbind.fill(ga.final, uu)}
      uu <- ga.final
      ############################################################################
      ## De-duplication, when time does not matter!###############################
      if(dd == "yes"){
        x <- ga.final
        #source(paste(path, "URL.Deduplicator.R", sep=""))
        source("URL.Deduplicator.R")
        ga.final <- x
      }
      ############################################################################
      ############################################################################
      dimensions <- dimensions #if we use the URL breaker, it changes the dimensions, so here we set them up as before
    }}} 
#Save to .csv file
write.csv(ga.final, file_output, row.names = F)
#write.csv(ga.final,  paste("C:/Users/gcoelho/Desktop/",file_output, sep=""), row.names =  F)
#########tail(ga.fin#######################################################################
## Variable Classifier for Shiny ###############################################
#source("VariableClassifierShiny.R")
################################################################################
################################################################################
#Stop and print cronometer result
paste("Time elapsed:", seconds_to_period(round((proc.time() - ptm)[3])))

#END