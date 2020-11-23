#This Script de-duplicates rows with identical pages (after or not correcting 
#Choose option 1 or 2. Option 1 is the ID one
option <- 2
#for parameters) when time does not matter for the analysis
ga.f <- x
#Test if new dimensions were incorporated
if(is.null(dimensions.2) == T){
  dimensions.2 <- c("GA.View", "View.ID", gsub("ga:", "", dimensions))
} else{
  #Get rid of Start.Date, End.Date and Metrics
  p.sD <- grep("Start.Date",dimensions.2)
  p.eD <- grep("End.Date",dimensions.2)
  p.metrics <- sapply(metrics, function(x){grep(gsub("ga:", "", x), dimensions.2)})
  dimensions.2 <- dimensions.2[c(-p.sD,-p.eD,-p.metrics)]
  }
#Substitute NAs per "0"
ga.f[is.na(x)] <- 0
#Create the ID
if(option == 1){
  ga.f$ID <- do.call(paste, c(ga.f[dimensions.2], sep=";"))
  #Do the deduplication
  ga.f <- ddply(ga.f,
                "ID", 
                function(x) colSums(x[gsub("ga:","",metrics)]))
  #Separate the ID into the originals columns
  ga.f <- cSplit(ga.f, "ID",";")
  names(ga.f)[(length(metrics) + 1):ncol(ga.f)] <- dimensions.2
  } else{
  #Do the deduplication
  ga.f <- ddply(x,
                dimensions.2, 
                function(x) colSums(x[gsub("ga:","",metrics)]))
  }
#Get the Start.date and end date columns to the game
ga.f$Start.Date <- 0; ga.f$End.Date <- 0

x <- ga.f

#END