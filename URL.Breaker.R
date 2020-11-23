#This Script sorts out the URLs!!
#Mining the URLs
### Packages ###################################################################
ga.f <- x
### Inputs #####################################################################
keep <- 6 #Number of URL levels you want to keep as independent columns
#We don't keep all, because of processing time
################################################################################
cc <- c("landingPagePath","secondPagePath","exitPagePath")
#cc <- c("pagePath")
cc <- cc[1:sum(!is.na(match(cc,names(x))))] #Number of page dimentions download. 
#According to the vector on top, it can go from 1 to 3 
xx <- c("\\?", "#")
#xx <- 0
################################################################################
#ga.f$Start.Date <- NULL; ga.f$End.Date <- NULL
ga.f <- cbind(0,ga.f) #It does help in the loop
yy <- list(NULL)
for(ii in cc){
    #Get rid of page parameters
  if(xx!=0){
    z <- which(colnames(ga.f) == ii)
    for(jj in xx){
      ga.f[,z] <- sapply(ga.f[,z], function(x){
        if(regexpr(jj, x)!=-1){substr(x, 1,regexpr(jj, x) - 1)} else{x}})
      }
    }
  #Break the URL
  yy[[match(ii, cc)]] <- cSplit(ga.f[,c(1,which(colnames(ga.f) == ii))],ii, "/", drop=F) #What a package... -> "splitstackshape"
}
  
#Clean the breaks
ww <- ncol(ga.f)
keep <- lapply(yy, function(x){min(keep, ncol(x)-4)}) #Put 2 instead of 4 if you choose to NULL out the dates
zz <- lapply(lapply(lapply(yy, function(x){data.frame(x)}), 
             function(y){apply(y, 2, function(x){gsub("^$","HP",x)})}),
             function(k){data.frame(k)})
for(ii in 1:length(keep)){ #Devias por uma especie de duplo lapply aqui....
  zz[[ii]] <- zz[[ii]][,c(3:(3 + keep[[ii]] - 1))] 
}
ga.f <- cbind(ga.f, do.call(cbind,zz))
#Sort out the headers
jj <- 1
for(ii in 1:length(keep)){
  if(keep[[ii]] == 1){
    names(ga.f)[ww + jj] <- paste(cc[[ii]], "_1", sep="")
  }
  jj <- jj + keep[[ii]]
}

#Number of Pages (captured ones - from 0 to 3)
yy <- list(NULL); for(ii in 1:length(cc)){yy[[i]] <- ga.f[,cc[ii]]}
yy <- do.call(cbind, yy)
zz <- rowSums(yy == "(not set)")
ga.f <- cbind("Number.of.Pages" = apply(yy,1, function(x){length(unique(x))}) - zz,
              "Not.Set" = sapply(zz, function(x){if(x==0){"URL"}else{"(not set)"}}),
              ga.f[,-1])

#Does Page Number = Page Depth?
if(is.na(match("pageDepth", names(x)))==F){
  ga.f$page.vs.pageDepth <- sapply(as.numeric(ga.f$Number.of.Pages) - as.numeric(ga.f$pageDepth), 
                                   function(x){if(x == 0){"page=pageDeph"}
                                     else{if(x > 0){"page>pageDeph"}
                                         else{if(x < 0){"page<pageDeph"}
                                         else{"!Error!"}}}})
}

#Update the dimensions vector
dimensions.2 <- names(ga.f)

x <- ga.f
#END
