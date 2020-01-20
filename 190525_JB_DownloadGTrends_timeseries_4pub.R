    # Clear workspace and graphs
if(!is.null(dev.list())) dev.off()
rm(list = ls()) 

# install.packages('Rcpp', dependencies = TRUE)
library("devtools")
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE,, dependencies = TRUE)
setwd("~/git")
install("gtrendsR",dependencies = TRUE)
library("gtrendsR")
library("zoo")

username <- Sys.info()["user"]
path <- # Your working path
# There should be a folder:  DATA/GTrends/


##################################################
# TODO: 
#   - Turn this into a function, submit to gtrendsR


###################################################
#
# SET PARAMETERS
#
###################################################
uselocal <- F
username <- # Your Username
passwd <- # Your Password
###################################################


baseDIR <- path
dataDIR <- paste0(baseDIR,"DATA/GTrends/")

# Set the value of the earliest and latest date
earliestDate <- as.POSIXct("2006-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
latestDate <- as.POSIXct("2019-06-10T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
#as.POSIXct(Sys.time())

# Set the timezone offset to zero to get GMT time
tz <- 0

# Also set the number of observation that you want to overlap
overlap <- 30

# Significance level for alpha in regression
siglevel <- 0.05

# frequency
freq_list <- c(1440)
# geo: "US","DE","FR","LU"
geo_list <- c("US")# ,"LU"
#,"US","DE","FR"
#Possible values:
# 1         1   minute intervals
# 8         8   minute intervals
# 16        16  minute intervals
# 60        1   hour intervals
# 1440      1   day  intervals
# 10080     1   week intervals
# 43200     1   month intervals

# Define the price levels that you would like to retrieve the price levels for
# c(1,2,5,10,20,50,100,250,500,750,1000,1500,2000,2500,5000,7500,seq(10000,95000,5000))
# 1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000
prices_vec <- c(1,600)
#1,5,10,20,50,100,500,
#c(0,1,5,10,20,50,100,250,500,1000,1500,2000,5000,10000,15000,20000,50000,100000,250000,500000,1000000,5000000,100000000)
compare_level <- 1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Price levels with all major EuroArea languages ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
prop <- c("web")    
for(geo in geo_list){
  
  if(geo=="all"){
    # Define the price levels that you would like to retrieve the price levels for
    searchterm_list <- sapply(prices_vec,function(x) {
      paste0(sprintf("%i", x)," euro", # Name
             ' + ',sprintf("%i", x)," eur" #Ticker
             ,' + ',sprintf("%i", x)," €" #Symbol
             ,' + ',sprintf("%i", x),"€" #Symbol
             ,' + €',sprintf("%i", x)
             ,' + ',sprintf("%i", x)," ευρώ" # Greek
             ,' + ',sprintf("%i", x)," euros" # Spanish Portugese
             ,' + ',sprintf("%i", x)," euroa" # Finish
             #,' + ',sprintf("%i", x)," evrov" # Slovenish
             # ,' + ',sprintf("%i", x)," eurot" # Estnisch
             # ,' + ',sprintf("%i", x)," eiro" # lettisch
             # ,' + ',sprintf("%i", x)," ewro" # Maltesisch
             # ,' + ',sprintf("%i", x)," eurų" # Litauisch
             #," -in -to -en -a -το -em -v" # Exclude exchange rate queries
      )
      # in: English/Italian/Dutch; to: English/Dutch; en: French; a; Spanish; em: Protugese; v: Slowenisch
      # Maltese changes proposi tion for currency exchanges depending on the target currency => disregarded
      #
    }
    )
    
  }else if(geo=="US"){
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Price levels for US ----
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    searchterm_list <- sapply(prices_vec,function(x) {
      paste0(sprintf("%i", x)," $" #Ticker
             ,' + ',sprintf("%i", x)," dollar" #Ticker
             ,' + ',sprintf("%i", x),"USD" #Ticker
             ,' + ',sprintf("%i", x),"$" #Ticker
             ,' + $',sprintf("%i", x)
             ,' + ',sprintf("%i", x), ' USD'
             ,' -million', ' -billion'
      )
    }
    )
    
  }else if(geo=="DE" | geo=="FR" | geo=="LU"){
    # Define the price levels that you would like to retrieve the price levels for
    searchterm_list <- sapply(prices_vec,function(x) {
      paste0(sprintf("%i", x)," euro", # Name
             ' + ',sprintf("%i", x)," eur" #Ticker
             ,' + ',sprintf("%i", x)," €" #Symbol
             ,' + ',sprintf("%i", x),"€" #Symbol
             ,' + €',sprintf("%i", x)
             ,' + ',sprintf("%i", x)," euros" # Spanish Portugese
             #,' + ',sprintf("%i", x)," evrov" # Slovenish
             # ,' + ',sprintf("%i", x)," eurot" # Estnisch
             # ,' + ',sprintf("%i", x)," eiro" # lettisch
             # ,' + ',sprintf("%i", x)," ewro" # Maltesisch
             # ,' + ',sprintf("%i", x)," eurų" # Litauisch
             #," -in -to -en -a -το -em -v" # Exclude exchange rate queries
      )
      # in: English/Italian/Dutch; to: English/Dutch; en: French; a; Spanish; em: Protugese; v: Slowenisch
      # Maltese changes proposition for currency exchanges depending on the target currency => disregarded
      #
    }
    )
  }
  
  
  
  for(freq in freq_list){
    for (qq in searchterm_list){
      if(which(qq==searchterm_list)<2){next}
      #qq <- searchterm_list[1]
      method = "backward"
      testid = "exalpha"
      if (qq==searchterm_list[which(prices_vec==compare_level)]){
        query <- qq
      }else{
        query <- c(qq,searchterm_list[which(prices_vec==compare_level)])
      }
      
      print(paste0("Do the dance for ",query))
      
      
      region <- geo
      
      if(geo=="all"){
        region <- ""
      }
      
      if(freq==1){
        strInterval <- "1min"
        gfmt <- "%Y-%m-%dT%H"
        fmt <- "%Y-%m-%d %H:%M:%S"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 4*60
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==8){
        strInterval <- "8min"
        gfmt <- "%Y-%m-%dT%H"
        nodig <- "13"
        fmt <- "%Y-%m-%d %H:%M:%S"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 35*60
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==16){
        strInterval <- "16min"
        gfmt <- "%Y-%m-%dT%H"
        nodig <- "13"
        fmt <- "%Y-%m-%d %H:%M:S"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 70*60
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==60){
        strInterval <- "hourly"
        gfmt <- "%Y-%m-%dT%H"
        nodig <- "13"
        fmt <- "%Y-%m-%d %H:%M:%S"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 7*24*60
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==1440){
        strInterval <- "daily"
        nodig <- "10"
        gfmt <- fmt <- "%Y-%m-%d"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 210*24*60
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==10080){
        strInterval <- "weekly"
        nodig <- "10"
        gfmt <- fmt <- "%Y-%m-%d"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 5*365*24*60 - 40*24*60 
        goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
      }else if(freq==43200){
        strInterval <- "monthly"
        nodig <- "10"
        gfmt <- fmt <- "%Y-%m-%d"
        dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
        outDIR <- paste0(dataDIR,strInterval,"/")
        if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
          earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
        }
        step <- 1 
        goBackIter <- 1
      }else{
        stop("Not a valid frequency.")
      }
      
      
      # Create the output folders
      dir.create(file.path(outDIR, prop), showWarnings = FALSE)
      outDIR <- paste0(outDIR,prop,"/")
      dir.create(file.path(outDIR, geo), showWarnings = FALSE)
      outDIR <- paste0(outDIR,geo,"/")
      dir.create(file.path(outDIR, "raw"), showWarnings = FALSE)
      
      
      if(!uselocal){
        # Remove all raw data
        searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
        
        flist <- list.files(path=paste0(outDIR,"raw"),full.names=T)
        if(length(query)==1){
          flist <- flist[!grepl("#",flist)]
        }
        do.call(file.remove, list(flist[grepl(searchstr,flist)]))
        # Remove all merged data data
        flist <- list.files(path=paste0(outDIR),full.names=T)
        if(length(query)==1){
          flist <- flist[!grepl("#",flist)]
        }
        do.call(file.remove, list(flist[grepl(searchstr,flist)]))
      }
      
      
      start_date <- end_date <- NA
      
      
      method="backward"
      end_date <- latestDate
      
      # set start and end_date for first iteration
      if(freq!=43200){
        start_date <- end_date -step*60
      }else{
        start_date <- earliestDate
      }
      start_date <- as.POSIXct(start_date,origin="1970-01-01",tz="UCT")
      end_date <- as.POSIXct(end_date,origin="1970-01-01",tz="UCT")
      
      
      endres <- res <- tmp_endres_merge <- NA
      pathology <- last <- redo <- FALSE
      
      
      ####################################################################
      # Here begins the main loop
      ####################################################################
      
      if(!uselocal){
        while(TRUE){
          #Convert datas to an amicable formats
          intervalstr <- make_intervalstr(start_date,end_date,freq)
          
          # Save old results for next iteration
          # but only when we do not have a pathelogical case
          # and have to redo the download process
          if(!redo){
            resold <- res
            old_enddate <- end_date
          }
          
          # Get new results
          attempt <- 1
          tmpres <- NULL
          while( is.null(tmpres) && attempt <= 5 ) {
            attempt <- attempt + 1
            if(!uselocal){
              setHandleParameters(user = username, password = passwd)
            }
            try(
              tmpres <- gtrends(query,time = rep(intervalstr,length(query)), geo=rep(region,length(query)) ,gprop=prop,tz = tz,onlyInterest=TRUE)$interest_over_time
            )
          }
          if(is.null(tmpres)){break}
          
          if(length(query)>1){
            res <- zoo(as.numeric(tmpres[tmpres$keyword==query[1],2]),order.by = as.POSIXct(tmpres[tmpres$keyword==query[1],1],tz="UCT"))
            
            for(jj in 2:length(query)){
              res <- merge(res,zoo(as.numeric(tmpres[tmpres$keyword==query[jj],2]),order.by = as.POSIXct(tmpres[tmpres$keyword==query[jj],1],tz="UCT")))
            }
            
          }else{
            res <- zoo(as.numeric(tmpres[,2]),order.by = as.POSIXct(tmpres[,1],tz="UCT"))
          }
          
          
          
          # Merge new results beginning in the second iteration
          if(length(resold)!=1){
            # Merge between two subsequent time frames
            tmp <- na.omit(merge(resold,res))
            attr(index(tmp),"tzone") <- "UTC"
            
            # Construct the merge for the endresult
            if(length(endres)!=1){
              tmp_endres_merge <- na.omit(merge(endres,res))
              attr(index(tmp_endres_merge),"tzone") <- "UTC"
            }else{
              tmp_endres_merge <- NA
              endres <- res
            }
            
            limit <- 15
            slimit <- 10
            if((freq == 60)|
               (prop == "news")){
              limit <- 15
              slimit <- 10
            }
            # Check pathological cases
            if(sum(apply(tmp,1,sum)!=0)<limit){
              # We require 30 dates with nonzero values in both time frames
              # here: We only have less then 30 dates.
              # Thus, reset the start and end dates
              end_date <- end_date+overlap*freq*60
              start_date <- end_date-step*60
              # Set marker for pathelogical case
              redo <- TRUE
              
              if((old_enddate>end_date)|(end_date>Sys.time())){
                warning(paste0("We cannot concatenate the SVI for ",query,", too many zero observations!"))
                break
              }
            }else if((sum(tmp[,1]!=0)<slimit)|(sum(tmp[,2]!=0)<slimit)){
              # We require 20 dates with nonzero values in either of the two time frames
              # here: We only have less then 20 dates in one of the time frames
              # Thus, reset the start and end dates
              end_date <- end_date+20*freq*60
              start_date <- end_date-step*60
              # Set marker for pathelogical case
              redo <- TRUE
              if((old_enddate>end_date)|(end_date>Sys.time())){
                warning(paste0("We cannot concatenate the SVI for ",query,", too many zero observations!"))
                break
              }
            }else{
              end_date <- start_date+overlap*freq*60
              start_date <- end_date-step*60
            }
          }else{
            endres <- res
            end_date <- start_date+overlap*freq*60
            start_date <- end_date-step*60
          }
          
          if(!redo){
            ########################################################################
            # In the case everything is fine ...
            ########################################################################
            # 1. Dump the downloaded data
            outstr <- gsub(" ","_",intervalstr)
            write.zoo(res,file=paste0(outDIR,"raw/",paste0(query,collapse="#"),"_",outstr,".csv"))
            
            # 2. Do the regression and fitting
            if(length(tmp_endres_merge)!=1){ 
              if(length(query)==1){ # This is the case when we only want to download one time series
                X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,2]))
                Y <- as.matrix(tmp_endres_merge[,1])
                
                nn <- nrow(X)
                XXinv <- solve(t(X)%*%X)
                beta <- XXinv%*%t(X)%*%Y
                
                if(testid=="exalpha"){
                  # Calculate the errors
                  u <- as.vector(Y-X%*%beta)
                  # Calculate the heteroskedasty consistent variance estimator
                  sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
                  # Calculate the test statistic
                  test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
                  if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                    XXinv <- solve(t(X[,2])%*%X[,2])
                    beta <- XXinv%*%t(X[,2])%*%Y
                    beta <- c(0,beta)
                  }
                }
                
                Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
                Yhat <- Xmat%*%beta
                
                
                final <- merge(endres,cbind(res,Yhat)[,2])
                attr(index(final),"tzone") <- "UTC"
                final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
                endres <- final[,1]
                }else{ # This is the case when we only want to download multiple time series
                  no1 <- which(query==searchterm_list[which(prices_vec==compare_level)])
                  X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,length(query)+no1]))
                  Y <- as.matrix(tmp_endres_merge[,no1])
                  
                  nn <- nrow(X)
                  XXinv <- solve(t(X)%*%X)
                  beta <- XXinv%*%t(X)%*%Y
                  
                  if(testid=="exalpha"){
                    # Calculate the errors
                    u <- as.vector(Y-X%*%beta)
                    # Calculate the heteroskedasty consistent variance estimator
                    sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
                    # Calculate the test statistic
                    test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
                    if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                      XXinv <- solve(t(X[,2])%*%X[,2])
                      beta <- XXinv%*%t(X[,2])%*%Y
                      beta <- c(0,beta)
                    }
                  }
                  
                  Yhat <- matrix(0,NROW(res),NCOL(res))
                  for (hh in 0:length(query)){
                    if(hh == 0){
                      Yhat[,1] <- rep(1,NROW(res))*beta[1]
                    }else{
                      Yhat[,hh] <- Yhat[,1] + as.vector(res[,hh])*beta[2]
                    }
                  }
                  Yhat <- zoo(Yhat, order.by=index(res))
                
                  tmp2 <- cbind(res,Yhat)
                  final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
                  attr(index(final),"tzone") <- "UTC"
                  final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
                  endres <- final[,1:length(query)]
                }
              
            }
          }
          
          if(end_date <= earliestDate){
            break
          }
          if (last){
            break
          }
          
          if(start_date <= earliestDate){
            start_date <- earliestDate
            end_date <- start_date + step*60
            last <- TRUE
          }
        }
        
        
        # Dump the merged data
        write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
        
        
        
        rm("resold")
        rm("endres")
        
      }else{
        ####################################
        # Concatenate backward (exalpha) LOCAL!!!
        ####################################
        method = "backward"
        testid = "exalpha"
        
        searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
        flist <- list.files(path=paste0(outDIR,"/raw"))
        flist <- flist[grepl(searchstr,flist)]
        dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
        if (length(dates)==0){
          next
        }
        if(length(query)==1){
          dates <- dates[!grepl("#",dates)]
          dates <- dates[order(as.POSIXct(dates,format=gfmt))]
          fname <- flist[grepl(dates[length(dates)],flist)&!grepl("#",flist)]
        }else{
          dates <- dates[order(as.POSIXct(dates,format=gfmt))]
          fname <- flist[grepl(dates[length(dates)],flist)]
        }
        
          
        endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
        endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
        
        
        if(length(dates)>=2){
          for(jj in (length(dates)-1):1){
            
            if(length(query)==1){
              fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
            }else{
              fname <- flist[grepl(dates[jj],flist)]
            }
            
            res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
            res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
            
            tmp_endres_merge <- na.omit(merge(endres,res))
            attr(index(tmp_endres_merge),"tzone") <- "UTC"
            
            if(length(tmp_endres_merge)==0){break}
            
            if(length(query)==1){ # This is the case when we only want to download one time series
    
                X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,2]))
                Y <- as.matrix(tmp_endres_merge[,1])
                
                nn <- nrow(X)
                XXinv <- solve(t(X)%*%X)
                beta <- XXinv%*%t(X)%*%Y
                
                if(testid=="exalpha"){
                  # Calculate the errors
                  u <- as.vector(Y-X%*%beta)
                  # Calculate the heteroskedasty consistent variance estimator
                  sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
                  # Calculate the test statistic
                  test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
                  if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                    XXinv <- solve(t(X[,2])%*%X[,2])
                    beta <- XXinv%*%t(X[,2])%*%Y
                    beta <- c(0,beta)
                  }
                }
                
                Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
                Yhat <- Xmat%*%beta
                
                
                final <- merge(endres,cbind(res,Yhat)[,2])
                attr(index(final),"tzone") <- "UTC"
                final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
                endres <- final[,1]
              }else{ # This is the case when we only want to download multiple time series
                no1 <- which(query==searchterm_list[which(prices_vec==compare_level)])
                if(sum(tmp_endres_merge==1)==NROW(tmp_endres_merge)){
                  # Handle here the case were we ONLY have ones in the vector
                  X <- as.matrix(tmp_endres_merge[,length(query)+no1])
                }else{
                  # Handle here the case were we DO NOT have ones in the vector
                  X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,length(query)+no1]))
                }
                
                Y <- as.matrix(tmp_endres_merge[,no1])
                
                nn <- nrow(X)
                XXinv <- solve(t(X)%*%X)
                beta <- XXinv%*%t(X)%*%Y
                
                if(testid=="exalpha"){
                  # Calculate the errors
                  u <- as.vector(Y-X%*%beta)
                  # Calculate the heteroskedasty consistent variance estimator
                  sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
                  # Calculate the test statistic
                  test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
                  if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                    XXinv <- solve(t(X[,2])%*%X[,2])
                    beta <- XXinv%*%t(X[,2])%*%Y
                    beta <- c(0,beta)
                  }
                }
                
                Yhat <- matrix(0,NROW(res),NCOL(res))
                for (hh in 0:length(query)){
                  if(hh == 0){
                    Yhat[,1] <- rep(1,NROW(res))*beta[1]
                  }else{
                    Yhat[,hh] <- Yhat[,1] + as.vector(res[,hh])*beta[2]
                  }
                }
                Yhat <- zoo(Yhat, order.by=index(res))
                
                tmp2 <- cbind(res,Yhat)
                final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
                attr(index(final),"tzone") <- "UTC"
                final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
                endres <- final[,1:length(query)]
              }
            
          }
        }
        
        # Dump the merged data
        write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
        rm("endres")
      }
      
      
      ####################################
      # Concatenate backward (inalpha)
      ####################################
      method = "backward"
      testid = "inalpha"
      
      searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
      flist <- list.files(path=paste0(outDIR,"/raw"))
      flist <- flist[grepl(searchstr,flist)]
      dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
      if(length(query)==1){
        dates <- dates[!grepl("#",dates)]
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[length(dates)],flist)&!grepl("#",flist)]
      }else{
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[length(dates)],flist)]
      }
      endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
      endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
      
      
      if(length(dates)>=2){
        for(jj in (length(dates)-1):1){
          
          if(length(query)==1){
            fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
          }else{
            fname <- flist[grepl(dates[jj],flist)]
          }
          
          res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
          res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
          
          
          tmp_endres_merge <- na.omit(merge(endres,res))
          attr(index(tmp_endres_merge),"tzone") <- "UTC"
          
          
          if(length(query)==1){ # This is the case when we only want to download one time series
            
            if(sum(tmp_endres_merge==1)==NROW(tmp_endres_merge)){
              # Handle here the case were we ONLY have ones in the vector
              X <- as.vector(tmp_endres_merge[,2])
            }else{
              # Handle here the case were we DO NOT have ones in the vector
              X <- cbind(rep(1,NROW(tmp_endres_merge)),as.vector(tmp_endres_merge[,2]))
            }  
           
            if(length(unique(tmp_endres_merge[,2]))==1){
              break
            }
            Y <- as.matrix(tmp_endres_merge[,1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
            Yhat <- as.zoo(Xmat%*%beta,order.by=index(res))
            
            
            final <- merge(endres,Yhat)
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
            endres <- final[,1]
          }else{
            # This is the case when we only want to download multiple time series
            no1 <- which(query==searchterm_list[which(prices_vec==compare_level)])
            X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,length(query)+no1]))
            Y <- as.matrix(tmp_endres_merge[,no1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Yhat <- matrix(0,NROW(res),NCOL(res))
            for (hh in 0:length(query)){
              if(hh == 0){
                Yhat[,1] <- rep(1,NROW(res))*beta[1]
              }else{
                Yhat[,hh] <- Yhat[,1] + as.vector(res[,hh])*beta[2]
              }
            }
            Yhat <- zoo(Yhat, order.by=index(res))
            
            tmp2 <- cbind(res,Yhat)
            final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
            endres <- final[,1:length(query)]
            
          }
          
        }
      }
      
      # Dump the merged data
      write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
      rm("endres")
      
      ####################################
      # Concatenate forward (exalpha)
      ####################################
      
      method = "forward"
      testid = "exalpha"
      
      searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
      flist <- list.files(path=paste0(outDIR,"/raw"))
      flist <- flist[grepl(searchstr,flist)]
      dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
      
      if(length(query)==1){
        dates <- dates[!grepl("#",dates)]
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[1],flist)&!grepl("#",flist)]
      }else{
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[1],flist)]
      }
      
      
      endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
      endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
      
      if(length(dates)>=2){
        for(jj in 2:length(dates)){
          
          if(length(query)==1){
            fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
          }else{
            fname <- flist[grepl(dates[jj],flist)]
          }
          
    
          res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
          res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
          
          tmp_endres_merge <- na.omit(merge(endres,res))
          attr(index(tmp_endres_merge),"tzone") <- "UTC"
          
          
          if(length(query)==1){ # This is the case when we only want to download one time series
              
            X <- cbind(rep(1,NROW(tmp_endres_merge)),as.vector(tmp_endres_merge[,2]))
            if(length(unique(tmp_endres_merge[,2]))==1){
              break
            }
            Y <- as.matrix(tmp_endres_merge[,1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
            Yhat <- as.zoo(Xmat%*%beta,order.by=index(res))
            
            
            final <- merge(endres,Yhat)
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
            endres <- final[,1]
          }else{
            
            # This is the case when we only want to download multiple time series
            no1 <- which(query==searchterm_list[which(prices_vec==compare_level)])
            X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,length(query)+no1]))
            Y <- as.matrix(tmp_endres_merge[,no1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Yhat <- matrix(0,NROW(res),NCOL(res))
            for (hh in 0:length(query)){
              if(hh == 0){
                Yhat[,1] <- rep(1,NROW(res))*beta[1]
              }else{
                Yhat[,hh] <- Yhat[,1] + as.vector(res[,hh])*beta[2]
              }
            }
            Yhat <- zoo(Yhat, order.by=index(res))
            
            tmp2 <- cbind(res,Yhat)
            final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
            endres <- final[,1:length(query)]
            
          }
        }
      }
      
      # Dump the merged data
      write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
      rm("endres")
      
      ####################################
      # Concatenate forward (inalpha)
      ####################################
      
      method = "forward"
      testid = "inalpha"
      searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
      flist <- list.files(path=paste0(outDIR,"/raw"))
      flist <- flist[grepl(searchstr,flist)]
      dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
      if(length(query)==1){
        dates <- dates[!grepl("#",dates)]
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[1],flist)&!grepl("#",flist)]
      }else{
        dates <- dates[order(as.POSIXct(dates,format=gfmt))]
        fname <- flist[grepl(dates[1],flist)]
      }
      endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
      endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
      
      if(length(dates)>=2){
        for(jj in 2:length(dates)){
          
          if(length(query)==1){
            fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
          }else{
            fname <- flist[grepl(dates[jj],flist)]
          }
          res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
          res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
          
          
          tmp_endres_merge <- na.omit(merge(endres,res))
          attr(index(tmp_endres_merge),"tzone") <- "UTC"
          
          
          if(length(query)==1){ # This is the case when we only want to download one time series
            X <- cbind(rep(1,NROW(tmp_endres_merge)),as.vector(tmp_endres_merge[,2]))
            if(length(unique(tmp_endres_merge[,2]))==1){
              break
            }
            Y <- as.matrix(tmp_endres_merge[,1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
            Yhat <- as.zoo(Xmat%*%beta,order.by=index(res))
            
            
            final <- merge(endres,Yhat)
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
            endres <- final[,1]
          }else{
            
            # This is the case when we only want to download multiple time series
            no1 <- which(query==searchterm_list[which(prices_vec==compare_level)])
            X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,length(query)+no1]))
            Y <- as.matrix(tmp_endres_merge[,no1])
            
            nn <- nrow(X)
            XXinv <- solve(t(X)%*%X)
            beta <- XXinv%*%t(X)%*%Y
            
            if(testid=="exalpha"){
              # Calculate the errors
              u <- as.vector(Y-X%*%beta)
              # Calculate the heteroskedasty consistent variance estimator
              sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*beta[1]/sqrt(sigma2[1,1])
              if ((qt(siglevel/2,nn-2) < test_alpha) | (test_alpha > qt(1-siglevel/2,nn-2) )){
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Yhat <- matrix(0,NROW(res),NCOL(res))
            for (hh in 0:length(query)){
              if(hh == 0){
                Yhat[,1] <- rep(1,NROW(res))*beta[1]
              }else{
                Yhat[,hh] <- Yhat[,1] + as.vector(res[,hh])*beta[2]
              }
            }
            Yhat <- zoo(Yhat, order.by=index(res))
            
            tmp2 <- cbind(res,Yhat)
            final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
            attr(index(final),"tzone") <- "UTC"
            final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
            endres <- final[,1:length(query)]
          }
          
        }
      }
      
      # Dump the merged data
      write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
      Sys.sleep(1)
      rm("endres")
      
    }
  }
}
