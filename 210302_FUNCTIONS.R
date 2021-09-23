.pkgenv <- new.env(parent=emptyenv())

get_search_term_list <- function(geo,prices_vec){
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
  return(searchterm_list)
}


#########################################################################



get_freq_vars <- function(freq,latestDate){
  if(freq==1){
    strInterval <- "1min"
    gfmt <- "%Y-%m-%dT%H"
    fmt <- "%Y-%m-%d %H:%M:%S"
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
    if(earliestDate<as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
      earliestDate <- as.POSIXct("2015-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
    }
    step <- 7*24*60
    goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
  }else if(freq==1440){
    strInterval <- "daily"
    nodig <- "10"
    gfmt <- fmt <- "%Y-%m-%d"
    if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
      earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
    }
    step <- 210*24*60
    goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
  }else if(freq==10080){
    strInterval <- "weekly"
    nodig <- "10"
    gfmt <- fmt <- "%Y-%m-%d"
    if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
      earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
    }
    step <- 5*365*24*60 - 40*24*60 
    goBackIter <- ceiling(as.numeric(difftime(latestDate,earliestDate,unit="min")/step))
  }else if(freq==43200){
    strInterval <- "monthly"
    nodig <- "10"
    gfmt <- fmt <- "%Y-%m-%d"
    if(earliestDate<as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")){
      earliestDate <- as.POSIXct("2004-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
    }
    step <- 1 
    goBackIter <- 1
  }else{
    stop("Not a valid frequency.")
  }
  
  return(list(step=step,goBackIter=goBackIter,gfmt=gfmt,fmt=fmt,nodig=nodig,strInterval=strInterval))
}

#########################################################################

create_outputfolders <- function(dataDIR,strInterval,geo,prop){
  # Create the output folders
  dir.create(file.path(dataDIR, strInterval), showWarnings = FALSE)
  outDIR <- paste0(dataDIR,strInterval,"/")
  dir.create(file.path(outDIR, prop), showWarnings = FALSE)
  outDIR <- paste0(outDIR,prop,"/")
  dir.create(file.path(outDIR, geo), showWarnings = FALSE)
  outDIR <- paste0(outDIR,geo,"/")
  dir.create(file.path(outDIR, "raw"), showWarnings = FALSE)
  return(outDIR)
}

#########################################################################
remove_raw_data <- function(uselocal,query,outDIR){
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
}


#########################################################################
# DOWNLOAD DATA
#########################################################################

download_gtrends <- function(intervalstr,query,region,prop,tz,uselocal){
  library("gtrendsR")
  # Get new results
  attempt <- 1
  tmpres <- NULL
  restartedTOR <- FALSE
  # Try 5 times then give up
  while( is.null(tmpres) && attempt <= 5 && (!restartedTOR) ) {
    attempt <- attempt + 1
    if((attempt == 5) && (!restartedTOR)){
      tor$kill()
      tor <<- process$new(command="tor")
      Sys.sleep(10)
      restartedTOR <- TRUE
      attempt <- 1
    }
    if(!uselocal){
      ###################################################
      # ERASE THIS BEFORE PUBLICATION!
      ###################################################
      account <- sample(c(1,2,3,4),1) # THE NUMBER OF YOUR GOOGLE ACCOUNTS GOES HERE
      username <- # THE VECTOR OF YOUR GOOGLE ACCOUNTS GOES HERE
      passwd <- # THE VECTOR OF THE PASSWORDS FOR YOUR GOOGLE ACCOUNTS GOES HERE
      ###################################################
      gtrendsR::setHandleParameters(user = username[account], 
                                    password = passwd[account],
                                    proxyhost = "socks5://127.0.0.1",
                                    proxyport = 9050)
    }
    try(
      tmpres <- gtrends(query,
                          time = rep(intervalstr,length(query)), 
                          geo=rep(region,length(query)) ,
                          gprop=prop,
                          tz = tz,
                          onlyInterest=TRUE)$interest_over_time
    )
  }
  if(is.null(tmpres)){return(NULL)}
  
  if(length(query)>1){
    res <- zoo(as.numeric(tmpres[tmpres$keyword==query[1],2]),order.by = as.POSIXct(tmpres[tmpres$keyword==query[1],1],tz="UCT"))
    
    for(jj in 2:length(query)){
      res <- merge(res,zoo(as.numeric(tmpres[tmpres$keyword==query[jj],2]),order.by = as.POSIXct(tmpres[tmpres$keyword==query[jj],1],tz="UCT")))
    }
    
  }else{
    res <- zoo(as.numeric(tmpres[,2]),order.by = as.POSIXct(tmpres[,1],tz="UCT"))
  }
  
  return(res)
}



#########################################################################
# CHECK OVERLAP
#########################################################################
check_overlap <- function(query,tmp,limit,slimit,length.query,redo,uselocal){
  tmp <- as.data.frame(tmp)
  tmp <- na.fill0(tmp,0)
  # There are three possible states
  # 001: High intensity gtrends: Use RBC
  # 002: Low intensity gtrends: Enlarge sample --> Download again
  # 003: Low intensity gtrends: Simple concatenation
  # 004: Low intensity gtrends: RBC concatenation
  length.query <- length(query)
  
  if(length.query>1){
    # Store the columns that belong to the query and the compare level in one row
    # First row is always the query, second row is always the comparison level
    query.cols <- sapply(1:length.query,function(x) seq(x,ncol(tmp),length.query))
    
    # We require 'slimit' dates with nonzero values in either of the two time frames
    number_zeros_lt_slimit <- sapply(query.cols[1,],function(x) (sum(tmp[,x]==0)>slimit))
    number_zeros_lt_slimit <- sum(number_zeros_lt_slimit)>1
    
  }else{
    query.cols <- matrix(1:2,2,1)
    # We require 'slimit' dates with nonzero values in either of the two time frames
    number_zeros_lt_slimit <- sapply(query.cols,function(x) (sum(tmp[,x]==0)>slimit))
    number_zeros_lt_slimit <- sum(number_zeros_lt_slimit)>1
    # if we have number_zeros_lt_slimit==TRUE  then we are in a low intensity case
  }
  
  
  # We require 'limit' dates with nonzero values in both time frames
  number_zeros_lt_limit <- sum(apply(tmp,1,sum)==0)>limit
  # if we have number_zeros_lt_limit==TRUE  then we are in a low intensity case
  
  # Have we already redone everything once?
  # -- Yes, we have redone it.
  if(redo|uselocal){
    
    if((number_zeros_lt_slimit | number_zeros_lt_limit)){
      return("003")
    }else{
      return("001")
    }
  # -- No, we have not redone it.
  }else{
    #Low intensity?
    # -- Yes, low intensity.
    if((number_zeros_lt_slimit | number_zeros_lt_limit)){
      return("002")
    # -- No, high intensity.
    }else{
        return("001")
      }
  } 
}


#########################################################################
# REGRESSION
#########################################################################

do.regression <- function(tmp_endres_merge,endres,res,state,testid,siglevel,meanAlgo,query,searchterm_list){
  
  if(length(tmp_endres_merge)!=1){ 
    if(length(query)==1){ # This is the case when we only want to download one time series
      
      X <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,2]))
      Y <- as.matrix(tmp_endres_merge[,1])
      
      na.X <- which(X[,2]==0)
      na.Y <- which(Y==0)
      
      if((length(na.X)!=0)|(length(na.Y)!=0)){
        X <- X[-union(na.X,na.Y),]
        Y <- Y[-union(na.X,na.Y)]
      }
      
      if(length(Y)<25){
        meanAlgo<-TRUE
      }
      
      all.constant.X <- sum(sapply(c(X[,2]),function(x) x==as.numeric(X[1,2])))==length(X[,2])
      
      
      all.zero.Y <- sum(Y==0) == length(Y)
      
      # In this object we collect the fitted values
      Yhat <- matrix(0,NROW(res),1)
      
      if (all.constant.X|meanAlgo){
        test <- as.data.frame(tmp_endres_merge)
        test[test==0] <- NA
        # If all X are constant, then we use the ratio of the means
        mean.vec <- apply(test,2,mean,na.rm=T)
        if(mean.vec[2]!=0){
          ratio <- mean.vec[1]/mean.vec[2]
        }else{
          ratio <- 1
        }
        
        
        # Predict the NAs in the non-overlapping area (basically with the mean)
        Yhat <- as.matrix(res)*ratio
      }
      else if(all.zero.Y){
        Yhat <- 0
      }
      else{
        
        nn <- nrow(X)
        XXinv <- solve(t(X)%*%X)
        beta <- XXinv%*%t(X)%*%Y
        # Calculate the errors
        u <- as.vector(Y-X%*%beta)
        # Calculate the heteroskedasticity consistent variance estimator
        sigma2 <- XXinv%*%(t(X)%*%diag(u^2)%*%X)%*%t(XXinv)
        
          
          if(state=="003"){

            # Test whether beta_0 > 1.5 (one-sided)
            test_alpha1 <- sqrt(nn)*(beta[1])/sqrt(sigma2[1,1])
            p.val1 <- 2*min(pt(test_alpha1,nn-2,lower.tail = T),pt(test_alpha1,nn-2,lower.tail = F))
            # Calculate R^2
            Yhat1 <- X%*%beta
            R2 <- 1-sum((Y-Yhat1)^2)/(sum((Y-mean(Y))^2))
            
            if (p.val1<siglevel){
              
              # If we are in a low intensity case, then we use the ratio of the  means of the entire
              # samples
              mean.vec <- c(mean(ifelse(endres==0,NA,endres),na.rm=T),mean(ifelse(res==0,NA,res),na.rm=T))
              if(mean.vec[2]!=0){
                ratio <- mean.vec[1]/mean.vec[2]
              }else{
                ratio <- 1
              }
              
              
              # Predict the NAs in the non-overlapping area (basically with the mean)
              Yhat <- as.matrix(res)*ratio
            }
          }
          else{
            if (testid=="exalpha"){
              # Calculate the test statistic
              test_alpha <- sqrt(nn)*(beta[1]-1)/sqrt(sigma2[1,1])
              p.val <- 2*min(pt(test_alpha,nn-2,lower.tail = T),pt(test_alpha,nn-2,lower.tail = F))
              if (T){#} ((p.val<siglevel)){
                #If the tests cannot be rejected
                # then exclude the constant
                XXinv <- solve(t(X[,2])%*%X[,2])
                beta <- XXinv%*%t(X[,2])%*%Y
                beta <- c(0,beta)
              }
            }
            
            Xmat <- cbind(rep(1,NROW(res)),as.vector(res))
            
            Yhat <- Xmat%*%beta
          }
        }
      
      
      
      final <- merge(endres,cbind(res,Yhat)[,2])
      attr(index(final),"tzone") <- "UTC"
      final[is.na(final[,1]),1] <- final[is.na(final[,1]),2]
      return(final[,1])
      
    }
    else{ 
      
      # This is the case when we  want to download multiple time series
      comp.col <- which(query==searchterm_list[which(prices_vec==compare_level)])
      plev.col <- which(query!=searchterm_list[which(prices_vec==compare_level)])
      
      query.cols <- sapply(1:length(query),function(x) seq(x,length(query)*2,length(query)))
      
      # Comparison level
      X.comp <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,query.cols[2,comp.col]]))
      X.comp[is.infinite(X.comp)] <- 0
      Y.comp <- as.matrix(tmp_endres_merge[,query.cols[1,comp.col]])
      Y.comp[is.infinite(Y.comp)] <- 0
      
      # Specific price level
      X.plev <- cbind(rep(1,NROW(tmp_endres_merge)),as.matrix(tmp_endres_merge[,query.cols[2,plev.col]]))
      X.plev[is.infinite(X.plev)] <- 0
      Y.plev <- as.matrix(tmp_endres_merge[,query.cols[1,plev.col]])
      Y.plev[is.infinite(Y.plev)] <- 0
      
      
      
      na.X.plev <- which(X.plev[,2]==0)
      na.Y.plev <- which(Y.plev==0)
      
      if((length(na.X.plev)!=0)|(length(na.Y.plev)!=0)){
        X.plev <- as.matrix(X.plev[-union(na.X.plev,na.Y.plev),])
        Y.plev <- as.matrix(Y.plev[-union(na.X.plev,na.Y.plev)])
      }
      
      na.X.comp <- which(X.comp[,2]==0)
      na.Y.comp <- which(Y.comp==0)
      
      if((length(na.X.comp)!=0)|(length(na.Y.comp)!=0)){
        X.comp <- as.matrix(X.comp[-union(na.X.comp,na.Y.comp),])
        Y.comp <- as.matrix(Y.comp[-union(na.X.comp,na.Y.comp)])
      }
      if(nrow(X.plev)<15){
        meanAlgo <- TRUE
      }
      if(nrow(X.comp)<15){
        meanAlgo <- TRUE
      }
      
      # 
      if(!meanAlgo){
        # Check if one of the X time series is constant
        all.constant.X.comp <- sum(sapply(c(X.comp[,2]),function(x) x==as.numeric(X.comp[1,2])))==length(X.comp[,2])
        all.constant.X.plev <- sum(sapply(c(X.plev[,2]),function(x) x==as.numeric(X.plev[1,2])))==length(X.plev[,2])
        
        all.zero.Y.comp <- sum(Y.comp==0) == length(Y.comp)
        all.zero.Y.plev <- sum(Y.plev==0) == length(Y.plev)
      }else{
        all.constant.X.comp <- all.constant.X.plev <- all.zero.Y.comp <- all.zero.Y.plev <- T
      }
      
      
      # In this object we collect the fitted values
      Yhat <- matrix(0,NROW(res),NCOL(res))
      
      # First, treat the price level
      if (all.constant.X.plev|meanAlgo){
        test <- as.data.frame(tmp_endres_merge[,query.cols[,comp.col]])
        test[test==0] <- NA
          
        # If all X are constant, then we use the ratio of the means
        mean.vec <- apply(test,2,mean,na.rm=T)
        if(mean.vec[2]!=0){
          ratio <- mean.vec[1]/mean.vec[2]
        }else{
          ratio <- 1
        }

        # Predict the NAs in the non-overlapping area (basically with the mean)
        Yhat[,query.cols[1,plev.col]] <- as.matrix(res[,query.cols[1,plev.col]])*ratio
      }
      else if(all.zero.Y.plev){
        Yhat[,query.cols[1,plev.col]] <- mean(res[,query.cols[1,plev.col]])
      }
      else{
        
        nn.plev <- nrow(X.plev)
        XXinv.plev <- solve(t(X.plev)%*%X.plev)
        beta.plev <- XXinv.plev%*%t(X.plev)%*%Y.plev
        
        # Calculate the errors
        u.plev <- as.vector(Y.plev-X.plev%*%beta.plev)
        
        # Calculate the heteroskedasty consistent variance estimator
        sigma2.plev <- XXinv.plev%*%(t(X.plev)%*%diag(u.plev^2)%*%X.plev)%*%t(XXinv.plev)
        
        if(state=="003"){
          # Test whether beta_0 > 1.5 (one-sided)
          test_alpha1.plev <- sqrt(nn.plev)*(beta.plev[1]-1.5)/sqrt(sigma2.plev[1,1])
          
          # Calculate R^2
          Yhat1.plev <- X.plev%*%beta.plev
          if((sum((Y.plev-mean(Y.plev))^2))==0){
            R2.plev <- 1
          }else{
            R2.plev <- 1-sum((Y.plev-Yhat1.plev)^2)/(sum((Y.plev-mean(Y.plev))^2))
          }
          
          
          if ((R2.plev<0.1) && (test_alpha1.plev<qt(1-siglevel,nn.plev-2))){
            
            # Create full dataset with NAs on non-overlapping dates
            final <- cbind(endres,res)
            # Select price level time series
            final <- final[,query.cols[,plev.col]]
            
            
            # If all X are constant, then we use the ratio of the means
            mean.vec <- apply(final,2,mean,na.rm=T)
            if(mean.vec[2]!=0){
              ratio <- mean.vec[1]/mean.vec[2]
            }else{
              ratio <- 1
            }
            
            # Predict the NAs in the non-overlapping area (basically with the mean)
            Yhat[,query.cols[1,plev.col]] <- as.matrix(res[,query.cols[1,plev.col]])*ratio
          }
        }
        else{
          
          if (testid=="exalpha"){
            
            # Calculate the test statistic
            if(sigma2.plev[1,1]==0){#Handle the pathological case, when all values are the same but one -- in X or Y
              test_alpha.plev <- 1000
            }else{
              test_alpha.plev <- sqrt(nn.plev)*(beta.plev[1])/sqrt(sigma2.plev[1,1])
            }
            
            p.val.plev <- 2*min(
              pt(test_alpha.plev,nn.plev-2,lower.tail = T),
              pt(test_alpha.plev,nn.plev-2,lower.tail = F)
            )
            if ((p.val.plev<siglevel)){
              #If the tests can be rejected
              # then exclude the constant because then it is to big
              XXinv.plev <- solve(t(X.plev[,2])%*%X.plev[,2])
              beta.plev <- XXinv.plev%*%t(X.plev[,2])%*%Y.plev
              beta.plev <- c(0,beta.plev)
            }
          }
          # Predict the NAs in the non-overlapping area (basically with the mean)
          X.plev1 <- cbind(rep(1,NROW(res)),as.matrix(res[,query.cols[1,plev.col]]))
          Yhat[,query.cols[1,plev.col]] <- X.plev1%*%beta.plev
        }
        
      }
      
      
      if (all.constant.X.comp|meanAlgo){
        test <- as.data.frame(tmp_endres_merge[,query.cols[,comp.col]])
        test[test==0] <- NA
        # If all X are constant, then we use the ratio of the means
        mean.vec <- apply(test,2,mean,na.rm=T)
        ratio <- mean.vec[1]/mean.vec[2]
        
        
        # Predict the NAs in the non-overlapping area (basically with the mean)
        Yhat[,query.cols[1,comp.col]] <- as.matrix(res[,query.cols[1,comp.col]])*ratio
      }
      else if(all.zero.Y.comp){
        Yhat[,query.cols[1,comp.col]] <- mean(res[,query.cols[1,comp.col]])
      }
      else{
        nn.comp <- nrow(X.comp)
        XXinv.comp <- solve(t(X.comp)%*%X.comp)
        beta.comp <- XXinv.comp%*%t(X.comp)%*%Y.comp
        
        # Calculate the errors
        u.comp <- as.vector(Y.comp-X.comp%*%beta.comp)
        
        # Calculate the heteroskedasty consistent variance estimator
        sigma2.comp <- XXinv.comp%*%(t(X.comp)%*%diag(u.comp^2)%*%X.comp)%*%t(XXinv.comp)
        
        if(state=="003"){
          # Test whether beta_0 > 1.5 (one-sided)
          test_alpha1.comp <- sqrt(nn.comp)*(beta.comp[1]-1.5)/sqrt(sigma2.comp[1,1])
          # Calculate R^2
          Yhat1.comp <- X.comp%*%beta.comp
          R2.comp <- 1-sum((Y.comp-Yhat1.comp)^2)/(sum((Y.comp-mean(Y.comp))^2))
          
          if ((R2.comp<0.1) && (test_alpha1.comp<qt(1-siglevel,nn.comp-2))){
            
            # Create full dataset with NAs on non-overlapping dates
            final <- cbind(endres,res)
            # Select comparison level time series
            final <- final[,query.cols[,comp.col]]
            
            # If all X are constant, then we use the ratio of the means
            mean.vec <- apply(final,2,mean,na.rm=T)
            if(mean.vec[2]!=0){
              ratio <- mean.vec[1]/mean.vec[2]
            }else{
              ratio <- 1
            }
            
            # Predict the NAs in the non-overlapping area (basically with the mean)
            Yhat[,query.cols[1,comp.col]] <- as.matrix(res[,query.cols[1,comp.col]])*ratio
          }
        }
        
        if (testid=="exalpha"){

          # Calculate the test statistic
          if(sigma2.comp[1,1]==0){#Handle the pathological case, when all values are the same but one -- in X or Y
            test_alpha.comp <- 1000
          }else{
            test_alpha.comp <- sqrt(nn.comp)*(beta.comp[1])/sqrt(sigma2.comp[1,1])
          }
          
          p.val.comp <- 2*min(
                                pt(test_alpha.comp,nn.comp-2,lower.tail = T),
                                pt(test_alpha.comp,nn.comp-2,lower.tail = F)
                              )
          if ((p.val.comp<siglevel)){
            #If the tests cannot be rejected
            # then exclude the constant
            XXinv.comp <- solve(t(X.comp[,2])%*%X.comp[,2])
            beta.comp <- XXinv.comp%*%t(X.comp[,2])%*%Y.comp
            beta.comp <- c(0,beta.comp)
          }
        }
        # Predict the NAs in the non-overlapping area (basically with the mean)
        X.comp1 <- cbind(rep(1,NROW(res)),as.matrix(res[,query.cols[1,plev.col]]))
      
        Yhat[,query.cols[1,comp.col]] <- X.comp1%*%beta.comp
      
        }
     
      
      
      
      Yhat <- zoo(Yhat, order.by=index(res))
      
      tmp2 <- cbind(res,Yhat)
      final <- merge(endres,tmp2[,(length(query)+1):NCOL(tmp2)])
      attr(index(final),"tzone") <- "UTC"
      final[is.na(final[,1]),1:length(query)] <- final[is.na(final[,1]),(length(query)+1):NCOL(tmp2)]
      endres <- final[,1:length(query)]
    }
    
    return(endres)
    
  }
}



#############################################
# Main Function
############################################

main_loop <- function(geo_list,
                      freq_list,
                      tz,
                      searchterm_list = NULL,
                      prices_vec = NULL,
                      slimit,
                      limit,
                      prop,
                      compare_level = NULL,
                      latestDate,
                      earliestDate,
                      uselocal,
                      dataDIR,
                      overlap,
                      meanAlgo,
                      download_comparison_level
                      ){

  if((!is.null(prices_vec))&(is.null(compare_level))){
    stop("You need a comparison level, if you supply prices")
  }
  
  for(geo in geo_list){
    #geo <- "US"
    #geo <- "DE"
    if(!is.null(prices_vec)){
      searchterm_list <- get_search_term_list(geo,prices_vec)
    }
    
   
    
    # searchterm_list <- c("unemployment benefit")#get_search_term_list(geo)
    # searchterm_list <- c("DAX")#get_search_term_list(geo)
    
    for(freq in freq_list){
      #freq <- freq_list[1] 
      if((freq == 60)|
         (prop == "news")){
        limit <- 15
        slimit <- 10
      }
      for (qq in searchterm_list){
        if(download_comparison_level){
          if(which(qq==searchterm_list)<2){next}
        }
        
        #qq <- searchterm_list[1]
        
        method = "backward"
        testid = "exalpha"
        
        if(!is.null(prices_vec)){
          if (qq==searchterm_list[which(prices_vec==compare_level)]){
            query <- qq
          }else{
            query <- c(qq,searchterm_list[which(prices_vec==compare_level)])
          }
        }
        else{
          query <- qq
        }
        
        
        print(paste0("Do the dance for ",query))
        
        
        region <- geo
        
        if(geo=="all"){
          region <- ""
        }
        
        # Get the frequency specific variables
        freq_vars <- get_freq_vars(freq, latestDate)
        step <- freq_vars$step
        goBackIter <- freq_vars$goBackIter
        gfmt <- freq_vars$gfmt
        fmt <- freq_vars$fmt
        nodig <- freq_vars$nodig
        strInterval <- freq_vars$strInterval
        
        # Create OUTPUT FOLDERS
        outDIR <- create_outputfolders(dataDIR,strInterval,geo,prop)
        
        # Remove raw data if uselocal is TRUE
        rc <- remove_raw_data(uselocal=uselocal,
                              query=query,
                              outDIR=outDIR)
        
        #########################################################################
        # PREPARE FIRST ITERATION
        #########################################################################
        start_date <- end_date <- NA
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
        ####################################################################
        if(!uselocal){
          uselocal <- FALSE
          while(TRUE){
            #Convert data to an amicable format
            intervalstr <- make_intervalstr(start_date,end_date,freq)
            
            # Save old results for next iteration
            # but only when we do NOT have a pathological case
            # and have to redo the download process
            if(!redo){
              resold <- res
              old_enddate <- end_date
            }
            
            # DOWNLOAD DATA
            res <- download_gtrends(intervalstr=intervalstr,query=query,region=region,prop=prop,tz=tz,uselocal=uselocal)
            names(res) <- NULL
            if(is.null(res)){break}
            # Merge new results ONLY beginning in the second iteration
            if(length(resold)==1){
              endres <- res
              # 1. Dump the downloaded data
              outstr <- gsub(" ","_",intervalstr)
              write.zoo(res,file=paste0(outDIR,"raw/",paste0(query,collapse="#"),"_",outstr,".csv"))
              # Create new end_date 
              end_date <- start_date+overlap*freq*60
              # Create new start_Date
              start_date <- end_date-step*60
            }else{
              # Merge between two subsequent time frames
              tmp <- na.omit(merge(resold,res))
              attr(index(tmp),"tzone") <- "UTC"
              names(tmp) <- NULL
              
              # Construct the merge for the endresult
              if(length(endres)!=1){
                tmp_endres_merge <- na.omit(merge(endres,res))
                attr(index(tmp_endres_merge),"tzone") <- "UTC"
                names(tmp_endres_merge) <- NULL
              }else{
                tmp_endres_merge <- NA
                endres <- res
              }
              
              # Merge the samples together
              tmp_endres_merge <-  as.data.frame(tmp_endres_merge)
              if(nrow(tmp_endres_merge)==0){break}
              names(tmp_endres_merge) <- c(sapply(1:length(query),function(x) paste0(c("Y","X"),x)))
              
              # Determine how to proceed, by checking the overlap
              state <- check_overlap(query=query,
                                     tmp=tmp_endres_merge,
                                     limit=limit,
                                     slimit=slimit,
                                     length.query = length(query),
                                     redo=redo,
                                     uselocal=uselocal)
              
              # There are three possible states
              # 001: High intensity gtrends: Use RBC
              # 002: Low intensity gtrends: Enlarge sample --> Download again
              # 003: Low intensity gtrends: Sample enlarged --> Decide what to do based on regression
              
              if(state %in% c("001","003")) {
                redo <- FALSE
              }else if(state=="002"){
                redo <- TRUE
              }
              
              # Adjust start and end dates
              if(redo){
                end_date <- end_date+overlap*freq*60
                start_date <- end_date-step*60
              }else{
                end_date <- start_date+overlap*freq*60
                start_date <- end_date-step*60
              }
              
              
              if(!redo){
                ########################################################################
                # In case everything is fine ...
                ########################################################################
                # 1. Dump the downloaded data
                outstr <- gsub(" ","_",intervalstr)
                write.zoo(res,file=paste0(outDIR,"raw/",paste0(query,collapse="#"),"_",outstr,".csv"))
                
                # 2. Do the regression and fitting
                
                endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
                                        endres=endres,
                                        res=res,
                                        state=state,
                                        siglevel=siglevel,
                                        testid=testid,
                                        meanAlgo=meanAlgo,
                                        query=query,
                                        searchterm_list)
                names(endres) <- NULL
              }
              
              if(end_date <= earliestDate){
                break
              }
              if (last){
                break
              }
              if(freq==43200){
                break
              }
              
              if(start_date <= earliestDate){
                start_date <- earliestDate
                end_date <- start_date + step*60
                last <- TRUE
              }
            }
          }# ENDE MAIN LOOP
          
          
          # Dump the merged data
          write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
          
          
          
          rm("resold")
          rm("endres")
          
        }
        else{
          uselocal <- TRUE
          ####################################
          # Concatenate backward (exalpha) LOCAL!!!
          ####################################
          method = "backward"
          testid = "exalpha"
          
          searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
          flist <- list.files(path=paste0(outDIR,"/raw"))
          flist <- flist[grepl(paste0("^",searchstr),flist)]
          dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
          if (length(dates)==0){
            next
          }
          if(length(query)==1){
            dates <- dates[!grepl("#",dates)]
            dates <- dates[order(as.POSIXct(dates,format=gfmt))]
            fname <- flist[grepl(dates[length(dates)],flist)&!grepl("#",flist)]
          }
          else{
            dates <- dates[order(as.POSIXct(dates,format=gfmt))]
            fname <- flist[grepl(dates[length(dates)],flist)]
          }
          
          
          endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
          endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
          
          
          if(length(dates)>=2){
            for(jj in (length(dates)-1):1){
              #jj <- length(dates)-1
              if(length(query)==1){
                fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
              }else{
                fname <- flist[grepl(dates[jj],flist)]
              }
              
              res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
              res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
              
              tmp_endres_merge <- na.omit(merge(endres,res))
              attr(index(tmp_endres_merge),"tzone") <- "UTC"
              if(nrow(tmp_endres_merge)==0){break}
              if(length(tmp_endres_merge)==0){break}
              # Determine how to proceed, by checking the overlap
              state <- check_overlap(query=query,
                                     tmp=tmp_endres_merge,
                                     limit=limit,
                                     slimit=slimit,
                                     length.query = length(query),
                                     redo=T,
                                     uselocal=uselocal)
              endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
                                      endres=endres,
                                      res=res,
                                      state=state,
                                      siglevel=siglevel,
                                      testid=testid,
                                      meanAlgo=meanAlgo,
                                      query=query,
                                      searchterm_list)
              
            }
          }
          
          # Dump the merged data
          write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
          rm("endres")
        }
        
        if(freq!=43200){
          
          ####################################
          # Concatenate backward (inalpha)
          ####################################
          method = "backward"
          testid = "inalpha"
          
          searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
          flist <- list.files(path=paste0(outDIR,"/raw"))
          flist <- flist[grepl(paste0("^",searchstr),flist)]
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
              if(nrow(tmp_endres_merge)==0){break}
              state <- check_overlap(query=query,
                                     tmp=tmp_endres_merge,
                                     limit=limit,
                                     slimit=slimit,
                                     length.query = length(query),
                                     redo=T,
                                     uselocal=uselocal)
              endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
                                      endres=endres,
                                      res=res,
                                      state=state,
                                      siglevel=siglevel,
                                      testid=testid,
                                      meanAlgo=meanAlgo,
                                      query=query,
                                      searchterm_list)
              
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
          flist <- flist[grepl(paste0("^",searchstr),flist)]
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
              #jj <- 5
              if(length(query)==1){
                fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
              }else{
                fname <- flist[grepl(dates[jj],flist)]
              }
              
              
              res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
              res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
              
              tmp_endres_merge <- na.omit(merge(endres,res))
              attr(index(tmp_endres_merge),"tzone") <- "UTC"
              if(nrow(tmp_endres_merge)==0){break}
              state <- check_overlap(query=query,
                                     tmp=tmp_endres_merge,
                                     limit=limit,
                                     slimit=slimit,
                                     length.query = length(query),
                                     redo=T,
                                     uselocal=T)
              endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
                                      endres=endres,
                                      res=res,
                                      state=state,
                                      siglevel=siglevel,
                                      testid=testid,
                                      meanAlgo=meanAlgo,
                                      query=query,
                                      searchterm_list)
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
          flist <- flist[grepl(paste0("^",searchstr),flist)]
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
              if(nrow(tmp_endres_merge)==0){break}
              state <- check_overlap(query=query,
                                     tmp=tmp_endres_merge,
                                     limit=limit,
                                     slimit=slimit,
                                     length.query = length(query),
                                     redo=T,
                                     uselocal=uselocal)
              endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
                                      endres=endres,
                                      res=res,
                                      state=state,
                                      siglevel=siglevel,
                                      testid=testid,
                                      meanAlgo=meanAlgo,
                                      query=query,
                                      searchterm_list)
              
            }
          }
          
          # Dump the merged data
          write.zoo(endres,file=paste0(outDIR,paste0(query,collapse="#"),"_",method,"_",testid,".csv"))
          Sys.sleep(1)
          rm("endres")
        }
      }
    }
  }
}
