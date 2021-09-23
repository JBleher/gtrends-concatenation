    # Clear workspace and graphs
if(!is.null(dev.list())) dev.off()
rm(list = ls()) 

# install.packages('Rcpp', dependencies = TRUE)
library("devtools")
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE,, dependencies = TRUE)
setwd("~/git")
# in your git repository should be a development version of gtrends... or install from Github...
install("gtrendsR",dependencies = TRUE)
library("gtrendsR")
library("zoo")
library("sandwich")
library("lmtest")
library("processx")

username <- Sys.info()["user"]
path <- paste0("/home/",username,"/git/Article11/")

source(paste0(path,"/R/FUNCTIONS.R"))
source(paste0(path,"/R/210302_JB_FUNCTIONS.R"))
##################################################
# TODO: 
#   - Turn this into a function, submit to gtrendsR
###################################################
#
# SET PARAMETERS
#
###################################################
uselocal <- F
tor <<- process$new(command="tor")

baseDIR <- path
dataDIR <- paste0(baseDIR,"DATA/GTRENDS_REV/")

# Set the value of the earliest and latest date
earliestDate <- as.POSIXct("2006-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
latestDate <- #as.POSIXct("2019-06-10T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT") #REVIEW
              as.POSIXct("2021-02-28T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT") # REV
              #as.POSIXct("2020-05-31T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT")
#as.POSIXct(Sys.time())

# Set the timezone offset to zero to get GMT time
tz <- 0

# Also set the number of observation that you want to overlap
overlap <- 30


# Set LIMITS for concatenation
limit <- 15 # Maximal number of dates with zero values in BOTH time frames
slimit <- 10 # Maximal number of dates with zero values in either of the two time frames


# Significance level for alpha in regression
siglevel <- 0.05

# frequency
freq_list <- c(1440)
#freq_list <- c(43200)
# geo: "US","DE","FR","LU"
geo_list <- c("all")# ,"LU"
#geo_list <- c("US")
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
prices_vec <- c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
#1,5,10,20,50,100,500,
#c(0,1,5,10,20,50,100,250,500,1000,1500,2000,5000,10000,15000,20000,50000,100000,250000,500000,1000000,5000000,100000000)
compare_level <-1
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Price levels with all major EuroArea languages ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
prop <- c("web")

# main_loop(geo_list="US",
#           freq_list=freq_list,
#           tz=tz,
#           searchterm_list = "unemployment benefit",
#           slimit=slimit,
#           limit=limit,
#           prop=prop,
#           latestDate=as.POSIXct("2020-05-31T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="UCT"),
#           earliestDate=earliestDate,
#           uselocal=TRUE,
#           dataDIR=paste0(baseDIR,"DATA/GTrends21/"),
#           overlap=overlap,
#           meanAlgo = F)

main_loop(geo_list=geo_list,
                      freq_list=freq_list,
                      tz=tz,
                      prices_vec = prices_vec,
                      slimit=slimit,
                      limit=limit,
                      prop=prop,
                      compare_level = compare_level,
                      latestDate=latestDate,
                      earliestDate=earliestDate,
                      uselocal=F,
                      dataDIR=dataDIR,
                      overlap=overlap,
                      meanAlgo = F,
                      download_comparison_level=F)

tor$kill()

#####################################
# DEBUG
#####################################
# #if(which(qq==searchterm_list)<2){next}
# geo <- "US"
# qq <- "unemployment benefit"
# 
# query <- qq
# freq <- freq_list[1]
# 
# 
# print(paste0("Do the dance for ",query))
# 
# 
# region <- geo
# 
# if(geo=="all"){
#   region <- ""
# }
# 
# 
# 
# # Get the frequency specific variables
# freq_vars <- get_freq_vars(freq, latestDate)
# step <- freq_vars$step
# goBackIter <- freq_vars$goBackIter
# gfmt <- freq_vars$gfmt
# fmt <- freq_vars$fmt
# nodig <- freq_vars$nodig
# strInterval <- freq_vars$strInterval
# 
# # Create OUTPUT FOLDERS
# outDIR <- create_outputfolders(dataDIR,strInterval,geo,prop)
# 
# 
# 
# rm("endres")
# method = "forward"
# testid = "inalpha"
# searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
# flist <- list.files(path=paste0(outDIR,"/raw"))
# flist <- flist[grepl(paste0("^",searchstr),flist)]
# dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
# 
# if(length(query)==1){
#   dates <- dates[!grepl("#",dates)]
#   dates <- dates[order(as.POSIXct(dates,format=gfmt))]
#   fname <- flist[grepl(dates[1],flist)&!grepl("#",flist)]
# }else{
#   dates <- dates[order(as.POSIXct(dates,format=gfmt))]
#   fname <- flist[grepl(dates[1],flist)]
# }
# endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
# endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
# 
# if(length(dates)>=2){
#   for(jj in 2:length(dates)){
#     
#     if(length(query)==1){
#       fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
#     }else{
#       fname <- flist[grepl(dates[jj],flist)]
#     }
#     res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
#     res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
#     
#     
#     tmp_endres_merge <- na.omit(merge(endres,res))
#     attr(index(tmp_endres_merge),"tzone") <- "UTC"
#     if(nrow(tmp_endres_merge)==0){break}
#     state <- check_overlap(query=query,
#                            tmp=tmp_endres_merge,
#                            limit=limit,
#                            slimit=slimit,
#                            length.query = length(query),
#                            redo=T,
#                            uselocal=uselocal)
#     endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
#                             endres=endres,
#                             res=res,
#                             state=state,
#                             siglevel=siglevel,
#                             testid=testid,
#                             meanAlgo=F,
#                             query=query)
#     
#   }
# }
# 
# 
# 
# 
# ####################################
# # Concatenate backward (inalpha)
# ####################################
# rm("endres")
# method = "backward"
# testid = "exalpha"
# 
# searchstr <- gsub("([\\+\\$])","\\\\\\1",paste0(query,collapse="#"))
# flist <- list.files(path=paste0(outDIR,"/raw"))
# flist <- flist[grepl(paste0("^",searchstr),flist)]
# dates <- gsub(paste0("^",searchstr,"_(.{",nodig,"})_.*$"),"\\1",flist)
# if(length(query)==1){
#   dates <- dates[!grepl("#",dates)]
#   dates <- dates[order(as.POSIXct(dates,format=gfmt))]
#   fname <- flist[grepl(dates[length(dates)],flist)&!grepl("#",flist)]
# }else{
#   dates <- dates[order(as.POSIXct(dates,format=gfmt))]
#   fname <- flist[grepl(dates[length(dates)],flist)]
# }
# endres <- read.table(paste0(outDIR,"raw/",fname),skip=2)
# endres <- zoo(as.matrix(endres[,2:(length(query)+1)]),order.by=as.POSIXct(endres[,1],format=fmt,tz ="UCT"))
# 
# 
# if(length(dates)>=2){
#   for(jj in (length(dates)-1):1){
#     
#     if(length(query)==1){
#       fname <-  flist[grepl(dates[jj],flist)&!grepl("#",flist)]
#     }else{
#       fname <- flist[grepl(dates[jj],flist)]
#     }
#     
#     res <- read.table(paste0(outDIR,"raw/",fname),skip=2)
#     res <- zoo(as.matrix(res[,2:(length(query)+1)]),order.by=as.POSIXct(res[,1],format=fmt,tz ="UCT"))
#     
#     
#     tmp_endres_merge <- na.omit(merge(endres,res))
#     attr(index(tmp_endres_merge),"tzone") <- "UTC"
#     if(nrow(tmp_endres_merge)==0){break}
#     state <- check_overlap(query=query,
#                            tmp=tmp_endres_merge,
#                            limit=limit,
#                            slimit=slimit,
#                            length.query = length(query),
#                            redo=T,
#                            uselocal=uselocal)
#     endres <- do.regression(tmp_endres_merge=tmp_endres_merge,
#                             endres=endres,
#                             res=res,
#                             state=state,
#                             siglevel=siglevel,
#                             testid=testid,
#                             meanAlgo=T,
#                             query=query)
#     
#   }
#   
# }







