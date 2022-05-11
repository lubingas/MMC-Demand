#####################################################################
#####################################################################
# CLEANING MEMORY AND LOADING FUNCTIONS                             #
#####################################################################
#####################################################################

gc()

# switch off file writing if in use
if(sink.number()>0) sink()

# delete all variables
rm(list = ls())

# load functions
#library(plyr)
#library(fastDummies)
#library(date)
#library(ggplot2)
#library(lavaan)
#library(psych)
#library(readstata13)
#library(foreign)
#library(xlsx)

## Function to arrange data-frames variables by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

# write small function to compute individual discount rates
discount <- function(Xv,Xs,v,s) {
  ((Xv/Xs)**(1/(v-s)) - 1)
}

## some data manipulations
Grouper <- function(x) {
  i = cut_interval(x, n=5, right=FALSE, labels=c(-2,-1,0,1,2))
  i = as.numeric(levels(i))[i]
  i
}

## some data manipulations
GrouperX <- function(x) {
  i = cut_interval(x, n=5, labels=c(-2,-1,0,1,2))
  i = as.numeric(levels(i))[i]
  i
}


# social values function
getsocvals <- function(self,other,socval){
    payself <- c()
    payother <- c()
    for(i in 1:length(socval)) {
        if (socval[i]==1) {
            payself[i] <- self[1]
            payother[i] <- other[1]
        } else if (socval[i]==2) {
            payself[i] <- self[2]
            payother[i] <- other[2]
        } else if (socval[i]==3) {
            payself[i] <- self[3]
            payother[i] <- other[3]
        } else if (socval[i]==4) {
            payself[i] <- self[4]
            payother[i] <- other[4]
        } else if (socval[i]==5) {
            payself[i] <- self[5]
            payother[i] <- other[5]
        } else if (socval[i]==6) {
            payself[i] <- self[6]
            payother[i] <- other[6]
        } else if (socval[i]==7) {
            payself[i] <- self[7]
            payother[i] <- other[7]
        } else if (socval[i]==8) {
            payself[i] <- self[8]
            payother[i] <- other[8]
        } else if (socval[i]==9) {
            payself[i] <- self[9]
            payother[i] <- other[9]
        }
    }
    return(cbind(payself-5000,payother-5000))
}

### some functions to compute summary statistics and tabulate them
descrfx <- function(x) {
    #    miss <- paste(sum(is.na(x))," (",round(100*sum(is.na(x))/length(x),2),"%)",sep="")
    mean <- paste0(round(mean(as.numeric(x), na.rm=T),2)," (",round(sd(as.numeric(x), na.rm=T),2),")")
    return(c(mean))
}

## 2. by group
subdescfx <- function(x) {
    return(cbind(descrfx(x[d$block==1]),descrfx(x[d$block==2]),descrfx(x[d$block==3])))
}

### for categorical variables
descrfx2 <- function(x) {
    percent <- paste0(sum(x,na.rm=T)," (",round(mean(as.numeric(x), na.rm=T),2)*100,"%)")
    return(c(percent))
}

## 2. by group
subdescfx2 <- function(x) {
    return(cbind(descrfx2(x[d$block==1]),descrfx2(x[d$block==2]),descrfx2(x[d$block==3])))
}

save.xlsx <- function(file, ...) {
    objects <- list(...)
    fargs <- as.list(match.call(expand.dots=TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i==1)
        write.xlsx(objects[[i]],file,sheetName=objnames[i],col.names=TRUE,row.names=TRUE,showNA=FALSE)
        else write.xlsx(objects[[i]],file,sheetName=objnames[i],col.names=TRUE,row.names=TRUE,showNA=FALSE,append = TRUE)
    }
    print(paste("Workbook",file,"has",nobjects,"worksheets."))
}



# define function for detaching variables
detachAllData <-
function () 
{
    pos.to.detach <- (1:length(search()))[substring(search(), 
        first = 1, last = 8) != "package:" & search() != ".GlobalEnv" & 
        search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
    for (i in 1:length(pos.to.detach)) {
        if (length(pos.to.detach) > 0) {
            detach(pos = pos.to.detach[1])
            pos.to.detach <- (1:length(search()))[substring(search(), 
                first = 1, last = 8) != "package:" & search() != 
                ".GlobalEnv" & search() != "Autoloads" & search() != 
                "CheckExEnv" & search() != "tools:rstudio" & 
                search() != "TempEnv"]
        }
    }
}

tablesummary=function(x){
  apply(x,2,function(x) c(Min=min(x), 
                          "1st Qu" =quantile(x, 0.25,names=FALSE), 
                          Median = quantile(x, 0.5, names=FALSE), 
                          Mean= mean(x), 
                          Sd=sd(x), 
                          "3rd Qu" = quantile(x,0.75,names=FALSE), 
                          IQR=IQR(x), 
                          Max = max(x)))
}

# run detach function
detachAllData()
