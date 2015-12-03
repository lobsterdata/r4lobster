# Copyright 2012-2015 frishedaten (service@lobsterdata.com) 
# 
# $Revision: 1.0.0 $
#
#
# Created: Wed 19 Sep 2012 01:28:58 PM CEST
#
# Last Modified: Wed 19 Nov 2015 01:29:53 PM CEST
#
# ==========================================================

library(tools)

######### Read Binary Order Book Data  ###################

readBinOrderBook <- function(filename,level,N=100000){
        zz <- file(filename,'rb')
        books = readBin(con=zz, 'int', n =N*4*level, size = 8, signed = TRUE,
                        endian = 'big' )
        close(zz)
        dim(books) <- c(4*level,length(books)/(4*level))
        books <- t(books)
        books <- as.data.frame(books)
        fields  <- rep('',4*level)
        for (i in 0:(level-1)){
                fields[i*4 + 1] = paste('ask.price.',i+1,sep='')
                fields[i*4 + 2] = paste('ask.size.',i+1,sep='')
                fields[i*4 + 3] = paste('bid.price.',i+1,sep='')
                fields[i*4 + 4] = paste('bid.size.',i+1,sep='')
        }
        names(books) <- fields
        row.names(books) <- NULL
        return(books)
}

readCSVOrderBook <- function(filename){
        books <- read.csv(filename,header=FALSE)
        books <- as.data.frame(books)
        level <- floor(dim(books)[2]/4);
        fields  <- rep('',4*level)
        for (i in 0:(level-1)){
                fields[i*4 + 1] = paste('ask.price.',i+1,sep='')
                fields[i*4 + 2] = paste('ask.size.',i+1,sep='')
                fields[i*4 + 3] = paste('bid.price.',i+1,sep='')
                fields[i*4 + 4] = paste('bid.size.',i+1,sep='')
        }
        names(books) <- fields
        row.names(books) <- NULL
        return(books)
}

lobster.readOrderBook <- function(filename,...){
        if (file_ext(filename)=='csv'){
                books <- readCSVOrderBook(filename)
        } else {
                books  <- readBinOrderBook(filename,...)
        }
        return(books)
}
#------------------- End -------------------------------#

######### Read Binary Message Data  #####################
readBinMessage <- function(filename,N=100000){
        zz <- file(filename,'rb')
        rawdata = readBin(con=zz, 'int', n =N*7, size = 8, signed = TRUE,
                          endian = 'big' )
        close(zz)
        rows <- length(rawdata)/7
        dim(rawdata) <- c(7,rows)
        eventtime <- rawdata[1,]+rawdata[2,]/(10^9)
        messages <- rbind(eventtime,rawdata[3:7,]);
        messages <- as.data.frame(t(messages))
        names(messages) <- c('time','type','orderid','effectivesize','price',
                             'direction')
        row.names(messages) <- NULL
        return(messages)
}

######### Read CSV Message Data  #####################
readCSVMessage <- function(filename){
        messages <- read.csv(filename,header=FALSE)
        messages <- as.data.frame(messages)
        names(messages) <- c('time','type','orderid','effectivesize','price',
                             'direction')
        row.names(messages) <- NULL
        return(messages)
}

lobster.readMessage <- function(filename,...){
        if (file_ext(filename)=='csv'){
                mess <- readCSVMessage(filename)
        } else {
                mess  <- readBinMessage(filename,...)
        }
        return(mess)
}

# ---------- Define Class OrderBook ------------------------------------#
setGeneric("lobster.plot", function(object) standardGeneric("lobster.plot"))

setClass('OrderBook',representation(bookData='data.frame',ticker='character',timeStamp='numeric',level='numeric'))
setMethod('initialize','OrderBook',function(.Object,ticker,timeStamp,bookData){
          .Object@ticker = ticker
          .Object@timeStamp = timeStamp
          .Object@bookData = bookData
          .Object@level = floor(length(bookData)/4 )
          return(.Object)
                             })
setMethod("lobster.plot",'OrderBook', function(object){
          colmatrix = c("red","green")

          par(oma = c( 0, 0, 3, 0 ),mfrow=c(1,2))
          prices=as.numeric(object@bookData[seq(from=1,by=2,length=2*object@level)])
          volumes=as.numeric(object@bookData[seq(from=2,by=2,length=2*object@level)])
          plot(prices,volumes, type="h" , lwd = 5,col = colmatrix,
               main = 'Order Book Shape', xlab ="Price($)", ylab = "Volume",)
          legend("top",c('BID', 'ASK'), lty = 1, col=c('green','red'),ncol=1 , , horiz = TRUE,inset = .03)
          askVol = cumsum(volumes[seq(1,2*object@level,2)])
          bidVol = cumsum(volumes[seq(2,2*object@level,2)])
          askVol = askVol / askVol[object@level]
          bidVol = bidVol / bidVol[object@level]
          plot(1:object@level,askVol ,type = "s" , col = 'red' ,lwd = 2 , ylim= c(-1,1) , xlab = "Level" , ylab = "% of volume", main='Relative Depth' )
          lines(1:object@level, -bidVol,type='s',col='green',lwd=2)
          legend("bottomleft",c('Ask','Bid' ), lty = 1, col=c('red','green' ),ncol=1 ,horiz = TRUE,  inset = .05)
          mtext(sprintf("%s, %s",object@ticker, lobster.timeStamp(object@timeStamp)), outer = TRUE,cex=2)
                             })

# -------------------- Some Utils ------------------------------------#

lobster.timeStamp <- function(timeinseconds){
        # Represent the time stamp in string
        hours <- floor(timeinseconds/3600)
        minutes <- floor((timeinseconds-3600*hours)/60)
        seconds <- timeinseconds-3600*hours-60*minutes
        timeStr <- sprintf("%02d:%02d:%012.9f",hours,minutes,seconds)
        return(timeStr)
}


