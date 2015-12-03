#_____________________________________________________________________________

#						Introduction
#_____________________________________________________________________________


# 				Welcome to the LOBSTER R demo.

#              http://www.lobsterdata.com
#___________________________________________________________________________



#The code provided below might help you get started with your LOBSTER data. 
#The demo focuses on the two LOBSTER output files 'orderbook' and 'message'.

#You can find a detailed description of the LOBSTER data structure 
#at http://LOBSTER.wiwi.hu-berlin.de

#Data used: AMZN - 2012-June-21 - 10 Levels
#_____________________________________________________________________________



#_____________________________________________________________________________
#
#Set up the Basics
#load the libraries, the package gplots and graphics need to be installed
#_____________________________________________________________________________
rm(list=ls(all=TRUE))

library(graphics)
source('lobster.R')
## set the working directory
# setwd('C:/path/to/your/data/directory')

# Note: The files must be in the same working directory as the LOBSTER_demo.r file.
 
ticker  <- "AMZN"                     #TICKER 

# DATE for which data is downloaded , the file name you downloaded contains this string , say if you downloaded from 1st july 2009 , type here 2009-07-01
demodate = "2012-06-21"
starttime <- 34200000 
endtime <- 57600000
# Levels
lvl         = 10;

# Load data
filenameBook <- paste(paste(ticker , demodate ,starttime,endtime,"orderbook" ,lvl ,sep = "_"),"csv",sep = ".")
filenameMess <- paste(paste(ticker , demodate ,starttime,endtime,"message" ,lvl ,sep = "_"),"csv",sep = ".")

mess <- lobster.readMessage(filenameMess);
book <- lobster.readOrderBook(filenameBook)

# Visualize order book
# convert prices into dollars
book[,seq(1,(4*lvl),by=2)] <- book[,seq(1,(4*lvl),by=2)]/10000; 
idx <- which(mess[,1]<10.50*3600)
idx <- idx[length(idx)] # the index of the order book immediately before 10:30
obk <- new("OrderBook",ticker=ticker,timeStamp=mess[idx,1],bookData=book[idx,])
lobster.plot(obk)


