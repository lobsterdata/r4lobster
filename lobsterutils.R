# Copyright 2012 Ruihong Huang (ruihong.huang@wiwi.hu-berlin.com)
# 
# $Revision: 1.0.0 $
#
#
# Created: Wed 19 Sep 2012 01:28:58 PM CEST
#
# Last Modified: Wed 19 Sep 2012 01:29:53 PM CEST
#
# ==========================================================


######### Read Binary Order Book Data  ###################

readorderbook.bin <- function(filename,level,N=100000){
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

#------------------- End -------------------------------#

######### Read Binary Message Data  #####################
readmessage.bin <- function(filename,N=100000){
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

#------------------- End -------------------------------#

######### Read Order Trace Data  ########################
readordertrace.bin <- function(filename,N=1000000){
  zz <- file(filename,'rb')
  
  rawdata = readBin(con=zz, 'int', n =N*7, size = 8, signed = TRUE,
             endian = 'big' )
  close(zz)
  dim(rawdata) <- c(7,length(rawdata)/7)
  submittime <- rawdata[1,]+rawdata[2,]/(10^9) 
  eventtime <- rawdata[3,]+rawdata[4,]/(10^9)
  traces <- cbind(t(submittime),t(eventtime),t(rawdata[5:7,]))
  traces <- as.data.frame(traces)
  names(traces) <- c('submittime','eventtime','effectivesize',
                     'isexecution','executedbefore')
  row.names(traces) <- NULL
  rm(rawdata)
  return(traces)
}

#------------------- End -------------------------------#

######### Read Binary Limit Order Data  #################
readlimitorder.bin <- function(filename,N=100000000){
  zz <- file(filename,'rb')
  
  orders = readBin(con=zz, 'int', n =N*5, size = 8, signed = TRUE,
             endian = 'big' )
  close(zz)
  dim(orders) <- c(5,length(orders)/5)
  orders <- as.data.frame(t(orders))
  names(orders) <- c('id','size','remainsize','price','direction')
  row.names(orders) <- NULL
  return(orders)
}

#------------------- End -------------------------------#

#=====================================================================
#
# FINDPRECEDING  Find the index of an increasing series s2 that
# precedes another increasing series s1. 
#
# The calling syntax is:
#
#		[id] = findPreceding(series1,series2)
#
#
#====================================================================

dyn.load('Rlobsterutils.so')

find.preceding <- function(s1,s2)
{
  n1 <- length(s1)
  n2 <- length(s2)
  res <- .C('findPreceding',as.double(s1),as.double(s2),
            as.integer(n1),as.integer(n2),idx=double(n1))
  
  # Increase the idex by 1 for R index beginning with 1, while C
  # beginning  with 0

  return(res[['idx']] + 1) 
}

orderbook.snapshot <- function(messages, orderbooks,interval, 
                               start.time='9:30:00',end.time='16:00:00')
{
  if (is.numeric(start.time) && is.numeric(end.time))
    time.in.sec <- c(start.time,end.time)
  else
  {
    time.in.sec <- sapply(strsplit(c(start.time,end.time),':'),
                    function (x){
                      x <- as.numeric(x)
                      x[1]*3600+x[2]*60+x[3]
                    }
                      )
  }
  time.stamp  <- seq(time.in.sec[1],time.in.sec[2],by=interval)
  idx <- findPreceding(time.stamp,messages$time)
  snapshots <- orderbooks[idx,]
  snapshots$time <-time.stamp

  return(snapshots)
}
#=====================================================================
#
# MESSAGES2ORDERFLOW converge the message stream to the order flow 
#  where market orders are identified by using limit order executions.
#
# The calling syntax is:
#
#     flow = messages2orderflow(messages, time_threshold)
#
# hiddenprice -- If the marketable order executed with hidden orders
#                it would be the worst revealed hidden price
# tradeprice  -- The VWAP of execution part of a marketble order.
# tradeSize   -- The executed volume of a marketable order
#    
#====================================================================
message2orderflow <- function(messages,time_threshold=0.05)
{
  # take only submission and execution
  myms <- subset(messages, type==1 | type>3) 
  n <- length(myms$type)
  if (n==0)
    return(NULL)
  end
  num.msg  <- as.vector(t(data.matrix(myms)))
  flow <- rep(0,n*8);
  res <- .C('toOrderFlow_Interface',flow=flow, as.double(num.msg),
            as.integer(n), as.double(time_threshold))
  myfl <- res[['flow']]
  dim(myfl) <- c(8,length(myfl)/8)
  myfl <- as.data.frame(t(myfl))
  names(myfl) <- c('time','marketable','size','limitprice','direction', 'hiddenprice','tradeprice','tradesize')

  myfl <- myfl[myfl$time>0,]
  rm(num.msg,res) 
  return(myfl)
}


#some testing
#load('../data/GPRO_20110428_38757.Rdata')
#testdata<-mydata$data[['20110131']];
#tt = message2orderflow(testdata$messages)
tt = readordertrace.bin('../rawData/WHRT_2011-08-24_35704700_52200000_trace.bin') 

