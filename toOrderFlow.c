#include <stdlib.h>
#include <stdio.h>


/* real working c function*/
void writeRecord(double *record, double tim, double market,double size, double price, double bs, 
        double hprice, double tradePrice, double tradeSize){
  record[0]=tim;
  record[1]=market;
  record[2]=size;
  record[3]=price;
  record[4]=bs;
  record[5]=hprice;
  if (tradePrice >0)
    record[6]=tradePrice/tradeSize;
  else 
      tradePrice =0;
  record[7]=tradeSize;
}

void toOrderFlow(double * const orderflow, double const * const message,
    size_t rows, double tim){

  double *of=orderflow;
  size_t r;
  double time=0,marketable=0,size=0,price=0,bs=0,hprice=0,tradePrice=0,
         tradeSize = 0;
  time=message[0]; 
  marketable=(message[1]>3); 
  size=message[3]; 
  price=message[4];
  bs=message[5];

  for (r=1; r<rows; r++){
  /* Higer than time threshold or different types or previous is 
   * a new order*/
    if (message[r*6]-message[(r-1)*6]>tim  || message[r*6+5]!=bs || message[(r-1)*6+1]==1){
      writeRecord(of,time,marketable,size,price,bs,hprice,tradePrice,tradeSize); /*Write previous record*/
      of+=8;
      hprice = 0;
      time=message[r*6]; 
      marketable=(message[r*6+1]>3); 
      size=message[r*6+3]; 
      price=message[r*6+4];
      tradePrice = tradeSize = 0;
      bs=message[r*6+5];
      if (marketable){
          tradeSize += message[r*6+3];
          tradePrice += message[r*6+3]*message[r*6+4];
          bs = -bs;
      }
      if (message[r*6+1]==5)
        hprice = price;
      else
        hprice=0;
      continue;
    }
    /*type always be updated*/
    bs = message[r*6+5];
    /*trade with limit order. update marketable =1, size sum up, price be current one*/
    if (message[r*6+1]>3){
      marketable=1;
      size+=message[r*6+3];
      price=message[r*6+4];
      tradeSize +=message[r*6+3];
      tradePrice += message[r*6+3]*message[r*6+4];
      bs = -bs;
      /*If a hidden order execution. Update hidden price only if there is not previously recorded*/
      if (message[r*6+1] <5)
          hprice = 0;
      else {
          if (hprice==0)
          hprice=price;
      }
      continue;
    }
    
    /*A limit order which is the last part of a marketable orders*/
    if (message[r*6+1]==1)
        if ((bs<0 && message[r*6+4] < price) || (bs>0 && message[r*6+4] > price)){
             size+=message[r*6+3];
             price=message[r*6+4];
        }
  }
  writeRecord(of,time,marketable,size,price,bs,hprice, tradePrice,tradeSize); /*write the last record*/
  of +=8;
}

