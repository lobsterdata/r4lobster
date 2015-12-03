/*!
  \file Rlobsterutils.c

  \brief 

  \author Ruihong Huang

  \version:  1.0
  Copyright (c) 2012, Ruihong Huang
  */

#include <R.h>
#include <Rdefines.h>

#include "findPreceding.c"
#include "toOrderFlow.c"

void toOrderFlow_Interface(double * const orderflow, double const * const message, int* rows, double *tim)
{
  toOrderFlow(orderflow,message,*rows,*tim);
}

