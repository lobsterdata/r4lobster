int findPreceding(double* s1, double* s2, int* n1p, int* n2p, double* ix){
  int i=0;
  int j=0;
  int n1,n2;
  n1 = *n1p;
  n2 = *n2p;
  while (i<n1 && j<n2){
    if (*(s1+i) > *(s2+j)){
      j++;
      continue;
    }
    if (j==0)
        ix[i]=0;
    else{
        ix[i] = j-1; 
        j--;
    }
      i++;
  }
  if (i<n1 && j==n2){
    for (;i<n1;i++)
      ix[i] = n2-1;
  }
  return 0;
}

