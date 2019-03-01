#The following function takes as parameter an object of type rtPCRlist, and calculates the Ct
#values for each well contained. Te return is a named array.
#'@export
CtValues<-function(X,threshold=0,reference=0,Delta=TRUE,...){
  if(class(X)[1]!="rtPCRlist"){stop("Can only use clean.rtPCR on an object of type rtPCRlist")}

  if(threshold==0){#If the threshold is not defined, and left as zero,
    #We use as threshold the largest baseline value
    Th<-max(X["Baseline"])
  }else{
    Th<-threshold #Else we take the defined threshold
  }

  if(reference ==0){ #If no well is given as reference, then the first well of the list is used
    R<-X["Wells"][1]
  }else{
    R<-reference
  }

  CtVal<-sapply(X["Wells"], function(a){ #We now calculate the Ct Value for each well
    Sig<-X[a]["Signal"]["Signal"] #Take the signal of the well
    Up<-as.numeric(min(which(Sig>Th),na.rm = TRUE)) #Take the first value above threshold
    if(Up<2){
      return(NA)
    }

    #We now use a linear interpolation between the point above threshold and below
    #and use the interpolating line to calculate the exact Ct Value, the interpolating
    #line is y=ax+b
    a<-Sig[Up,1]-Sig[Up-1,1] #value of a
    b<-Sig[Up,1]-a*Up #Value of b
    return( (Th- b)/a )
  })
  if(Delta){
    return(CtVal-CtVal[R])
  }else{
    return(CtVal)
  }
}
