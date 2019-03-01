#The following function cleans an rtPCRlist type object. It does so by calling a veriety of different
#cleaning functions that select what should be in the final list
#source('~/Applications/R_scripts/rtPCR/rtPCRlist_functions/rtPCR_clean_functions.R')
#'@export
clean.rtPCR<-function(X,use.method="robust",...){
  #we now call the cleaning functions with different methods
  if(class(X)[1]!="rtPCRlist"){stop("Can only use clean.rtPCR on an object of type rtPCRlist")}
  #The cleaning is done within each group.
  Groups<-levels(X@groups$group)
  NG<-length(Groups)
  if((NG<1)){
    stop("The rtPCR list given has not even one group")

  }else if(NG==1){
    #First thing is to remove failed PCR
    C<-X[names(which(X["Success"]))]
    #We now call onto a cleaning method (specified with the option use.method)
    #to return the list of wells that needs to be kept
    if(use.method=="normal"){
      keep<-(clean.rtPCR.normal(C,...))

    }else if(use.method=="robust"){
      keep<-(clean.rtPCR.robust(C,...))

    }else{
      warning("The method specified is not a valid method to clean, only failed PCR will be removed")
      keep<-C["Wells"]
    }

    #If the list of wells to be kept is equal to one, we need to return an object of type
    #rtPCRlist even then. And the [ function does not do that, so we need to do the following ugly
    #mess to take care of the exception

    if(length(keep)>1){
      return(X[keep])

    }else if(length(keep)==1){
      List<-list()
      List[keep]<-X[keep]
      return( rtPCRlist(Data=List,data.frame(well=keep,group=Groups[1])) )
    }

    #If there is more than on group, we clean within each group and join the results
  }else if(NG > 1){
    return(join.rtPCR(
      (sapply(X=Groups,simplify=FALSE,
              FUN=function(G){clean.rtPCR(X[G],use.method=use.method,...)}))
    ))


  }else{
    #If the number of groups is either 0 or lower, then something went very wrong
    stop("something went terribly wrong")
  }
}
