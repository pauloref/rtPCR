#The current function is used to append elements to an rtPCRlist type object
#The first argument is an rtPCRlist, what follows can be a mixture of rtPCR objects, rtPCRlist, or a list
#of any of the above
#The output will be an rtPCRlist object containing everithing
#'@export
join.rtPCR<-function(x,...){
  if(is.list(x) & length(x) >1){
    Args<-c(x[2:length(x)],list(...))
    x<-x[[1]]
  }else{
    Args<-list(...)
  }

  if(length(Args)<1){
    stop("You must give at least 2 arguments to join")

  }else if(length(Args)==1){
    Y<-Args[[1]]
    if(class(Y)[1]=="rtPCRlist"){
      return(new("rtPCRlist",List=c(x@List,Y@List),
                 groups=rbind(x["groups"],Y["groups"]) ))

    }else if(class(Y)[1]=="rtPCR"){
      return(new("rtPCRlist",List=c(x@List,Y),
                 groups=rbind(x@groups,c(names(Y),names(Y)) )))

    }else if( is.list(Y)){
      if(length(Y)==1){
        return(join.rtPCR(x,Y[[1]]))
      }else if(length(Y)>1){
        return(join.rtPCR(join.rtPCR(x,Y[[1]]),Y[2:length(Y)]))

      }else if(is.null(Y) | is.na(Y) ){
        return(x)
      }

    }else{
      stop(c("You can only append an object of type rtPCR or rtPCRlist to an rtPCRlist,
or a list of them \n you gave a type: "),
           class(Y[[1]]) )
    }
  }else if(length(Args)>1){

    return(join.rtPCR(join.rtPCR(x,Args[[1]]),Args[2:length(Args)]))
  }

}
