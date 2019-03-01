
#The current file describes the rtPCRlist class type
#It is a class that contains a list of rtPCR that can be groupped togethere

#The file contains: The class deffinition, the initializer, a constructor to be used by the user, and
#a function to access the elements inside


#We declare the class rtPCRlist
#' @import methods
#' @import ggplot2 reshape2 functional plyr tools
#' @export
setClass("rtPCRlist",representation(
  List="list",
  groups="data.frame",
  N="numeric"
))

#A simple initializer that will be used by other functions but should never be called upon
#by users
setMethod("initialize",signature="rtPCRlist",definition=function(.Object,...,List=list(),groups=data.frame()){
  .Object@List=List
  .Object@N=length(List)
  .Object@groups=groups
  return(.Object)
})


#A simple validity check
setValidity("rtPCRlist",function(object){
  if(length(object@groups)!=length(object@List)){stop("The group list is of different size as the list of rtPCR")

  }else if(!all(lapply(object@List,FUN=function(x)(class(x)[1])) =="rtPCR" )){
    stop("All the elements of the List are not of type rtPCR")
  }else{
    return(TRUE)
  }
})

#The user-friendly constructor to be used. It takes as input a data-frame containing a series of rtPCR
#Signal. The signal are arranged on one of the dimmentions. If they are the rows, then dimm=1. If they are
#on the columns then dimm=2. The default is the collumn
#'@export
rtPCRlist<-function(Data=data.frame(),Groups=data.frame(),dimm=2){
  if(is.data.frame(Data)){

    names<-colnames(Data)
    if(length(Groups)==0){groups<-data.frame(well=names,group=1:length(names),row.names=names)
    }else{
      groups<-Groups
      colnames(groups)<-c("well","group")
      rownames(groups)<-names
    }
    List<-apply(Data,MARGIN=dimm,function(x){rtPCR(x)})


  }else if(is.list(Data)){

    List<-Data
    names<-names(Data)
    if(length(Groups)==0){groups<-data.frame(well=names,group=as.factor(1:length(names)),row.names=names)
    }else{
      groups<-Groups
      colnames(groups)<-c("well","group")
      rownames(groups)<-names
      groups$group<-as.factor(groups$group)
    }
  }
  return(new(Class="rtPCRlist",List=List, groups=groups))
}


#we now define a class action acessor, that can access the different elements of the list
#'@export
setMethod(f="[",signature="rtPCRlist",definition=function(x,i,j = 0,...,drop){
  #if the fisrt argument has length 1, then we do one of the following:
  if(length(i) ==0 ){return(NULL)}
  if(missing(j)){j<-0}
  if(length(i)==1){
    if(i=="Names"| i=="Wells"){
      return(names(x@List))

    }else if(i=="Groups" | i=="groups"){
      return(x@groups)

    }else if(i=="GroupList" | i=="GL"){
      return(as.character(unique(x@groups$group)))

    }else if(i %in% x@groups$group){
      selected<-x@groups$well[which(x@groups$group == i)]
      return(new("rtPCRlist",List=x@List[selected],
                 groups=data.frame(well=selected,group=rep(i,length(selected)),row.names=selected)))

    }else if( all(i %in% x@groups$well) ){
      #return( new("rtPCRlist",Data=x@List[i],groups=x@groups[i,] ) )
      return(x@List[[i]])

    }else{
      if(j==0|is.null(j)){
        return((sapply(x@List,simplify=TRUE,function(a){a[i]})))

      }else if(j %in% x@groups$group){
        return(as.numeric(sapply(x@List,simplify=TRUE,function(a){a[i]}))[which(x@groups$group==j)])

      }else{
        return(as.numeric(sapply(x@List,simplify=TRUE,function(a){a[i]})[j]))
      }
    }
  }else if(length(i)>1){
    return(new("rtPCRlist",
               List=sapply(i,FUN=function(a){x[a]},simplify=FALSE, USE.NAMES=TRUE),
               #groups=x@groups[which(x@groups$well %in% i),]))
               groups=x@groups[ (which(x@groups$well %in% i | x@groups$group %in% i  )),]))
  }else{stop("Can't recognise the type of input")}
})


#source('~/Applications/R_scripts/rtPCR/load_functions.R', echo=TRUE)


