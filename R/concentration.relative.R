#The following function is a method that applies onto an rtPCRlist type object. 
#It's output is a matrix containing all the relative initial DNA concentration between all groups
#deffined in the list. 

concentration.relative<-function(X,...){
 return(cbind(sapply(X["GL"],USE.NAMES=TRUE,simplify=TRUE,FUN=function(a){
    summary(X,reference=a,...)["Concentration",]
    
  })) )
}