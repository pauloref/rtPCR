#The following method applies to an object of type rtPCRlist and prints a report
#The report computes a variety of statistics. Options configuring the layout of the report can be given
#'@export
setMethod(signature="rtPCRlist",f="summary",definition=function(object,reference=0,clean=TRUE,xtables=FALSE,...){
  if(clean){
    X<-clean.rtPCR(object,...)
  }else{X<-object}

  if(!(reference %in% X["GL"]) ){
    Ref<-X["GL"][1]
  }else{
    Ref<-reference
  }

  Efficiency<-sapply(X["GL"],simplify=TRUE,USE.NAMES=TRUE,
                     FUN=function(a){mean(X["Eff",a])})

  Concentration<-(sapply(X["GL"],simplify=TRUE,USE.NAMES=TRUE,
                         FUN=function(a){2^(mean(X["Intercept",a])-mean(X["Intercept",Ref]))
                         }))
  CI<-sapply(X["GL"],simplify=TRUE,USE.NAMES=TRUE,
             FUN=function(a){
               na<-length(X["Intercept",a])
               nref<-length(X["Intercept",Ref])
               n<-na+nref
               2^(sqrt( (var(X["Intercept",a])/na+var(X["Intercept",Ref])/nref )/2)*qt(p = 0.975,df = (n-2)) )
             }
             )
  CI[Ref]<-1

  report<-data.frame("Efficiency"=Efficiency,"Concentration"=Concentration,
                     "Confidence Ratio"=CI)


  return(report)
})
