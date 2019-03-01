#The following function creates a histogramm of an rtPCR list
#Without parameters, 3 histogramms are created. One for efficiency of rtPCR reaction,
#one for the Baseline (ie, the noise) and a third with the intercept of the robust linear
#regression computed for each of the rtPCR elements in the list.

#If groups are specified, the histogramms are computed by groups separetely
#'@export
setMethod(f="hist",signature="rtPCRlist",definition=function(x,...,Cols=0,bins=15){
  Args<-list(...)
  L<-length(Args)
  if(L == 0){
    Args<-list("Efficiency","Intercept","Baseline")
    L<-3
    #     Eff<-ggplot(data=Data,aes(x=Efficiency,fill=group))+geom_histogram(position="identity", alpha=0.5)
    #     Int<-ggplot(data=Data,aes(x=Intercept,fill=group))+geom_histogram(position="identity", alpha=0.5)
    #     Base<-ggplot(data=Data,aes(x=Baseline,fill=group))+geom_histogram(position="identity", alpha=0.5)
    #     multiplot(Eff,Int,Base,cols=2)
    #     return
  }
  if(Cols<1) Cols<-floor(sqrt(L))
  plotList<-(sapply(X = Args,FUN = function(a){
    ggplot(data=data.frame(H=x[a],x["groups"]),aes(x=H,fill=group))+
      geom_histogram(position="dodge", alpha=0.5, bins = bins ) +xlab(a)+theme_bw()
  },simplify=FALSE))
  return(multiplot(plotlist = plotList,cols=Cols))

})
