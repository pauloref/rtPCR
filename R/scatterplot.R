#The curent script defines the function scatterplot that applies to rtPCRlist objects
#The function serves to create a scatter plot of the rtPCR elements present in the list.
#One can choose 2 values of rtPCR object type and creat a scater plof of one vallues as
#a function of the other.
#Further options can be passed such as colloring them by group or adding regression lines by group
#or total
setGeneric(name = "scatterplot",def = function(x,...){
  standardGeneric("scatterplot")
})
#'@export
setMethod(f="scatterplot",signature="rtPCRlist",definition=function(x,axisX="Intercept",axisY="Efficiency",...){
  plot<-ggplot(data = data.frame(X=x[axisX],Y=x[axisY],x["Groups"]),aes(x=X,y=Y,color=group,label=x["Wells"]))+
    geom_point(size=3)+theme_bw()+xlab(axisX)+ylab(axisY)+geom_label()
  return(plot)
})
