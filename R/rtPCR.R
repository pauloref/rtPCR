#The following file contains a series of functions that can be used to analyse the output of rtPCR Data
#library("ggplot2")
#source('~/Applications/R_scripts/robustlm.R', echo=TRUE)
#source('~/Applications/R_scripts/multiplot.R')

#we here deffine the rtPCR class
#'@export
setClass("rtPCR",representation(
  #the rtPCR class contains data that are input and data that are computed from the input
  #The only thing that is contained given as input is the Signal. The signal is a data.frame
  #with the cycle number and corresponding fluorescence data. From there the rest can be conputed
  Signal = "data.frame",
  #Follows the properties that are computed from the signal, using possible options passed to the constructo method
  logSignal= "data.frame",
  baseline = "numeric",
  robLM = "roblm"
)
)
#the main initializer function. It should not be used by user, but will later be called by the
#constructors at the disposal of the user
setMethod("initialize", signature="rtPCR",
          definition=function(.Object,...,Cycle=integer(),Signal=integer()){
            #No Value should be smaller than 0 (or 1 for that matters), so to simplify
            #the log-transform, we set all values smaller than 1 to 1, so the log will map
            #the values of the signal into the positives.

            middle<- which.min((Signal-max(Signal)/2)^2)
            non_monotonous<-which(diff(Signal)<0)
            noisy<-non_monotonous[which(non_monotonous<= middle)]
            .Object@baseline <- max(Signal[noisy])

            Signal[which(Signal<1)]<-1
            signal.data=data.frame(Cycle,Signal)
            colnames(signal.data)<-c("Cycles","Signal")

            .Object@Signal <- signal.data

            #We now compute the log Signal, that will be used to compute the efficiency of the reaction
            #We make the assumption that the signal is monotonous and always increasing
            #Thus all positions with a negative derivative are part of the noise and are removed from the
            #log Signal
            log_Signal<-  data.frame(Cycle,
                                     log2(Signal))

            log_Signal<- log_Signal[which(diff(Signal) >= 0),]

            colnames(log_Signal)<-c("Cycles","logSignal")
            .Object@logSignal <- log_Signal

            #We now uase the roust regression to fit the expected slope To do that,
            #the regression must be forced to pass through the first point above threshold.
            #Passage<-as.numeric(min(which(.Object@Signal$Signal>.Object@baseline),na.rm = TRUE))+1
            Passage<-"mid"
            #Passage
            .Object@robLM <- roblm.fit(.Object@logSignal$Cycles,.Object@logSignal$logSignal,nres=8,force.passage = Passage)

            return(.Object)
          })


#The accessor function. it will return one of the properties of an rtPCR object type
#One can put into brackets the desired property
setMethod(f="[",signature="rtPCR",definition=function(x,i,j,drop){
  if(i=="Signal"){return(x@Signal)}else{}

  if(i=="logSignal" | i=="LogSignal" | i=="Log" | i== "LogS"){return(x@logSignal)}else{}

  if(i=="Cycles"){return(x@Signal$Cycles)}else{}

  if(i=="Efficiency"|i=="Eff"){return(2^x@robLM$slope)}else{}

  if(i=="Intercept"){return(x@robLM$intercept)}else{}

  if(i=="baseline"|i=="Base"|i=="Baseline"){return(x@baseline)}else{}

  if(i=="MidPoint"|i=="MP" | i == "MiddlePoint"){return(x@robLM$middle)}else{}

  if(i=="success" | i=="Success"){return(success(x))}


})

#user friendly constructors follows. A wide variety of types of input can be given
#The output is always an object of type rtPCR.
rtPCR<-rtPCR<-function(x,y=0,gaps=FALSE){
  if(!gaps){
    if(y==0){
      return(new("rtPCR",Cycle=1:length(x),Signal=x))
    }else if(all(x==1:max(x))){
      return(new("rtPCR",Cycle=x,Signal=y))
    }else if( all(y==1:max(y)) ){
      return(new("rtPCR",Cycle=y,Signal=x))
    }else{
      break("neither of the 2 parameters given could be identified as a cycle list without gaps\n
              if you have gaps in your readings, set gaps=TRUE")
    }
  }else if(gaps){
    if(y==0){break("If gaps is set as TRUE, a cycle vector matching the signal must be provided")
    }else if(length(y)==length(x)){
      return(new("rtPCR",Cycle=x,Signal=y))
    }else{break("The length of the cycle and signal vector must match ")}
  }

}

#The following method is the plot method
#It plots an rtPCR class type object
#it takes as parameter an rtPCR object and an option parameter
#the option parameter specifies if it should plot the linear version or the log version of the signal
#The log plot includes the linear regression
setMethod(f="plot",
          signature= "rtPCR",
          definition=function(x,y="all",...){
            plot<-ggplot(x@Signal,aes(x=Cycles,y=Signal)) + geom_point() + ggtitle("Signal")+theme_bw()
            plotLog<-ggplot(x@logSignal,aes(x=Cycles,y=logSignal))+
              geom_abline(intercept=x@robLM$intercept,slope=x@robLM$slope) + geom_point()+ggtitle("Log signal and linear regression")+
              theme_bw()+geom_hline(yintercept = log2(x["Baseline"]))
            if(y=="log"){
              plotLog
            }else if(y=="lin" || y=="linear"){
              plot
            }else{
              multiplot(plot,plotLog,cols=1)
            }
          })


#The following method checks wether an rtPCR reaction is failed or not. it is called success
#and returns TRUE if the reaction has succeded, and FALSE if not. To check
#for a successfull reaction, we check if the values are coherent with a successfull PCR.
setGeneric(name = "success",def = function(x,...){
  standardGeneric("success")
})

setMethod(f="success",
          signature= "rtPCR",
          definition=function(x){
            if( x["Baseline"]>= max(x["Signal"])/100 |
                x["Eff"]<=1 |
                x["Intercept"]>=0

            ) return(FALSE)
            else return(TRUE)
          })

