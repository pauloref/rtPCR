#This script creates a function that performs a robust linear model.
#the basic structure is taken from the lm function and is then edited in order to perform robust analysis
#library("functional")
#library("plyr")
#library("reshape2")

#We now create a function, that given a vector x and a vector y, compute the robust linear regression between the two
#The input is the vector x, y and a parameter nres
#nres is the number of residuals that should be considered to compute the median squared residual
#force.passage, if true requires the point index to be used to fit the line.
#It can also take the value: middle to force the passage through the y midpoint
#The function can be asked to plot the regression as well as to save the plot in a PDF file
#The last options are self-expanatory in that sense
#The output of the function, is a list containing the following values
#coefficients : a named vector of coeficnents
#residuals: the residuals
#slope: the slope of the line
#intercept: the intercept of the line (redundant with coefficients)
#fitted.values: the fitted values
#median.res.sq
#x.values: The original x values
#y.values: The y values
#middle: The mid point used for the regression
#title: the main title of the plot if one asks the function to plot
#'@export
roblm.fit<-function(x,y,nres=0,force.passage=0,PLOT=FALSE,
                    Main=paste("Robust Linear Regression\n force middle is ",force.passage, " Number of residues considered: ", nres),
                    PDF=0, na.ommit=TRUE,keep.right.of.center=0,keep.left.of.center=0) {
  if(na.ommit){
    valid<-!is.na(y)
    x<-x[valid]
    y<-y[valid]
  }


  if(length(y)==0){break("there are no values to be fitted")
  }else if(length(x)==0|length(x)==1){(x<-1:length(y))
  }else if( (length(x)!=length(y)) )break("vectors have different lengths")
  if(!PLOT & (PDF!=0)  )break("Nothing to write to file as plot is set to FALSE")

  middle=which.min( (y-( (max(y)-min(y))/2 + min(y) ) )^2 )
  if(keep.right.of.center!=0){
    x<-x[1:(middle+keep.right.of.center)]
    y<-y[1:(middle+keep.right.of.center)]
  }
  if(keep.left.of.center!=0){
    x<-x[max(0,(middle-keep.left.of.center)):length(x)]
    y<-y[max(0,(middle-keep.left.of.center)):length(y)]
  }
  middle=which.min( (y-( (max(y)-min(y))/2 + min(y) ) )^2 )

  L<-length(y)
  if(nres > L)nres<-L
  grid<-data.frame(expand.grid(1:L,1:L))
  colnames(grid)<-c("x","y")
  grid<-grid[which(grid$x>grid$y),]

  if(force.passage %in% c("middle","mid","m","midle"))grid<-grid[which( grid$x==middle | grid$y==middle ),]
  else if((force.passage>0) && (force.passage<=length(x)) )grid<-grid[which( grid$x==force.passage | grid$y==force.passage ),]

    if(nres==0){
    MedRes<-as.matrix(apply(grid,1,function(a,b){
      median(sort( (y-( (y[a[1]]-y[a[2]])/(x[a[1]]-x[a[2]])*x+ y[a[1]]-(y[a[1]]-y[a[2]])/(x[a[1]]-x[a[2]])*x[a[1]]) )^2 ) ) }))
  } else {
    MedRes<-as.matrix(apply(grid,1,function(a,b){
      median(sort( (y-( (y[a[1]]-y[a[2]])/(x[a[1]]-x[a[2]])*x+ y[a[1]]-(y[a[1]]-y[a[2]])/(x[a[1]]-x[a[2]])*x[a[1]]) )^2 )[1:nres] ) }))
  }


  BestCombination<-as.matrix(grid[which.min(MedRes),])
  Slope=(y[BestCombination[1]]-y[BestCombination[2]])/(x[BestCombination[1]]-x[BestCombination[2]])
  Intercept=y[BestCombination[1]]-(y[BestCombination[1]]-y[BestCombination[2]])/(x[BestCombination[1]]-x[BestCombination[2]])*x[BestCombination[1]]
  Fitted.values<-x*Slope+Intercept

  fit<-list("slope"=Slope, "intercept"=Intercept, "coefficients"=list("intercept"=Intercept,"slope"=Slope),
            "fitted.values"=Fitted.values,"residuals"=y-Fitted.values, "median.res.sq"=median(sort( (y-Fitted.values)^2 )),
            "x.values"=x,"y.values"=y,"nres"=nres,"title"=Main,"middle"=middle )
  class(fit)<-"roblm"
  if(PDF!=0)pdf(PDF)
  if(PLOT){
    plot(x,y,main=Main)
    points(x,Fitted.values,type="l")
  }
  if(PDF!=0)dev.off()
  return(fit)

}

