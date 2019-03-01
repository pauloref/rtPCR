#The current piece of script contains all the ossible functions that can be used to clean up
#an rtPCRlist containing only one group.
#They should only be called though the function rtPCR.clean and in no way should be called dirrectly
#each function returns an array of wells that should be kept in the final list.

#First the normal method, that relies on clearing all Efficiency and Intercept further than 2 STD from the mean
clean.rtPCR.normal<-function(X,tollerance=3){

  keepE<-which( (X["Eff"]- mean(X["Eff"]))^2 < tollerance*var(X["Eff"]) )
  keepI<-which( (X["Intercept"]- mean(X["Intercept"]))^2 < tollerance*var(X["Intercept"]) )
  keep<-names(keepI)[((keepI %in% keepE))]
  return(keep)
}

clean.rtPCR.robust<-function(X,tollerance=3){
 # print(X["Groups"])
 # print((X["success"]))
#  print("Where")
#  print(median(X["Eff"]))
#  print("is the issue")
#  print(tollerance)
  keepE<-which( (X["Eff"]- median(X["Eff"]))^2< tollerance*var(X["Eff"]) )
  keepI<-which( (X["Intercept"]- median(X["Intercept"]))^2<tollerance*var(X["Intercept"]) )
  keep<-names(keepI)[((keepI %in% keepE))]
return(keep)
}
