#The function load.rtPCRlist, reads a file and returns an object of type rtPCRlist.
#The types of supported files and how they should be structured are:


#

#library("tools", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#'@export
load.rtPCRlist<-function(File=character(),type=0,...){
 names_to_filter=c("NA.","Cycle","Cycles","cycles","cycle","NA","NULL")
  if(!file.exists(File)){stop("File does not exists")}
  if(!type){
    type<-file_ext(File)
  }

  if(type=="xlsx"){
    data<-read.xlsx(file=File,as.data.frame=TRUE,header=TRUE,...)
    wells<-names(data)[which(!(names(data) %in% names_to_filter))]

  }else{
    stop("The input file is not of a recognized type")
  }
  return(data)
}

