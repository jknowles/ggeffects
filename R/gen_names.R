
gen_names<-function(x,pattern=NULL,...){
  if(is.null(pattern)){
    mynames<-names(x$coefficients)
  }
  else if(!is.null(pattern)){
    mynames<-names(x$coefficients)[-grep(pattern,names(x$coefficients))]
    
  }
  return(mynames)
}
