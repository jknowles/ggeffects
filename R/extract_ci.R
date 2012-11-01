
extract_ci<-function(x,...){
  a<-confint.default(mymod,...)
  a<-as.data.frame(a)
  names(a)<-c('lower','upper')
  return(a)
  
}