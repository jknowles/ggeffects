
extract_ci<-function(x,...){
  a<-confint.default(x,...)
  a<-as.data.frame(a)
  names(a)<-c('lower','upper')
  return(a)
  
}