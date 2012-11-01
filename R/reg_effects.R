
reg_effects<-function(x,...){
  a<-extract_ci(x,parm=gen_names(x,...))
  b<-t(ddply(mymod$data,.(),numcolwise(my_summary),na.rm=TRUE))
  b<-as.data.frame(b,stringsAsFactors=FALSE)
  names(b)<-c("mean","sd","min","2Q","median","3Q","max")
  b<-b[2:nrow(b),]
  b[,1:ncol(b)]<-sapply(b[,1:ncol(b)],as.numeric)
  c<-cbind(a,b[match(rownames(a),rownames(b)),])
  # Squares and logs needs to get the proper value
  v<-grep("\\^",rownames(c))
  v2<-grep("log\\(",rownames(c))
  v<-append(v,v2)
  n<-gsub("\\^2)","",rownames(c)[v])
  n<-gsub("[A-Z]\\(","",n)
  n<-gsub("log\\(","",n)
  n<-gsub("\\)","",n)
  c$mean[v]<-c$mean[row.names(c) %in% n]
  c$sd[v]<-c$sd[row.names(c) %in% n]
  c$min[v]<-c$min[row.names(c) %in% n]
  c$max[v]<-c$max[row.names(c) %in% n]
  c$median[v]<-c$median[row.names(c) %in% n]
  c$'2Q'[v]<-c$'2Q'[row.names(c) %in% n]
  c$'3Q'[v]<-c$'3Q'[row.names(c) %in% n]
  # For dummies we just need a 1, excepf for the SD
  c$sd[is.na(c$sd)]<-0
  c[is.na(c)]<-1
  # Get rid of intercept
  df<-c[2:nrow(c),]
  df<-df[order(abs(df$lower)),]
  return(df)
}
