
effect_prof<-function(x,nsim,...){
  df<-reg_effects(x)
  df2<-data.frame(var=rep(row.names(df),each=nsim),coef=rep(NA,nsim*nrow(df)),val=rep(NA,nsim*nrow(df)))
  for(i in 1:nrow(df)){
    df2$coef[df2$var==row.names(df[i,])]<-runif(nsim,df[i,]$lower,df[i,]$upper)
    df2$val[df2$var==row.names(df[i,])]<-rnorm(nsim,df[i,]$mean,df[i,]$sd)
  }
  if(is.null(x$family)==TRUE){
    df2$est<-df2$coef*df2$val
  }
  # Put below this line in control block above
  else if(is.null(x$family)==FALSE){
    if(x$family[2]=="probit"){
      df2$est<-pnorm(df2$coef*df2$val)
      df2$est<-df2$est*(sign(df2$coef*df2$val))
      
    }  else if(x$family[2]=="logit"){
      df2$est<-plogis(df2$coef*df2$val)*(1-plogis(df2$coef*df2$val))
      df2$est<-df2$est*(sign(df2$coef*df2$val))
    }
    
  }
  return(df2) 
}