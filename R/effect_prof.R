effect_prof<-function(x,nsim,...){
  df<-reg_effects(x,...)
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

mfxboot <- function(x,boot=1000,digits=3){
  x <- x
  data<-x$data
  # get marginal effects
  pdf <- ifelse(x$family[2]=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  set.seed(1111)
  for(i in 1:boot){
    samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
    x1 <- glm(formula(x), family=binomial(link=paste(x$family[2])),samp1)
    pdf1 <- ifelse(x$family[2]=="probit",
                   mean(dnorm(predict(x, type = "link"))),
                   mean(dlogis(predict(x, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
  } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
  }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
}

#http://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/

effect_prof_mfx<-function(x,nsim,...){
  df<-reg_effects(x)
  df2<-mfxboot(x,boot=nsim)
  df2 <- data.frame(cbind(rownames(df2),df2))
  df2$me <- as.numeric(as.character(df2$marginal.effect))
  df2$se <- as.numeric(as.character(df2$standard.error))
  df2$var<-rownames(df2)
  df$var<-rownames(df)
  df2<-merge(df,df2,by="var")
  df2$V1<-NULL
  df2$marginal.effect<-NULL
  df2$standard.error<-NULL
  df2$z.ratio<-as.numeric(as.character(df2$z.ratio))
  return(df2)
}

