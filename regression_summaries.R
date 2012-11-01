library(effects)

coef(mymod)

plot(effect("xtest",mymod3,transformation=list(link='probit',inverse='mean')))


extract_ci<-function(x,...){
  a<-confint.default(mymod,...)
  a<-as.data.frame(a)
  names(a)<-c('lower','upper')
  return(a)
  
}

gen_names<-function(x,pattern=NULL,...){
  if(is.null(pattern)){
    mynames<-names(x$coefficients)
  }
  else if(!is.null(pattern)){
  mynames<-names(x$coefficients)[-grep(pattern,names(x$coefficients))]

}
  return(mynames)
}

gen_names(mymod,pattern="log")


my_summary<-function(x,...){
  s<-list(mean(x),
          sd(x),
          min(x),
          as.numeric(quantile(x,.25)),
          as.numeric(quantile(x,.5)),
          as.numeric(quantile(x,.75)),
          max(x))
  names(s)<-c("mean","sd","Min","1Q","Median","3Q","Max")
  unlist(s)
}

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

reg_effects(mymod,pattern="factor")

reg_effects(mymod)

gen_names(mymod,"factor")

###################################
library(ggplot2)

# Check control statements below

###############
# Error, this produces some stupid results, nothing is negative
# Need to check out how to calculate the logistic regression effects
###############

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

df2<-effect_prof(mymod,5000)

# Wrap plotting functions in convenience function

qplot(var,est,geom='boxplot',data=df2)+theme_dpi()
qplot(est,data=df2)+facet_wrap(~var)+theme_dpi()
qplot(est,y=..scaled..,data=df2,geom='density',log="est")+facet_wrap(~var)+theme_dpi()+
  coord_cartesian(xlim=c(-1,1),ylim=c(0,1))
  

######################
# Alternate for marginal effects
######################
# Sample from the model data frame and create "exemplars"
# Then sim values from there

df<-reg_effects(mymod)

df2<-mymod$model[sample(row.names(mymod$model),100),]
df2$case<-seq(1:nrow(df2))
# Expand the dataframe so we can sub in new values
df2<-df2[rep(seq_len(nrow(df2)),each=200),]

# Need to expand but manipulate value of one variable from the distribution in df
for (i in 1:max(df2$case)){
  df2$xtest[df2$case==i]<-seq(df[18,]$'2Q',df[18,]$'3Q',length.out=200)
}

for (i in 1:max(df2$case)){
  df2$ztest[df2$case==i]<-seq(df[2,]$min,df[2,]$max,length.out=200)
}

for (i in 1:max(df2$case)){
  df2$x2test[df2$case==i]<-seq(df[3,]$mean-2*df[3,]$sd,df[3,]$mean+2*df[3,]$sd,length.out=200)
}



n<-gsub("factor\\(","",names(df2))
n<-gsub("\\)","",n)
names(df2)<-n

df2$yhat<-predict(mymod,newdata=df2,type="response")


#qplot(xtest,yhat,data=df2,geom='line')+facet_wrap(~case)+theme_dpi()
qplot(ztest,yhat,data=df2,geom='line',group=case,alpha=I(0.5))+theme_dpi()
qplot(x2test,yhat,data=df2,geom='line',group=case,alpha=I(0.5))+theme_dpi()


#

a<-extract_ci(mymod,parm=gen_names(mymod,"factor"))
a<-extract_ci(mymod)
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
# For dummies we just need a 1
c[is.na(c)]<-1
# Get rid of intercept
df<-c[2:nrow(c),]
df<-df[order(abs(df$lower)),]
return(df)


b<-t(ddply(x$data,.(),colwise(mean),na.rm=TRUE))
b<-as.data.frame(b)
names(b)<-"mean"
# Combine only numerics
c<-cbind(a,b[,1][match(rownames(a),rownames(b))])
names(c)<-c("lower","upper","mean")
c$mean<-destring(c$mean)
# Squares and logs needs to get the proper value
v<-grep("\\^",rownames(c))
v2<-grep("log\\(",rownames(c))
v<-append(v,v2)
n<-gsub("\\^2)","",rownames(c)[v])
n<-gsub("[A-Z]\\(","",n)
c$mean[v]<-c$mean[row.names(c) %in% n]
# For dummies we just need a 1
c$mean[is.na(c$mean)]<-1
# Get rid of intercept
df<-c[2:nrow(c),]
df<-df[order(abs(df$lower)),]
return(df)




a<-reg_effects_df(mymod)#,parm=mynames)

autoplot(mymod)


df<- ddply(.data = dwNominate,
            .variables = .(cong, majorParty),
            .fun = summarise,  # Allows the following:
            Median = wtd.quantile(dwnom1, 1/bootse1, 1/2),
            q25 = wtd.quantile(dwnom1, 1/bootse1, 1/4),
                            q75 = wtd.quantile(dwnom1, 1/bootse1, 3/4),
                            q05 = wtd.quantile(dwnom1, 1/bootse1, 1/20),
                            q95 = wtd.quantile(dwnom1, 1/bootse1, 19/20),
                            N = length(dwnom1),
                            .progress = "text")  # Because we can.



reg_effects_df<-function(x,...){
  a<-confint.default(x,...)
  a<-as.data.frame(a)
  names(a)<-c("lower","upper")
  b<-t(ddply(x$data,.(),colwise(mean),na.rm=TRUE))
  b<-as.data.frame(b)
  names(b)<-"mean"
  # Combine only numerics
  c<-cbind(a,b[,1][match(rownames(a),rownames(b))])
  names(c)<-c("lower","upper","mean")
  c$mean<-destring(c$mean)
  # Squares and logs needs to get the proper value
  v<-grep("\\^",rownames(c))
  v2<-grep("log\\(",rownames(c))
  v<-append(v,v2)
  n<-gsub("\\^2)","",rownames(c)[v])
  n<-gsub("[A-Z]\\(","",n)
  c$mean[v]<-c$mean[row.names(c) %in% n]
  # For dummies we just need a 1
  c$mean[is.na(c$mean)]<-1
  # Get rid of intercept
  df<-c[2:nrow(c),]
  df<-df[order(abs(df$lower)),]
  return(df)
}


autoplot_sig<-function(x,...){
  df<-reg_effects_df(x,...)
  ggplot(df,aes(x=row.names(df),ymin=lower,ymax=upper))+geom_linerange()+
    theme_dpi()+theme(axis.text.x=element_text(angle=40,size=8))
  #if(class(x)[1]=="glm"){
  #  print(p+labs(x="var",y="confidence interval",title=
   #                "Confidence Interval Plot of GLM Coefficients"))
  #}
  #else{
   # print(p+labs(x="var",y="confidence interval",title=
   #                "Confidence Interval Plot of LM Coefficients"))
  #}
  #print(p)
}
# mynames<-names(grad_sq_int_sch2$coefficients)[-grep("schg8*",names(grad_sq_int_sch2$coefficients))]
# 
mynames<-names(mymod$coefficients)[-grep("x3test",names(mymod$coefficients))]

df<-reg_effects_df(mymod,pattern=mynames)
autoplot_sig(mymod,parm=mynames)
autoplot_sig(mymod)


###################
# Gen data
#############
letters_case<-paste(expand.grid(letters,LETTERS)$Var1,expand.grid(letters,LETTERS)$Var2,sep="")
probs<-rpois(676,2)
probs<-(probs-min(probs))/(max(probs)-min(probs))


intercept <- 0
beta <- 7
xtest <- rnorm(5000,1,1)
beta2 <- .2
beta3<- .9
x2test<- rpois(5000,10)
ztest<- rnorm(5000,40,3.24)
beta4<- -1.2
x3test<- sample(letters[1:5],5000,replace=TRUE)
x4test<- sample(letters[6:8],5000,replace=TRUE)
#x3test[x3test=='d' & x4test=='f']<-'b'
#x5test<- sample(c(letters[9:25],LETTERS[1:20],month.abb[1:12]),5000,replace=TRUE)
#x6test<- sample(letters_case[1:676],5000,replace=TRUE,prob=probs)
#x5test[x6test %in% sample(letters_case[1:676],60)]<-sample(letters[9:25],1)
linpred <- intercept + (xtest * beta) +(xtest^2 * beta3) +(x2test*beta2)+ 
  (log(ztest)*beta4)+
  ifelse(x3test=="c",-1.2,0.1) + 
  ifelse(x4test=="h",0.25,0.1)
prob <- exp(linpred)/(1 + exp(linpred))
runis <- runif(5000,0,1)
ytest <- ifelse(runis < prob,1,0)
#ytest[x6test %in% sample(letters_case[1:676],60)]<-1
summary(as.factor(ytest))

mydat<-data.frame(xtest=xtest,x2test=x2test,x3test=x3test,x4test=x4test,ztest=ztest,
                  ytest=ytest)

myf<-ytest~xtest+x2test+I(xtest^2)+ztest+log(ztest)+factor(x3test)+factor(x4test)+
  factor(x3test)*factor(x4test)

mymod<-glm(myf,mydat,family=binomial(link="logit"))
summary(mymod)
mymod2<-glm(myf,mydat,family=binomial(link="probit"))

f3<-factor(x3test)
mymod3<-glm(ytest~xtest+f3,mydat,family=binomial(link="probit"))


rarecase_f(mymod,.8)

classcorrect_plot(mymod,40)
confuse_mat(mymod,.4,prop=FALSE)


df<-mymod$model
df$yhat<-fitted(mymod)

plotdf<-subset(df,yhat<.4 | ytest==0)

mymod2<-glm(ytest ~ xtest + x2test + I(xtest^2) + ztest + log(ztest) + factor(x3test) + 
              factor(x4test) + yhat + factor(x3test):factor(x4test),data=plotdf,
            family=binomial(link="probit"))

confuse_mat(mymod,.4,prop=FALSE)


