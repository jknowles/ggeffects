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

df<-mymod$model
df$yhat<-fitted(mymod)

plotdf<-subset(df,yhat<.4 | ytest==0)

mymod2<-glm(ytest ~ xtest + x2test + I(xtest^2) + ztest + log(ztest) + factor(x3test) + 
              factor(x4test) + yhat + factor(x3test):factor(x4test),data=plotdf,
            family=binomial(link="probit"))

