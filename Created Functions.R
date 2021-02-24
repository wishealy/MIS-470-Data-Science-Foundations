##1)convert F to C (works)
toCel<-function(x){
  tempc<-((x-32)*(5/9))
  return(tempc)
}
toCel(45)
toCel(58)
toCel(89)
toCel(65)
toCel(32)

##2)Computes Sum of Squares of 2 numbers (works)
sumSq<-function(y1,y2){
  sumofSquares<-(y1-y2)^2
  percent<-round(sumofSquares)
  return(percent)
}
sumSq(10,43)
sumSq(2,4)
sumSq(10,13)
sumSq(15,15)
sumSq(123,198)

##3)summary of univariate dataset (works)
uniVarSum<-function(x){
  mean<-mean(x)
  min<-min(x)
  max<-max(x)
  sd<- sd(x)
  return(c(mean,min,max,sd))
}
uniVarSum(cars$mpg)
uniVarSum(cars$hp)
uniVarSum(cars$wt)
uniVarSum(cars$gear)

##4)create box plot and histogram of sqrt transformed univariate data (works)
boxhist<-function(x){
    box<-boxplot(x,main="Univariate Boxplot")
    trans<-transform(x,method='sqrt')
    asnum<-as.numeric(unlist(trans))
    thist<-hist(asnum,main='Univariate Histogram',xlab="variable")
    return(list("box"=box,"thist"=thist))
}
boxhist(cars$mpg)
boxhist(cars$wt)
boxhist(cars$hp)
boxhist(cars$am)

##5)This fuction will take a variable and booleen variable to
#create a logistic model and plot booleen values in hist chart (works)
loghist<-function(x,bvariable){
   model<-glm(formula=bvariable ~ x, family=binomial)
   min_range<-min(x)
   max_range<-max(x)
   xpoints<-seq((min_range-1),(max_range+1),log10((max_range-min_range)))
   ypoints<-predict(model,list(x=xpoints),type='response')
   plot<-plot(x,bvariable,pch=16,xlab='x',ylab='Booleen Variable')
   lines<-lines(xpoints,ypoints)
   asnum1<-as.numeric(unlist(bvariable))
   hist2<-hist(asnum1)
   return(list("hist"=hist2,"logchart"=(c(plot,lines))))
}
loghist(cars$mpg,cars$vs)
loghist(cars$hp,cars$vs)
loghist(cars$wt,cars$am)
loghist(cars$mpg,cars$am)
Sys.Date()
