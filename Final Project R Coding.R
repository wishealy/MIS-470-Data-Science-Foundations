#convert F to C
toCel<-function(x){
  tempc<-((x-32)*(5/9))
  return(tempc)
}

#Convert F to C
degC<-function(x){
  DegreesC <- round((5(x-32)/9),digits = 1)
  result <- paste(DegreesC,"C","")
  return(result)
}

#Computes Sum of Squares of 2 numbers (works)
sumSq<-function(y1,y2){
  sumofSquares<-(y1-y2)^2
  percent<-round(sumofSquares)
  return(percent)
}
sumSq(2,5)

#summary of univariate dataset
uniVarSum<-function(x){
  mean<-mean(x)
  min<-min(x)
  max<-max(x)
}
