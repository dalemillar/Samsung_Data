set.seed(1506)
lambda=0.2
mns=NULL
rowBuilder<-function(){
  #for(i in 1:40){
    mns<-c(mns,(rexp(40,lambda)))  
  #}
  mns[41]=mean(mns)
  mns[42]=mean(mns)
return (mns)
}
answer=matrix(rowBuilder(),nrow=1)
for(i in 2:1000){
  answer<-rbind(rowBuilder(),answer)
    
}
mean(answer[,41])
sd(answer[,41])
var(answer[,41])


cumMeans=vector()
cumMeans[1]=answer[1,41]
accumulator=answer[1,41]
for(i in 2:1000){
  cumMeans[i]=(answer[i,41]+accumulator)/i
  accumulator=accumulator+answer[i,41]
}

library(ggplot2)
g<-ggplot(data.frame(x=1:1000,y=cumMeans),aes(x=x,y=y))
g<- g+scale_y_continuous(limits=c(4,6))
g<-g+geom_hline(yintercept=5)+geom_line(size=1,color='red')
g<-g+labs(x='Number of Sample Means Observations',y='Cumulative Mean')
g<-g+ggtitle("Cumulative Sample Means Converge toward Theoretical Mean")

set.seed(1506)
dat=data.frame(x=rexp(1000,0.2))
h<-ggplot(dat,aes(x=x))+
  geom_histogram(binwidth=.3,colour='black',aes(y=..density..))
h<-h+ggtitle("Distribution of rexp(1000,0.2)")
h=h+stat_function(fun = dnorm,colour="red",args=list(mean=5,sd=5),size=2)
h

dat1=data.frame(x=answer[1:1000,41])
i<-ggplot(dat1,aes(x=x))+
geom_histogram(binwidth=.3,colour='black',aes(y=..density..))
i=i+ggtitle("Normal Distribution of Means")
i=i+stat_function(fun = dnorm,colour="red",args=list(mean=5,sd=0.78),size=2)
i

set.seed(1506)
hist(rexp(1000,0.2))

