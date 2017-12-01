
trapezeIntegral<-function(x,y)



{
 sum=0.0
 for(i in 1:(length(x)-1))
 {
  sum=sum+0.5*(x[i+1]-x[i])*(y[i+1]+y[i])
 }
 sum
}

percent.string<-function(x)
{
 paste(round(x*100,2),"%",sep="")
}

gini.curve<-function(data.set,target.variable.index,scores.index,m,probing.argument,make.plot=FALSE,dataset.name="")
{
 n<-dim(data.set)[1]
 cutoffs<-seq(0,1,length=m)

 sensitivity.vector=vector(mode = "numeric", length = m)
 specificity.vector=vector(mode = "numeric", length = m)
 for(i in 1:m)
      {


                temp.scores<-as.integer(data.set[,scores.index]>=cutoffs[i]) 
                cm<-table(temp.scores,data.set[,target.variable.index] )
                if (dim(cm)[1]==1 && unique(temp.scores)==1) 
                  {sensitivity.vector[i]=1}
                else if (dim(cm)[1]==1 && unique(temp.scores)==0) 
                  {specificity.vector[i]=1}                 
                else
                {
                sensitivity.vector[i]=cm[2,2]/sum(cm[,2])  #poprawnosc klasyfikacji dobrych
                specificity.vector[i]=cm[1,1]/sum(cm[,1])  #poprawnosc klasyfikacji zlych
                }
        }

 gini.coefficient<-(trapezeIntegral(specificity.vector,sensitivity.vector)-0.5)*2
 probing.value<-sensitivity.vector[which(specificity.vector>=probing.argument)[1]]

 if(make.plot==TRUE)
 {
  windows()
  plot(specificity.vector,sensitivity.vector,type="l",lty="solid",lwd=2,xlab="specificity",ylab="sensitivity",main=paste(dataset.name," model assesment: Gini curve"))
  lines(c(0,1),c(1,0),type="l",lty="longdash",lwd=1)
  legend(0,0.4,legend=c("Gini curve","Random model"),lty=c("solid","longdash"),lwd=c(2,1))
  legend(0,0.2,legend=c("FIT PARAMETERS",paste("Gini coeff.:",percent.string(gini.coefficient)),paste("Gini",percent.string(probing.argument),"value:",percent.string(probing.value))))
 }

 list(specificity.vector=specificity.vector,sensitivity.vector=sensitivity.vector,gini.coefficient=gini.coefficient,probing.argument=probing.argument,probing.value=probing.value)
}

lift<-function(data.set,target.variable.index,scores.index,m,probing.argument,make.plot=FALSE,dataset.name="")
{
 sorted.data.set<-data.set[order(data.set[,scores.index],decreasing=TRUE),]
 n<-dim(sorted.data.set)[1]
 groupSeparators=ceiling(seq(1,n,length=m))
 lift.vector=vector(mode = "numeric", length = m)
 population.quantile.vector=(1:m)/m

 current.goods.number=0
 #liftprobing.value<-(current.goods.number/ceiling((1-thresholdParameter)*n))/goods.density

 goods.density=sum(sorted.data.set[,target.variable.index]==1)/n

 for(i in 1:(m-1))
 {
  current.goods.number=sum(sorted.data.set[groupSeparators[i]:groupSeparators[i+1],target.variable.index]==1)+current.goods.number
  lift.vector[i]=(current.goods.number/groupSeparators[i+1])/goods.density
 }
 lift.vector[m]=1
 lift.coefficient<-trapezeIntegral(population.quantile.vector,lift.vector)
 probing.value<-(sum(sorted.data.set[1:ceiling(probing.argument*n),target.variable.index]==1)/ceiling(probing.argument*n))/goods.density

 if(make.plot==TRUE)
 {
  windows()
  plot(population.quantile.vector,lift.vector,type="l",lty="solid",lwd=2,xlab="population quantile",ylab="lift",main=paste(dataset.name,"model assesment: Lift"),xlim=c(0,1),ylim=c(0,max(lift.vector)))
  lines(c(0,1),c(1,1),type="l",lty="longdash",lwd=1)
  legend(0.68,max(lift.vector),legend=c("Lift","Random model"),lty=c("solid","longdash"),lwd=c(2,1))
  legend(0.68,max(lift.vector)*0.8,legend=c("FIT PARAMETERS",paste("Lift coeff.:",round(lift.coefficient,2)),paste("Lift",percent.string(probing.argument),"value:",round(probing.value,2))))
 }

 list(population.quantile.vector=population.quantile.vector,lift.vector=lift.vector,lift.coefficient=lift.coefficient,probing.argument=probing.argument,probing.value=probing.value)
}

distribution.comparison<-function(x,y,m,make.density.plot=FALSE,make.cumulative.plot=FALSE,dataset.name="",x.label="x",y.label="y",legend.side="left")
{
 a=0
 b=1
# a=min(min(x),min(y))
# b=max(max(x),max(y))
 cutoffs<-seq(a,b,length=m)
 x.cardinality<-vector(mode = "numeric", length = m)
 y.cardinality<-vector(mode = "numeric", length = m)
 x.cumulative.cardinality<-vector(mode = "numeric", length = m)
 y.cumulative.cardinality<-vector(mode = "numeric", length = m)
 i=1
 x.cardinality[i]=sum(x<=cutoffs[i])
 y.cardinality[i]=sum(y<=cutoffs[i])
 x.cumulative.cardinality[i]=x.cardinality[i]
 y.cumulative.cardinality[i]=y.cardinality[i]
 for(i in 2:m)
 {
  x.cardinality[i]=sum(x>cutoffs[i-1] & x<=cutoffs[i])
  y.cardinality[i]=sum(y>cutoffs[i-1] & y<=cutoffs[i])
  x.cumulative.cardinality[i]=x.cardinality[i]+x.cumulative.cardinality[i-1]
  y.cumulative.cardinality[i]=y.cardinality[i]+y.cumulative.cardinality[i-1]
 }

 x.pdf=x.cardinality/length(x)
 y.pdf=y.cardinality/length(y)

 x.cdf=x.cumulative.cardinality/length(x)
 y.cdf=y.cumulative.cardinality/length(y)

 x.mean=mean(x)
 x.median=median(x)

 y.mean=mean(y)
 y.median=median(y)

 ks.vector=abs(y.cdf-x.cdf)
 ks.value=max(ks.vector)
 ks.argument=cutoffs[which(ks.vector==ks.value)[1]]

 if(make.density.plot==TRUE)
 {
  #density plot
  windows()
  plot(cutoffs,x.pdf,type="l", col = "red", lty="longdash",lwd=2,xlim=c(a,b),ylim=c(0,max(max(x.pdf),max(y.pdf))),xlab="score",ylab="probability density",main=paste(dataset.name,"model assesment: Goods/Bads PDF"))
  lines(cutoffs,y.pdf,type="l", col = "blue",lty="dotdash",lwd=2)
  if(legend.side=="left")
  {
   left.par.orientation=a
   left.l.orientation=b-(b-a)*0.2
  }
  else
  {
   left.par.orientation=b-(b-a)*0.38
   left.l.orientation=a
  }
  legend(left.par.orientation,max(max(x.pdf),max(y.pdf)),legend=c("SCORE PARAMETERS",paste(x.label,"mean:",percent.string(x.mean)),paste(x.label,"median:",percent.string(x.median)),paste(y.label,"mean:",percent.string(y.mean)),paste(y.label,"median:",percent.string(y.median))))
  legend(left.l.orientation,max(max(x.pdf),max(y.pdf)),legend=c(x.label,y.label),lty=c("longdash","dotdash"),lwd=c(2,2))
 }
 if(make.cumulative.plot==TRUE)
 {
  #distribution plot
  windows()
  plot(cutoffs,x.cdf,type="l", col = "red",lty="longdash",lwd=2,main=paste(dataset.name,"model assesment: Goods/Bads CDF"),xlab="score",ylab="probability")
  lines(cutoffs,y.cdf,type="l", col = "blue",lty="dotdash",lwd=2)
  lines(cutoffs,ks.vector,type="l",lty="solid",lwd=2)


  legend(a,max(max(x.cdf),max(y.cdf)),legend=c("SEPARATION PARAMETERS",paste("K-S argument:",percent.string(ks.argument)),paste("K-S value:",percent.string(ks.value))))
  legend(a,max(max(x.cdf),max(y.cdf))*0.8,legend=c(x.label,y.label,"K-S"),lty=c("longdash","dotdash","solid"),lwd=c(2,2,2))
 }
 list(score=cutoffs,x.pdf=x.pdf,y.pdf=y.pdf,x.mean=x.mean,y.mean=y.mean,x.median=x.median,y.median=y.median,x.cdf=x.cdf,y.cdf=y.cdf,ks.argument=ks.argument,ks.value=ks.value)
}

score.probability<-function(data.set,target.variable.index,scores.index,m,make.density.plot=FALSE,make.cumulative.plot=FALSE)
{
 a=0
 b=1
 cutoffs<-seq(a,b,length=m)
 goods.probability.vector<-vector(mode="numeric",length=m)
 goods.probability.cumulative.vector<-vector(mode="numeric",length=m)
 score.frequency.vector<-vector(mode="numeric",length=m)
 score.frequency.cumulative.vector<-vector(mode="numeric",length=m)
 goods.number=sum(data.set[,target.variable.index]==1)
 i=1
 score.frequency.vector[i]=sum(data.set[,scores.index]<cutoffs[i])
 score.frequency.cumulative.vector[i]=sum(data.set[,scores.index]>=cutoffs[i])
 goods.probability.vector[i]=sum(data.set[which(data.set[,scores.index]<cutoffs[i]),target.variable.index]==1)/goods.number
 goods.probability.cumulative.vector[i]=goods.probability.vector[i]
 for(i in 2:m)
 {
  score.frequency.vector[i]=sum(data.set[,scores.index]>=cutoffs[i-1] & data.set[,scores.index]<cutoffs[i])
  score.frequency.cumulative.vector[i]=sum(data.set[,scores.index]>=cutoffs[i])
  goods.probability.vector[i]=sum(data.set[which(data.set[,scores.index]>=cutoffs[i-1] & data.set[,scores.index]<cutoffs[i]),target.variable.index]==1)/goods.number
  goods.probability.cumulative.vector[i]=goods.probability.vector[i]+goods.probability.cumulative.vector[i-1]
 }

 if(make.density.plot==TRUE)
 {
  windows()
  main.scale.vector<-round(seq(0,max(goods.probability.vector),length=6),3)
  scaled.data<-scale.axis(goods.probability.vector,main.scale.vector,score.frequency.vector)
  plot(cutoffs,goods.probability.vector,type="l",lwd=2,lty="solid",axes=FALSE,xlab="score",ylab="probability")
  lines(cutoffs,scaled.data$scaled.y2,type="l",lwd=2,lty="longdash")


  axis(1)
  axis(2,at=main.scale.vector,main.scale.vector)
  axis(4,at=main.scale.vector,round(scaled.data$y2.labels,0))

  legend(a,max(goods.probability.vector),legend=c("probability","frequency"),lty=c("solid","longdash"),lwd=c(2,2))
 }
 if(make.cumulative.plot==TRUE)
 {
  windows()
  main.scale.vector<-round(seq(0,1,length=6),2)
  scaled.data<-scale.axis(goods.probability.cumulative.vector,main.scale.vector,score.frequency.cumulative.vector)
  plot(cutoffs,goods.probability.cumulative.vector,type="l",lwd=2,axes=FALSE,xlab="score",ylab="probability")
  lines(cutoffs,scaled.data$scaled.y2,type="l",lwd=2,lty="longdash")

  axis(1)
  axis(2,at=main.scale.vector,labels=main.scale.vector)
  axis(4,at=main.scale.vector,round(scaled.data$y2.labels,0))

  legend(a,0.5*(main.scale.vector[length(main.scale.vector)]-main.scale.vector[1]),legend=c("probability","frequency"),lty=c("solid","longdash"),lwd=c(2,2))
 }
 list(score=cutoffs,goods.probability.vector=goods.probability.vector,goods.probability.cumulative.vector=goods.probability.cumulative.vector,frequency.vector=score.frequency.vector,frequency.cumulative.vector=score.frequency.cumulative.vector)
}

scale.axis<-function(y1,y1.labels,y2)
{
 ay1=min(y1)
 by1=max(y1)
 ay2=min(y2)
 by2=max(y2)

 scaled.y2=(y2-ay2)/(by2-ay2)*(by1-ay1)+ay1
 y2.labels=(y1.labels-ay1)/(by1-ay1)*(by2-ay2)+ay2

 list(scaled.y2=scaled.y2,y2.labels=y2.labels)
}

income.mis<-function(data.set,income.index,cost.index,scores.index,m,commision,make.plot=FALSE,min.y=0,max.y=0)
{
 a=0
 b=1
 cutoffs<-seq(a,b,length=m)
 profit.vector<-vector(mode="numeric",length=m)
 cumulative.profit.vector<-vector(mode="numeric",length=m)


 i=1
 profit.vector[i]=sum(data.set[data.set[,scores.index]<cutoffs[i],income.index])
 for(i in 2:m)
 {
  profit.vector[i]=sum(data.set[data.set[,scores.index]<cutoffs[i] & data.set[,scores.index]>=cutoffs[i-1],income.index])*commision-sum(data.set[data.set[,scores.index]<cutoffs[i] & data.set[,scores.index]>=cutoffs[i-1],cost.index])*30
 }

 i=m
 cumulative.profit.vector[i]=profit.vector[i]
 for(i in (m-1):1)
 {
  cumulative.profit.vector[i]=profit.vector[i]+cumulative.profit.vector[i+1]
 }

 max.profit.value<-max(cumulative.profit.vector)
 max.profit.score<-cutoffs[which(cumulative.profit.vector==max.profit.value)[1]]
 min.profit.value<-min(cumulative.profit.vector)
 min.profit.score<-cutoffs[which(cumulative.profit.vector==min.profit.value)[1]]
 zero.profit.score<-cutoffs[which(cumulative.profit.vector<=0)[1]]

 if(make.plot==TRUE)
 {
  windows()
  if(min.y==0 & max.y==0)
  {
   plot(cutoffs,cumulative.profit.vector,type="l",lwd=2,lty="solid")
   lines(c(0,1),c(0,0),lty="longdash",lwd=1)
  }
  else
  {
   plot(cutoffs,cumulative.profit.vector,type="l",lwd=2,lty="solid",ylim=c(min.y,max.y))
   lines(c(0,1),c(0,0),lty="longdash",lwd=1)
  }
 }
}

#logistic.regression<-function(training.set,test.set,predicted.set,target.variable.index,explanatory.variables.indices,dir="none")
logistic.regression<-function(training.set,test.set,predicted.set,target.variable,explanatory.variables,Waga,wyraz.wolny,dir="none",family=binomial(link="logit"))
{
 variables.names<-attr(training.set,"names")
## temporary.formula<-as.formula(paste(variables.names[target.variable.index],"~",paste(variables.names[explanatory.variables.indices],collapse="+")))
# temporary.formula<-as.formula(paste(target.variable,"~",paste(explanatory.variables,collapse="+")))
# target.variable.index<-which(variables.names==target.variable)
# model<-glm(temporary.formula,data=training.set,family=binomial())
# if(dir!="none")
# {
#  model<-step(model,direction=dir)
# }

 if(dir=="backward")
 {
  temporary.formula<-as.formula(paste(target.variable,"~",paste(explanatory.variables,collapse="+")))
  target.variable.index<-which(variables.names==target.variable)
  model<-glm(temporary.formula,data=training.set,weights=Waga,family=family)
  model<-step(object=model,direction=dir,trace=1)
 }

 if(dir=="forward")
 {
  temporary.formula<-as.formula(paste(target.variable,"~",paste(explanatory.variables,collapse="+")))
  target.variable.index<-which(variables.names==target.variable)
  model<-glm(as.formula(paste(target.variable,"~",wyraz.wolny)),data=training.set,weights=Waga,family=family)
  model<-step(object=model,scope=temporary.formula,direction="forward",trace=1)
 }

  if(dir=="both")
 {
  temporary.formula<-as.formula(paste(target.variable,"~",paste(explanatory.variables,collapse="+")))
  target.variable.index<-which(variables.names==target.variable)
  model<-glm(as.formula(paste(target.variable,"~",wyraz.wolny)),data=training.set,weights=Waga,family=family)
  model<-step(object=model,scope=temporary.formula,direction="both",trace=1)
 }

 if(dir=="none")
 {
  temporary.formula<-as.formula(paste(target.variable,"~",paste(explanatory.variables,collapse="+")))
  target.variable.index<-which(variables.names==target.variable)
  model<-glm(temporary.formula,data=training.set,weights=Waga,family=family)
 }

 #print("alive")
#training set
# scores<-predict.glm(model,training.set,type="response")
# training.set.results<-data.frame(training.set,scores)
#predicted set
# scores<-predict.glm(model,predicted.set,type="response")
# predicted.set.results<-data.frame(predicted.set,scores)
#test set
                                                                                # rk -  scores<-predict.glm(model,test.set,type="response")
                                                                                # rk -   test.set.results<-data.frame(test.set,scores)
#writing results
# write.table(training.set.results,file="PrognozyTrn.csv",sep=",",row.names=F)
# write.table(test.set.results,file="PrognozyTst.csv",sep=",",row.names=F)
# write.table(predicted.set.results,file="PrognozyPred.csv",sep=",",row.names=F)
#model evaluation
                                                                                # rk -   scores.index<-which(names(test.set.results)=="scores")
                                                                                # rk -   gc<-gini.curve(test.set.results,target.variable.index,scores.index,101,0.8,TRUE,"PROV")
# l<-lift(test.set.results,target.variable.index,scores.index,101,0.2,TRUE,"PROV")
                                                                                # rk -    dc<-distribution.comparison(test.set.results[test.set.results[,target.variable.index]==1,scores.index],test.set.results[test.set.results[,target.variable.index]==0,scores.index],101,make.density.plot=TRUE,make.cumulative.plot=TRUE,dataset.name="PROV",x.label="Goods",y.label="Bads",legend.side="right")
# sp<-score.probability(test.set.results,target.variable.index,scores.index,11,make.density.plot=TRUE,make.cumulative.plot=FALSE)
# print(test.set[,24])
# income.mis(test.set.results,24,27,scores.index,101,0.3,make.plot=TRUE,min.y=-30000,max.y=30000)
 model
}

#logistic.regression(KRI.trn.dense,KRI.tst,KRI.tst,c(25),c(6,7,8,9,11,13,14,15,16,17,18,19,20,21,22,23),dir="none")

wykresy <- function(ramka,modelowana,model) 
{

scores <- predict.glm(model,ramka,type="response")
wynik <- data.frame(ramka,scores)

scores.index<-which(names(wynik)=="scores")
target.variable.index<-which(names(wynik)==modelowana)
gc<-gini.curve(wynik,target.variable.index,scores.index,101,0.8,TRUE,"PROV")

dc<-distribution.comparison(wynik[wynik[,target.variable.index]==1,scores.index],wynik[wynik[,target.variable.index]==0,scores.index],101,make.density.plot=TRUE,make.cumulative.plot=TRUE,dataset.name="PROV",x.label="Goods",y.label="Bads",legend.side="right")

}