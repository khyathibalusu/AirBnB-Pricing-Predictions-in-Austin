library(kknn)
library(tree)

data=read.csv('listings with reviews percent change added cleaned.csv')

attach(data)

df=c('latitude','longitude','price','percent change')

attach(df)




plot(longitude, latitude, col=percent.change)
#symbols(longitude, latitude, circles=percent.change, inches=0.1)


plot(percent.change,price)

train = data.frame(price,percent.change)
test = data.frame(price,percent.change)
ind = order(test[,1])
test =test[ind,]

MSE=NULL

kk = c(10,25,50,150,250)

for(i in kk){
  
  near = kknn(price~percent.change,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(percent.change,price,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  cat ("Press [enter] to continue")
  line <- readline()
}



#--------------------------------------------------
#fit a tree to boston data just using lstat.

#first get a big tree using a small value of mindev
temp = tree(price~percent.change,data=data.frame,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))

#then prune it down to one with 7 leaves
price.tree=prune.tree(temp,best=7)
cat('pruned tree size: \n')
print(length(unique(price.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.
par(mfrow=c(1,2))

#plot the tree
plot(price.tree,type="uniform")
text(price.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
price.fit = predict(price.tree) #get training fitted values

plot(percent.change,price,cex=.5,pch=16) #plot data
oo=order(percent.change)
lines(percent.change[oo],price.fit[oo],col='red',lwd=3) #step function fit

cvals=c(9.725,4.65,3.325,5.495,16.085,19.9) #cutpoints from tree
for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

rm(list=ls())
detach(data)
