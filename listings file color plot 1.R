####
#---------
##This part of the project focuses on tourism growth and whether or not it can:
##a) be determined from these datasets
##b) be shown to have an effect on price
#---------
####


library(kknn)
library(rpart)
library(tree)
library(ggplot2)

#Load the cleaned dataset.  In Python, I created columns that calculate certain factors
#for tourism growth including the total number of reviews (aggregated from a separate csv)
#and the percent change in reviews from 2014-2019.
data=read.csv('listings with reviews percent change added cleaned.csv')

attach(data)

df=data.frame(latitude,longitude,price,percent.change,number_of_reviews)

attach(df)




par(mfrow=c(1,1))

#Let's begin with some exploratory analysis:


#PLOTS THE NUMBER OF REVIEWS BY GEOGRAPHY
#As expected, the number of reviews is largest near the downtown area (the most popular
#tourist destination)

ggplot(df, aes(latitude, longitude)) +
  geom_point(aes(colour = number_of_reviews)) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour")


#PLOTS THE PERCENT CHANGE IN REVIEWS BY GEOGRAPHY
#Now we are seeing some evidence of outliers.  The largest percent changes tends to be near
#downtown, but there are outliers all over the place.
ggplot(df, aes(latitude, longitude)) +
  geom_point(aes(colour = percent.change)) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour")


#BEGINNING OF KNN:
#We will first try to predict the price based on number of reviews:
plot(number_of_reviews,price)

tr=sample(1:nrow(df),0.8*nrow(df))
train = data.frame(price,percent.change)[tr,]
test = data.frame(price,percent.change)[-tr,]

ind = order(test$percent.change)
test =test[ind,]

MSE=NULL

#Attempt k=25
near = kknn(price~percent.change,train,test,k=25,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)
  
MSE = c(MSE,aux)
  
plot(test$percent.change,test$price,main=paste("k=",25),pch=19,cex=0.8,col="darkgray",
     xlab='Percent Change',ylab='Price')
lines(test$percent.change,near$fitted,col=2,lwd=2)

sqrt(MSE)
#RMSE=$250.69: not very good, but not terrible considering the significant variance of price
#within the dataset (many mansions available for thousands of dollars per night that had
#very few reviews)


MSE=NULL

train = data.frame(price,number_of_reviews)[tr,]
test = data.frame(price,number_of_reviews)[-tr,]

ind = order(test$number_of_reviews)
test =test[ind,]

#Now let's try k=50  
near = kknn(price~number_of_reviews,train,test,k=50,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)

MSE = c(MSE,aux)
  
plot(test$number_of_reviews,test$price,main=paste("k=",50),pch=19,cex=0.8,col="darkgray",
     xlab='Number of Reviews',ylab='Price')
lines(test$number_of_reviews,near$fitted,col=2,lwd=2)

sqrt(MSE)
#RMSE=239.4725: better!



MSE=NULL

#We tried predicting price based on percent change in reviews, but that was even worse
#than predicting based on number of reviews.  Let's see why...

#To understand why this didn't work, let's analyze the total number of reviews and the 
#percent change in reviews from 2014-2019
train = data.frame(number_of_reviews,percent.change)[tr,]
test = data.frame(number_of_reviews,percent.change)[-tr,]

ind = order(test$percent.change)
test =test[ind,]

#This time k=150 was the optimal k
near = kknn(number_of_reviews~percent.change,train,test,k=150,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)
  
MSE = c(MSE,aux)
  
plot(test$percent.change,test$number_of_reviews,main=paste("k=",150),pch=19,cex=0.8,col="darkgray",
     xlab='Percent Change',ylab='Number of Reviews')
lines(test$percent.change,near$fitted,col=2,lwd=2)
#As you can see from the plot, percent change in reviews doesn't even have a distinguishable
#relationship with the number of reviews.  We determined that it is difficult to get meaningful
#information on tourism growth without more specific dates of reviews


sqrt(min(MSE))
#RMSE is about 89 reviews, which is very bad....


MSE=NULL

#Now what if we included geography in the percent change fit?
#Recall the original plot: there were some outliers, but generally it followed the intuitive trend
#of more growth within city limits, close to downtown.

train = data.frame(percent.change,latitude,longitude)[tr,]
test = data.frame(percent.change,latitude,longitude)[-tr,]

ind = order(test$percent.change)
test =test[ind,]


near = kknn(percent.change~latitude+longitude,train,test,k=150,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)
  
MSE = c(MSE,aux)

percent.change.knn.fit=near$fitted
ggplot(test, aes(latitude, longitude)) +
  geom_point(aes(colour = percent.change.knn.fit)) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour")
sqrt(MSE)
#21.2 seems like a rather high RMSE.  The problem here is that there's no way to account for
#new Airbnb locations (I cleaned the dataset to remove locations with 0 pre-2014 reviews,
#but there's no way to divide it beyond that while still being statistically accurate).


#Let's try the same thing with price.  We observed earlier that price tends to be higher
#downtown.  Let's see if the same trend applies to the knn fit.

MSE=NULL

train = data.frame(price,latitude,longitude)[tr,]
test = data.frame(price,latitude,longitude)[-tr,]

ind = order(test$price)
test =test[ind,]


near = kknn(price~latitude+longitude,train,test,k=50,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)
  
MSE = c(MSE,aux)
  


#Plot of the test set for comparison
#ggplot(test, aes(latitude, longitude)) +
#  geom_point(aes(colour = test$price)) +
#  scale_colour_gradient(low = "#56B1F7", high = "#132B43",
#                        space = "Lab", na.value = "grey50", guide = "colourbar",
#                        aesthetics = "colour")

price.knn.fit=near$fitted

ggplot(test, aes(latitude, longitude)) +
  geom_point(aes(colour = price.knn.fit)) +
  scale_colour_gradient(low = "#56B1F7", high = "#132B43",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour")+
  labs(xlab='Latitude',ylab='longitude')
sqrt(min(MSE))
#Aha!  We're getting somewhere.  The plot here shows that prices tend to be higher near
#downtown, with a trend of high price north of downtown as well.  RMSE=$236.48 is once again
#reasonable, given the large price variance in the dataset



#Let's move on now to the tree fits.
temp = tree(price~number_of_reviews,data=data,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))


train = data.frame(price,number_of_reviews)[tr,]
test = data.frame(price,number_of_reviews)[-tr,]

ind = order(test$percent.change)
test =test[ind,]

summary(rpart(price~number_of_reviews,data=train))



#Rpart has shown that the optimal tree size is 5 leaves
price.tree=prune.tree(temp,best=5)
cat('pruned tree size: \n')
print(length(unique(price.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.
par(mfrow=c(1,2))

#plot the tree
plot(price.tree,type="uniform")
text(price.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
price.fit = predict(price.tree)

plot(number_of_reviews,price,cex=.5,pch=16)
oo=order(number_of_reviews)
lines(number_of_reviews[oo],price.fit[oo],col='red',lwd=3)

cvals=c(2.5,13.5,4.5,205.5) #cutpoints from tree
for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

sqrt(mean((price.fit-test$price)^2))
#RMSE=$250.65.  Comparable to knn.


#Let's try the same tree fit with geographical coordinates included.
train = data.frame(price,latitude,longitude,number_of_reviews)[tr,]
test = data.frame(price,latitude,longitude,number_of_reviews)[-tr,]


temp = tree(price~number_of_reviews+latitude+longitude,data=train,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))


summary(rpart(price~number_of_reviews+latitude+longitude,data=train))
        
#then prune it down to one with 22 leaves
price.tree=prune.tree(temp,best=22)
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

plot(number_of_reviews,price,cex=.5,pch=16) #plot data
oo=order(number_of_reviews)
lines(number_of_reviews[oo],price.fit[oo],col='red',lwd=3) #step function fit

cvals=c(11.5,30,198.5,62.5,57) #cutpoints from tree
for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

sqrt(mean((price.fit-train$price)^2))
#While the plot looks strange (keep in mind it's taking into account 3 variables in a
#2-D plane), the fit actually produced a smaller RMSE!  The RMSE of $216.81 made this our most
#accurate tree model.






detach(df)
detach(data)
rm(list=ls())


####THE FOLLOWING TREE MODELS WERE UNSUCCESSFUL AND DIDN'T PRODUCE ANY USEFUL DATA.
#I tried running similar tree models on our second dataset, and didn't have any success.
#The errors were worse and the lack of quantitative variables also posed a challenge
#When looking at factors in tourism growth, it is difficult to use a dataset that focuses
#more on qualitative aspects of the Airbnb locations and less on review counts and 
#visitor numbers.  This dataset was more useful for other parts of the project.
#data=read.csv('listings2.csv')
#attach(data)

#data$price= as.numeric(gsub("\\$", "", data$price))


#tr=sample(1:nrow(data),0.8*nrow(data))
#train = data.frame(data)[tr,]
#test = data.frame(data)[-tr,]


#summary(rpart(price~accommodates+bathrooms+bedrooms,data=train))


#temp = tree(price~
#              accommodates+bathrooms+bedrooms,data=train,mindev=.0001)
#cat('first big tree size: \n')
#print(length(unique(temp$where)))

##then prune it down to one with 3 leaves
#price.tree=prune.tree(temp,best=3)
#cat('pruned tree size: \n')
#print(length(unique(price.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.
#par(mfrow=c(1,2))

#plot the tree
#plot(price.tree,type="uniform")
#text(price.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
#price.fit = predict(price.tree) #get training fitted values

#plot(accommodates+bathrooms+bedrooms,price,cex=.5,pch=16) #plot data
#vec=accommodates[tr]+bathrooms[tr]+bedrooms[tr]
#oo=order(vec)
#lines(vec[oo],price.fit[oo],col='red',lwd=3) #step function fit

#cvals=c(2.5,28.5,30.5,57.5,98.5,130.5,205.5) #cutpoints from tree
#for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

#sqrt(mean((price.fit-train$accommodates)^2))



#temp = tree(price~
#              accommodates,data=train,mindev=.0001)
#cat('first big tree size: \n')
#print(length(unique(temp$where)))

#then prune it down to one with 7 leaves
#price.tree=prune.tree(temp,best=11)
#cat('pruned tree size: \n')
#print(length(unique(price.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.
#par(mfrow=c(1,2))

#plot the tree
#plot(price.tree,type="uniform")
#text(price.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
#price.fit = predict(price.tree) #get training fitted values

#plot(accommodates,price,cex=.5,pch=16) #plot data
#vec=accommodates[tr]+bathrooms[tr]+bedrooms[tr]
#oo=order(accommodates)
#lines(accommodates[oo],price.fit[oo],col='red',lwd=3) #step function fit

#cvals=c(2.5,28.5,30.5,57.5,98.5,130.5,205.5) #cutpoints from tree
#for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

#sqrt(mean((price.fit-train$accommodates)^2))



#rm(list=ls())
#detach(data)

