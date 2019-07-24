setwd('C:/Users/catym/Desktop/Project Data/Dataset')
airbnb = read.csv("listings.csv")


airbnb <- airbnb[which(airbnb$city=='Austin'),]
attach(airbnb)

int_var_orig <- c('price','accommodates','bathrooms' ,'bedrooms','beds',
             'guests_included','security_deposit','cleaning_fee')  
int_var <- c('price','accommodates','bathrooms' ,'bedrooms','beds',
             'guests_included')
airb <- airbnb[int_var] # 9556
airb <- na.omit(airb) #9506
#summary(airb)
nrow(airb) #9506

attach(airb)

airb$price= as.numeric(gsub("\\$", "", airb$price))
length(airb$price) #9506
summary(airb$price) #415 na 

airb$security_deposit= as.numeric(gsub("\\$", "", airb$security_deposit))
security_deposit
summary(airb$security_deposit) # 5717 missing values, drop! 

airb$cleaning_fee = as.numeric(gsub("\\$", "", airb$cleaning_fee))
airb$cleaning_fee
summary(airb$cleaning_fee) # 3320 missing values, drop! 

#airb <- na.omit(airb)

#NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#replace(cleaning_fee, TRUE, lapply(cleaning_fee, NA2mean))
#lapply(cleaning_fee,NA2mean)
#summary(cleaning_fee)

airb = na.omit(airb) #9091
nrow(airb) #9091
summary(airb)


install.packages("dplyr")
library(dplyr)
airb=airb %>% filter(airb$price!=0)

lm_accom = lm(price~accommodates) #31% 
summary(lm_accom)

lm_bath = lm(price~bathrooms)
summary(lm_bath)

attach(airb)
length(accommodates) #9090


model = lm(airb$price~accommodates+bathrooms+bedrooms+guests_included)
summary(model)

cars
plot(cars$dist,cars$dist)
