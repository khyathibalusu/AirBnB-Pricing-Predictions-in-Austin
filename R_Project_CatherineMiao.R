setwd('C:/Users/catym/Desktop/Project Data/Dataset')
airbnb = read.csv("listings.csv")


airbnb <- airbnb[which(airbnb$city=='Austin'),]
attach(airbnb)

int_var_orig <- c('price','accommodates','bathrooms' ,'bedrooms','beds',
                  'guests_included','security_deposit','cleaning_fee')  
int_var <- c('price','accommodates','bathrooms' ,'bedrooms','beds',
             'guests_included','host_response_time','host_is_superhost'
             ,'number_of_reviews','cancellation_policy','maximum_nights','availability_365',
             'availability_30','availability_60','availability_90','neighbourhood_cleansed',
             'host_total_listings_count','host_identity_verified','calculated_host_listings_count',
             'beds','room_type','property_type')
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
host_is_superhost = as.factor(airb$host_is_superhost)
host_is_superhost = ifelse(host_is_superhost=="t",1,0)
length(host_is_superhost)
#airb <- na.omit(airb)

#NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#replace(cleaning_fee, TRUE, lapply(cleaning_fee, NA2mean))
#lapply(cleaning_fee,NA2mean)
#summary(cleaning_fee)

airb = na.omit(airb) #9091
nrow(airb) #9091
summary(airb)


#install.packages("dplyr")
library(dplyr)
airb=airb %>% filter(airb$price!=0)

lm_accom = lm(price~accommodates) #31% 
summary(lm_accom)

lm_bath = lm(price~bathrooms)
summary(lm_bath)


attach(airb)
length(accommodates) #9090



num_reviews=airb$number_of_reviews
length(num_reviews)

cancel=as.factor(airb$cancellation_policy)
length(cancel)
summary(cancel)

avail_365 = airb$availability_365
length(avail_365)

avail_30 = airb$availability_30
length(avail_30)

avail_60 = airb$availability_60
length(avail_60)

avail_90 = airb$availability_90
length(avail_90)

host_is_superhost = as.factor(airb$host_is_superhost)
host_is_superhost = ifelse(host_is_superhost=="t",1,0)
length(host_is_superhost)

zip = airb$neighbourhood_cleansed
length(zip)
uni_zip=unique(zip)
zip_sort=sort(uni_zip)
zip[zip<78715] <- "zip_one"
zip[78714<zip & zip<78730] <- "zip_two" 
zip[78729<zip & zip<78745] <- "zip_three"
zip[78744<zip & zip<78760] <- "zip_four"
length(zip)
summary(zip)

host_list = airb$calculated_host_listings_count

total_list = airb$host_total_listings_count
length(total_list)

host_id = airb$host_identity_verified

#max_nights = airb$maximum_nights
#length(max_nights)

model = lm(airb$price~accommodates+bathrooms+bedrooms+beds+guests_included+
             host_is_superhost+num_reviews+avail_365+avail_30+avail_60+avail_90+zip+total_list
           +host_id+host_list+room_type)
summary(model)

levels(room_type)


# best subset selection
library(leaps)
sub_select = regsubsets(price~accommodates+bathrooms+bedrooms+beds+guests_included+
                          host_is_superhost+num_reviews+avail_365+avail_30+avail_60+avail_90+zip+total_list
                        +host_id+host_list+room_type,airb) #16 variables 
summary(sub_select)
#accodmodates, bathrooms, bedrooms, super, num_rev, avail_30, avail_90, zip_one, 
# room_type 

red_model = lm(price~accommodates+bathrooms+bedrooms+
                 host_is_superhost+num_reviews+avail_30+avail_90+zip
               +room_type,airb)
summary(red_model) 
nrow(airb)


############################################################################
reg_var <- c('price','accommodates','bathrooms' ,'bedrooms'
             ,'host_is_superhost','number_of_reviews',
             'availability_30','availability_90','neighbourhood_cleansed',
             'room_type')
reg_data <- airb[reg_var]

nrow(reg_data)

#OLS
train=reg_data[1:8000,]
test = reg_data[-(1:8000),]

ls = lm(price ~., data=train)
pred = predict(ls, data= test)
MSE.ols=mean((test[,'price'] - pred)^2)
MSE.ols 

sqrt(MSE.ols) #255.4306

#ridge
train=reg_data[1:8000,]
test = reg_data[-(1:8000),]

library(glmnet)
train.matrix = model.matrix(price~., data=train)
test.matrix = model.matrix(price~.,data=test)
grid=10^seq(10,-2,length=100)
ridge = cv.glmnet(train.matrix, train[,'price'], alpha=0, lambda=grid, thresh=1e-12)
# the best lambda 
best = ridge$lambda.min
best #0.4977024
ridge.pred = predict(ridge, newx=test.matrix, s=best)
MSE.ridge=mean((test[,'price']- ridge.pred)^2)
MSE.ridge #30478.35 
sqrt(MSE.ridge) #174.5892

library(glmnet)
train.matrix = model.matrix(price~., data=train)
test.matrix = model.matrix(price~.,data=test)
grid=10^seq(10,-2,length=100)
lasso = cv.glmnet(train.matrix, train[,'price'], alpha=1, lambda=grid, thresh=1e-12)
# the best lambda 
best = lasso$lambda.min
best 
lasso.pred = predict(lasso, newx=test.matrix, s=best)
MSE.lasso=mean((test[,'price']- lasso.pred)^2)
MSE.lasso 
sqrt(MSE.lasso)
