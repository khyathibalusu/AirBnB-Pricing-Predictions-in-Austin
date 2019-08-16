library('tidyverse')
listings = read.csv('C:\\Users\\shiva\\Documents\\listings.csv')
summary(listings)
listings$room_type<-as.factor(listings$room_type)
##DISTRIBUTIONS USING HISTOGRAMS
##price distribution
ph<-ggplot(data=listings,aes(x=price))+
  geom_histogram(binwidth=5000, colour="black", fill="white")
ph + scale_x_log10()
ph
##minimum nights distribution
ggplot(data=listings, aes(x=minimum_nights)) +
  geom_histogram(binwidth=.3, colour="black", fill="white")+scale_x_log10()
##number_of_reviews distribution
ggplot(data=listings, aes(x=number_of_reviews)) +
  geom_histogram(binwidth=.25, colour="black", fill="white")+scale_x_log10()
##Availability distribution
ggplot(data=listings, aes(x=availability_365)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

##SCATTER PLOTS
ggplot(data=listings)+geom_point(mapping = aes(x=minimum_nights,y=price,color=room_type
                                               ))+scale_y_log10()+scale_x_log10()
ggplot(data=listings)+geom_point(mapping = aes(x=room_type,y=price))
ggplot(data=listings,mapping = aes(x=room_type,y=price))+geom_boxplot()+scale_y_log10()
ggplot(data=listings)+geom_point(mapping = aes(x=host_name,y=price))
ggplot(data=listings)+geom_point(mapping = aes(x=name,y=price))

ggplot(data=listings,mapping = aes(x=as.factor(neighbourhood),y=price))+geom_boxplot() +theme(axis.text.x=element_text(angle=90, hjust=1))+scale_y_log10()
ggplot(data=listings)+geom_point(mapping = aes(x=number_of_reviews,y=price))
arranged_list = listings %>% arrange(desc(price))
head(arranged_list)
quantile(listings$price)


##VERY HIGH NUMBER OF NIGHTS

ggplot(data=listings,mapping = aes(y=price))+geom_boxplot() +theme(axis.text.x=element_text(angle=90, hjust=1))+scale_y_log10()



##HIGH PRICE ANALYSIS
b = arranged_list[arranged_list$price>=300,]
quantile(b$price)

##PROPERTY OWNER
c = b %>% group_by(name) %>% summarise(count = n()) %>% arrange(desc(count))
head(c)

summary(listings)

ggplot(data=listings)+geom_point(mapping = aes(x=longitude,y=latitude))
head(b)
nrow(b)
