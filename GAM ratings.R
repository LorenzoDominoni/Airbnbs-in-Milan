rm(list=ls())

library(car)
library(mgcv)

set.seed(27091997)

load("~/CHIARA/Ingegneria matematica/Magistrale/II anno/Nonparametric Statistics/Progetto/Dataset_for_Regression.RData")

attach(datasetnooutliers)
#here we detlete observations with accommodates = 0
dataset <- datasetnooutliers[-which(accommodates==0),]
detach(datasetnooutliers)
attach(dataset)

#Regression model: we want to build a model for the ratings with an inferential point of view. We
#want to give some advice to the hosts of Airbnb on which are the features that can increase the 
#level of their service according to the ratings given by the clients

#Taking into account the results of the tests and different trials this is the model that we have
#built

var2 <- new_price/accommodates

model1=gam(review_scores_rating ~ s(distance,bs='cr')
              + s(I(var2),bs='cr')
              + s(host_total_listings_count,bs='cr')
              + s(new_cleaning, bs='cr')
              + host_is_superhost + host_response_rate + host_identity_verified + Host_greets_you 
              + instant_bookable
              + TV + WiFi + Air_Condition + Breakfast + Heating + Washer 
              + Pets_allowed + Suitable_for_events)
summary(model1)
R2 = c(R2,summary(model1)$r.sq)
#slpine coefficients plot
par(mfrow=c(3,1))
plot(model1)

#permutation test to evaluate the significance of the different variables (here reported for the 
#variable #host_is_superhost)
T0=abs(summary(model1)$p.table[2,3])
gam.H0 = gam(review_scores_rating ~ s(distance,bs='cr')
             + s(I(var1),bs='cr')
             + s(I(var2),bs='cr')
             + s(host_total_listings_count,bs='cr')
             + s(new_cleaning, bs='cr')
             + host_response_rate + host_identity_verified + Host_greets_you 
             + instant_bookable
             + TV + WiFi + Air_Condition + Breakfast + Heating + Washer 
             + Pets_allowed + Suitable_for_events)
residui.H0 <- gam.H0$residuals
B = 1000
n=nrow(dataset)

library(pbapply)

wrapper=function(){
  permutazione <- sample(nrow(dataset))
  residui.H0.perm <- residui.H0[permutazione]
  Y.perm.H0 <- gam.H0$fitted + residui.H0.perm
  gam.perm = gam(Y.perm.H0 ~ s(distance,bs='cr')
                 + s(I(var1),bs='cr')
                 + s(I(var2),bs='cr')
                 + s(host_total_listings_count,bs='cr')
                 + s(new_cleaning, bs='cr')
                 + host_is_superhost + host_response_rate + host_identity_verified + Host_greets_you 
                 + instant_bookable
                 + TV + WiFi + Air_Condition + Breakfast + Heating + Washer 
                 + Pets_allowed + Suitable_for_events)
  return(abs(summary(gam.perm)$p.table[2,3]))
}

T_H0=pbreplicate(B,wrapper(),simplify = 'vector')

hist(T_H0)
abline(v=T0,col='green')
plot(ecdf(T_H0))
abline(v=T0,col='green')
sum(T_H0>=T0)/B 
