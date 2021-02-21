rm(list=ls())
set.seed(27091997)

#Carico il dataset con anche le nuove variabili
load("Dataset.RData")

# Rimuovo osservazioni inconsistenti o fuori dal nostro range di interesse
datasetnooutliers=dataset
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$host_total_listings_count<200),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$accommodates<13),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price<1000),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price>20),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$guests_included<15),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$minimum_nights<200),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$number_of_reviews>5),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$review_scores_rating>60),]

attach(datasetnooutliers)



#### TEST PERMUTAZIONALI #### 
# Ognuno dei prossimi codici è svolto sia sul prezzo che sul rating, per ognuno abbiamo riportato solo 1 dei 2 casi
# I risultati dei test sono in fondo alla pagina


#### TEST 2 POPOLAZIONI INDIPENDENTI UNIVARIATE #### 
# Codice per il test su 2 popolazioni indipendenti univariate per confrontare la media del prezzo a seconda delle categorie dell'host
# Il codice è il medesimo per ogni categoria quindi ne abbiamo riportato solo uno (host_is_superhost)
# Abbiamo provato anche con la mediana data la presenza di code lunghe, ma abbiamo ottenuto praticamente gli stessi risultati
# Grazie a questi test abbiamo compreso quali fossero le variabili rilevanti per il prezzo

# host_is_superhost vs price
set.seed(27091997)

x_pooled=daily_price
category=host_is_superhost

x1 <- x_pooled[which(category==0)]
x2 <- x_pooled[which(category==1)]

n=length(x_pooled)
n1=length(x1)

boxplot(x1,x2,main='Original data')

# Statistica test: differenza tra le due medie
T0 <- abs(mean(x1) - mean(x2))
T0

B <- 10000 # Numero di permutazioni
T_stat <- numeric(B) 

for(perm in 1:B){
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # statistica test permutata
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

p_val <- sum(T_stat>=T0)/B
p_val



# stesso test di prima, ma ora la variabile è il rating (come è normale che sia qui l'host conta molto di più -> i p value sono quasi tutti zero)
# 2 popolazioni indipendenti univariate con test permutazionale data la non gaussianità dei dati

# host_is_superhost vs rating
set.seed(27091997)

x_pooled=review_scores_rating
category=host_is_superhost

x1 <- x_pooled[which(category==0)]
x2 <- x_pooled[which(category==1)]

n=length(x_pooled)
n1=length(x1)

boxplot(x1,x2,main='Original data')

# Statistica test: differenza tra le due medie
T0 <- abs(mean(x1) - mean(x2))
T0

B <- 10000 # Numero di permutazioni
T_stat <- numeric(B)

for(perm in 1:B){
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # statistica test permutata
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=2)

p_val <- sum(T_stat>=T0)/B
p_val



#### TEST ANOVA UNIVRIATI #### 

# Test Anova su dati univariati per capire se la variabile dipende dal quartiere o meno 
# Sia il pezzo che il rating hanno p value molto bassi, quindi bisogna senz'altro tenere conto della location nelle successive analisi

set.seed(27091997)

x_pooled=daily_price
category=neighbourhood_cleansed
g <- length(unique(category))
n <- length(x_pooled)

plot(category, x_pooled, xlab='Neighborhood',main='Original Data')

fit <- aov(x_pooled ~ category)
T0 <- summary(fit)[[1]][1,4] # Statistica test: F value dell'anova
T0

B <- 10000 # Numero di permutazioni
T_stat <- numeric(B) 

for(perm in 1:B){
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  fit_perm <- aov(x_perm ~ category)
  # Statistica test permutata
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val



# Siccome sembra che la significatività dell'anova sia dovuta solo alla zona 1 (centro, area c) abbiamo provato un test solo sulle altre zoe
# Questa volta i p value sono molto più alti, questo fa capire come principalmente la location sia importante quasi solo se intesa come distanza dal centro

set.seed(27091997)

x_pooled=daily_price[which(neighbourhood_cleansed!=1)]
category=neighbourhood_cleansed[which(neighbourhood_cleansed!=1)]
g <- length(unique(category))
n <- length(x_pooled)

plot(category, x_pooled, xlab='Neighborhood',main='Original Data')

fit <- aov(x_pooled ~ category)
T0 <- summary(fit)[[1]][1,4] # Statistica test: F value dell'anova
T0

B <- 10000 # Numero di permutazioni
T_stat <- numeric(B) 

for(perm in 1:B){
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  fit_perm <- aov(x_perm ~ category)
  # Statistica test permutata
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val



# Test su ogni facility presente nel nostro dataset per capire quali risultano essere significative per il prezzo e il rating
# 2 popolazioni indipendenti univariate ogni volta 

set.seed(27091997)
for(i in 39:61){
  x_pooled=review_scores_rating
  category=datasetnooutliers[,i]

  x1 <- x_pooled[which(category==0)]
  x2 <- x_pooled[which(category==1)]

  n=length(x_pooled)
  n1=length(x1)

  par(mfrow=c(1,2))
  boxplot(x1,x2,main='Original data')

  # Statistica test: differenza tra le medie
  T0 <- abs(mean(x1) - mean(x2))
  T0

  B <- 10000 # Numero di permutazioni
  T_stat <- numeric(B) 

for(perm in 1:B){
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # statistica test permutata
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

  hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
  abline(v=T0,col=3,lwd=2)

  plot(ecdf(T_stat))
  abline(v=T0,col=3,lwd=2)

  p_val <- sum(T_stat>=T0)/B
  print(colnames(datasetnooutliers)[i])
  print(p_val)
}

detach(datasetnooutliers)



# Qui abbiamo posto i risultati di tutti i test svolti 

# Superhost vs price pvalue=0.7488
# hostlocation vs price pvalue=0.0366
# hostresponsetime vs price pvalue=0.6949
# hostresponserate vs price pvalue=0.8846
# host total listings vs price pvalue=0
# hostidentity vs price pvalue=0.0313
# hostgreeting vs price pvalue=0.0712

# Superhost vs rating pvalue=0 
# hostlocation vs rating pvalue=0.0447
# hostresponsetime vs rating=0.4964
# hostresponserate vs rating pvalue=0 
# host total listings vs rating pvalue=0 
# hostidentity vs rating pvalue=0 
# hostgreeting vs rating pvalue=0 

# neighborhood anova vs price pvalue=0
# neighborhood anova vs rating pvalue=3e-04

# neighborhood anova without zone 1 vs price pvalue=0.8583
# neighborhood anova without zone 1 vs rating pvalue=0.0319

# facilities vs price
# [1] "instant_bookable"
# [1] 0.5168
# [1] "cancellation_policy"
# [1] 0
# [1] "require_guest_profile_picture"
# [1] 0.9212
# [1] "require_guest_phone_verification"
# [1] 0.2139
# [1] "TV"
# [1] 0
# [1] "WiFi"
# [1] 1e-04
# [1] "Air_Condition"
# [1] 0
# [1] "Wheelchair_accessible"
# [1] 0
# [1] "Kitchen"
# [1] 0.472
# [1] "Breakfast"
# [1] 0.8975
# [1] "Elevator"
# [1] 0
# [1] "Heating"
# [1] 0.8892
# [1] "Washer"
# [1] 4e-04
# [1] "Iron"
# [1] 0.2002
# [1] "Host_greets_you"
# [1] 0.0694
# [1] "Paid_parking_on_premises"
# [1] 0.5505
# [1] "Luggage_dropoff_allowed"
# [1] 3e-04
# [1] "Long_term_stays_allowed"
# [1] 0.004
# [1] "Doorman"
# [1] 0
# [1] "Pets_allowed"
# [1] 0.1691
# [1] "Smoking_allowed"
# [1] 0.0014
# [1] "Suitable_for_events"
# [1] 3e-04
# [1] "X24_hour_check_in"
# [1] 0.0063

# facilities vs rating
# [1] "instant_bookable"
# [1] 0
# [1] "cancellation_policy"
# [1] 0
# [1] "require_guest_profile_picture"
# [1] 2e-04
# [1] "require_guest_phone_verification"
# [1] 0.0013
# [1] "TV"
# [1] 0
# [1] "WiFi"
# [1] 0
# [1] "Air_Condition"
# [1] 0
# [1] "Wheelchair_accessible"
# [1] 0.3496
# [1] "Kitchen"
# [1] 0.0133
# [1] "Breakfast"
# [1] 0
# [1] "Elevator"
# [1] 1e-04
# [1] "Heating"
# [1] 0
# [1] "Washer"
# [1] 0
# [1] "Iron"
# [1] 0
# [1] "Host_greets_you"
# [1] 0
# [1] "Paid_parking_on_premises"
# [1] 0
# [1] "Luggage_dropoff_allowed"
# [1] 0
# [1] "Long_term_stays_allowed"
# [1] 0
# [1] "Doorman"
# [1] 0.1892
# [1] "Pets_allowed"
# [1] 0
# [1] "Smoking_allowed"
# [1] 0
# [1] "Suitable_for_events"
# [1] 0
# [1] "X24_hour_check_in"
# [1] 0.256
