rm(list=ls())
set.seed(27091997)

#carico librerie 
library(rgl)
library(DepthProc)

#Carico il dataset con anche le nuove variabili
load("Dataset.RData")
names(dataset)
head(dataset)
attach(dataset)



#### NONPARAMETRIC VISUALIZATION #### 
# Tramite bagplots e depthContour abbiamo visualizzato le caratteristiche più importanti dei nostri dati: Presenza di outliers, Correlazione tra variabili, Code lunghe...
# Abbiamo sempre usato la Tukey depth, perchè non abbiamo riscontrato nessun problema con questo tipo di depth e poichè i risultati erano molto simili quando abbiamo posto qualche cambiamento
# In tutti i grafici abbiamo notato la presenza di code lunghe nelle distribuzioni, che hanno richiesto un preprocessing quando abbiamo svolto la regressione
# Questi ultimi sono considearti da r come outlier, tuttavia sono dovuti all'asimmetria delle distribuzioni
# Guardando i grafici si capisce come l'assunzione di gaussianità dei dati sia completamente irrealistica e un approccio nonparametrico necessario



#### ALL OBSERVATIONS #### 
# Duomo vs logprice (Variabili molto correlate come prevedibile, in scala logaritmica per miglior visualizzazione)
current_data=cbind(duomo,log(daily_price))
plot(current_data, xlab="Distance from Duomo", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Rating vs logprice (La maggior parte degli airbnb ha valutazioni molto alte, non particolaramente correlate con il prezzo)
current_data=cbind(review_scores_rating,log(daily_price))
plot(current_data, xlab="Rating", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Guests vs logprice (jittering è stato aggiunto visto che accommodates è una variabile intera)
current_data=cbind(accommodates+rnorm(nrow(dataset),0,0.3),log(daily_price)) #jittering
plot(current_data, xlab="Guests", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Number of reviews vs Rating (Notare la distribuzione molto particolare)
current_data=cbind(number_of_reviews,review_scores_rating)
plot(current_data, xlab="numberofreviews", ylab="Rating")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

detach(dataset)


#### ONLY OBSERVATIONS IN REASONABLE RANGES #### 
# Abbiamo riproposto la stessa analisi senza osservazioni poco consistenti o al di fuori del range di nostro interesse per scopo di visualizzazione.
# In questo modo si capisce molto meglio la correlazione delle variabili, ma perdiamo idea della distribuzione nel suo insieme.
# Utilizzando entrambi gli approcci si può ottenere il meglio dei 2 mondi

datasetnooutliers=dataset
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$host_total_listings_count<100),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$accommodates<8),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price<500),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price>20),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$guests_included<12),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$minimum_nights<200),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$number_of_reviews>5),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$number_of_reviews<300),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$review_scores_rating>70),]
attach(datasetnooutliers)



# Duomo vs logprice
current_data=cbind(duomo,log(daily_price))
plot(current_data, xlab="Distance from Duomo", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Rating vs logprice 
current_data=cbind(review_scores_rating,log(daily_price))
plot(current_data, xlab="Rating", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Guests vs logprice
current_data=cbind(accommodates+rnorm(nrow(datasetnooutliers),0,0.3),log(daily_price)) #jittering
plot(current_data, xlab="Guests", ylab="Daily Price")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier

# Number of reviews vs Rating
current_data=cbind(number_of_reviews,review_scores_rating)
plot(current_data, xlab="numberofreviews", ylab="Rating")
smed=depthMedian(current_data) 
points(smed[1],smed[2], pch=19, col="red") 
depthContour(current_data,depth_params = list(method='Tukey'))
aplpack::bagplot(current_data,show.whiskers = T,show.loophull = T)
bgplot=aplpack::bagplot(current_data)
bgplot$pxy.outlier



#### SPLINES PER VISUALIZZAZIONE #### 
#Abbiamo utilizzato splines per poter visualizzare meglio le varie dipendenze tra le variabili
#Abbiamo notato che le dipendenze sono spesso nonlineari e complicate -> motivato l'utilizzo della regressione nonparametrica successiva con i GAM
library(splines)
library(gam)
library(mgcv)
library(rgl)



#### SPLINES UNIDIMENSIONALI #### 
#Abbiamo notato che le smooth splines catturavano bene i trend delle variabili grazie alla loro flessibilità
#Alcune volte i gradi di libertà sono stati trovati a mano e altre volte con crossvalidation se performava abbastanza bene

# Duomo vs logprice (ovviamente più ci si allontana dal duomo più il prezzo diminuisce, la cosa interessante è che sopra i 4 km la distanza diventa ininfluente)
fit=smooth.spline(duomo,log(daily_price),df=7)
plot(duomo,log(daily_price),cex =.5, col =" darkgrey ")
lines(fit,col="blue",lwd=2)
fit$lambda
fit$df

# Distance vs logprice
fit=smooth.spline(distance,log(daily_price),cv=T)
plot(distance,log(daily_price),cex =.5, col =" darkgrey ")
lines(fit,col="blue",lwd=2)
fit$lambda
fit$df



#### SPLINE BIDIMENSIONALE #### 
# Si osserva bene tramite una superficie in 3d e l'utilizzo dei gam con thin plate splines per le interazioni come al centro di Milano i prezzi siano decisamente più alti
# Un po' troppo wiggly, ma mostra bene la differenziazione dei quartieri

#Latitudine e Longitudine vs Prezzo
gam.m3=gam(log(daily_price) ~ s(longitude ,bs='cr')+s(latitude ,bs='cr') + s(longitude,latitude,bs='tp'))
longitude.grid=seq(range(longitude)[1],range(longitude)[2],by=0.001)
latitude.grid=seq(range(latitude)[1],range(latitude)[2],by=0.001)
summary(gam.m3)
grid=expand.grid(longitude.grid,latitude.grid)
names(grid)=c('longitude','latitude')
pred=predict(gam.m3,newdata=data.frame(grid,inter=grid$longitude*grid$latitude))
persp3d(longitude.grid,latitude.grid,pred,col='blue')

detach(datasetnooutliers)

#### AIRBNB DATA VISUALIZATION ####

# Di seguito ci si pone l'obiettivo di visualizzare meglio con il supporto di una mappa geografica della citta di 
# Milano alcune caratteristiche del dataset

#Import libraries
library( data.table )
library( ggplot2 )
library( GGally ) 
library(dplyr)
library(tidyverse)
options(repr.plot.width = 15, repr.plot.height = 8)

#Import dataset
dt = read.csv("Airbnb_Milan.csv")
dt = data.table(dt)

# definisco i nomi dei quartieri e li associo second la variabile neighbourhood_cleansed
Municipality <- c("Municipio 1", "Municipio 2", 
                  "Municipio 3","Municipio 4", 
                  "Municipio 5", "Municipio 6",
                  "Municipio 7", "Municipio 8", 
                  "Municipio 9")

#### Plotto tutti gli AIRBNB a Milano

library(leaflet)
library(IRdisplay)

pal <- colorFactor(palette = c("red", "darkorange","brown", "green", 
                               "gold", "blue", "dark green", "yellow", "purple" ), 
                   domain = dt$neighbourhood_cleansed)

p1 <- leaflet(data = dt) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood_cleansed),
                   weight = 1, radius=1,
                   label = paste("Name:", dt$neighbourhood_cleansed)) %>% 
  addLegend("bottomright", pal = pal, values = ~neighbourhood_cleansed,
            title = "Municipality", opacity = 1)

htmlwidgets::saveWidget(p1, "p1.html")
p1



#### Plotto tutti gli AIRBNB a Milano PER PREZZO
# Analogamente al punto precedente, plotto tutti gli appartamenti per fascia di prezzo giornaliera

# set up cut-off values 
breaks <- c(0,50,100,150,200,300,500,1000,4000)
# specify interval/bin labels
tags <- c("[0-50)","[50-100)", "[100-150)", "[150-200)", "[200-300)", "[300-500)","[500-1000)", "[1000-4000)")
# bucketing values into bins
group_tags <- cut(dt$daily_price, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins

ggplot( data =  as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='Total Price') +
  theme_minimal()
pal <- colorFactor(palette = c("red", "darkorange","brown", "green", 
                               "gold", "blue", "dark green", "yellow", "purple" ), 
                   domain = tags)

setDT(dt)[,"SET":= cut(dt$daily_price, breaks = breaks, labels = tags), by= dt$daily_price]
setDT(dt)[,"N_SET":= cut(dt$daily_price, breaks= breaks , 1:8), by= dt$daily_price]

pal <- colorNumeric( palette = "OrRd", domain = dt$NN_SET)

dt$NN_SET <-as.numeric(dt$N_SET)
p1 <- leaflet(data = dt) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(NN_SET),
                   weight = 4, radius=4,
                   label = paste("Name:", dt$NN_SET)) %>%
  addLegend("bottomright", pal = pal, values = ~NN_SET,
            title = "Price Range", opacity = 1)

htmlwidgets::saveWidget(p1, "p1.html")
p1

