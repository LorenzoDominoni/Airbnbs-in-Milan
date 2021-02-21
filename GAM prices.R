rm(list=ls())

# Carico le librerie
library(mgcv)
library(car)
library(conformalInference)

# Carico il dataset pronto per la regressione
load("Dataset_for_Regression.RData")



#### VALID E TRAIN SPLIT ####
# Dividiamo i nostri dati in un training e validation set, così che poi potremo controllare le le nostre predizioni sono accurate
# Preferiamo utilizzare questo metodo rispetto alla crossvalidation per questioni computazionali e per il fatto che il dataset è abbastanza grande da permetterlo

set.seed(27091997)
n = dim(datasetnooutliers)[1]
ampiezza_train = round(n*0.90)
campione = sample(n)
train = sort(campione[1:ampiezza_train])
valid = sort(campione[(ampiezza_train+1):n])
datasetnooutliers_train = datasetnooutliers[train,]
datasetnooutliers_valid = datasetnooutliers[valid,]
R2 = NULL



#### GAM ####
# Scelta dei GAM dovuta alla multidimensionalità del dataset
# Abbiamo pensato a 2 tipi di GAM, uno essenziale con solo le variabili davvero necessarie e uno con tutto ciò che abbiamo trovato di significativo
# Il primo è studiato per un soggetto che ha poche info sull'abitazione o poco tempo, il secondo per chi vuole fare un'analisi approfondita



#### GAM ESSENZIALE ####
# Variabili più rilevanti scelte tramite test sui coefficienti della regressione (vedi codice ...)
# Smooth terms sulle variabili numeriche e semplici coefficienti su quelle categoriche
model1=gam(new_log_price ~ 
                s(distance,bs='cr')  
              + s(metratura,bs='cr')
              + s(accommodates,bs='cr')
              + basic + standard + business + superior + luxury,
              data = datasetnooutliers_train)

summary(model1)
# Tutti i termini molto significativi poi confermati dai test permutazionali
# R quadro aggiustato alto per un problema del genere 
R2 = c(R2,summary(model1)$r.sq)

#Plot generale del modello
par(mfrow=c(2,2))
gam.check(model1) # Check buono, con residui tuttavia non gaussiani

#Plot dei coefficenti delle spline
par(mfrow=c(1,3))
plot(model1) #Spline marginali sensate (crescita e decrescita) e che giustificano l'uso di un metodo non lineare

ks.test(x=model1$residuals,y='pnorm',alternative='two.sided') #residui non gaussiani, necessità di test permutazionali



#### PREDICTION SUL GAM ESSENZIALE ####
# Qui abbiamo testato il nostro modello su un validation set per verificarne l'efficacia

pred=exp(predict(model1,newdata = datasetnooutliers_valid)) #previsioni

par(mfrow = c(2, 2))

#Plot delle previsioni (nero) vs valori reali (verde)
#Ogni x è una casa con il prezzo predetto e reale associato sulla y
#Le previsioni sono buone, tranne quelle a prezzo alto che normalemnte sottostimano un po'
m = 15
M =max(pred,datasetnooutliers_valid$new_price)
plot(valid,pred,ylim = c(m,M),pch=1,main = "Modello 1")
points(valid,datasetnooutliers_valid$new_price,pch=3,col='green3')
segments(valid,pred,valid,datasetnooutliers_valid$new_price,lty=3)

#Focus sulla fascia (0,200)
scalato = 200
plot(valid,pred,ylim = c(m,scalato),pch=1,main = "Modello 1 scalato")
points(valid,datasetnooutliers_valid$new_price,pch=3,col='green3')
segments(valid,pred,valid,datasetnooutliers_valid$new_price,lty=3)

# Plot delle previsioni (nero) vs valori reali (verde) in ordine crescente
# Utile per vedere se vi sono particolari fasce di prezzo con problemi (qui nessuno)
plot(valid,sort(pred),ylim = c(m,M),type='l',main = "Modello 1 ordinato")
lines(valid,sort(datasetnooutliers_valid$new_price),col='green3',cex=0.1)

#Focus sulla fascia (0,200)
plot(valid,sort(pred),ylim = c(m,scalato),type='l',main = "Modello 1 ordinato scalato")
lines(valid,sort(datasetnooutliers_valid$new_price),col='green3',cex=0.1)

#Trova quante predizioni troppo sbagliate 
# 20 a più di 50 euro di distanza da quelle reali e 7 a più di 100, sono tutte osservazioni poco sensate che reputiamo errori di airbnb o dell'host
male1 = datasetnooutliers[valid[abs(pred-datasetnooutliers_valid$new_price)>50],]
nrow(male1)
male1 = datasetnooutliers[valid[abs(pred-datasetnooutliers_valid$new_price)>100],]
nrow(male1)



#### GAM COMPLETO ####
# Variabili più rilevanti scelte tramite test sui coefficienti della regressione (vedi codice ...)
# Smooth terms sulle variabili numeriche e semplici coefficienti su quelle categoriche
model2=gam(new_log_price ~ 
                s(distance,bs='cr')  
              + s(metratura,bs='cr')
              + s(accommodates,bs='cr')
              + s(new_log_clean,bs='cr')
              + s(review_scores_rating,bs='cr')
              + s(guests_included,bs='cr',k=8)
              + s(log_reviews,bs='cr')
              + basic + standard + business + superior + luxury
              + cancellation_policy
              + TV + WiFi + Air_Condition + Wheelchair_accessible + Elevator + Kitchen
              + Luggage_dropoff_allowed + Long_term_stays_allowed + Doorman + Smoking_allowed 
              + Suitable_for_events + X24_hour_check_in,
              data = datasetnooutliers_train)

summary(model2)
# Tutti i termini numerici molto significativi poi confermati dai test permutazionali
# Facilities non tutte significative ma che abbiamo lasciato perchè sono significative nei test permutazionali
# R quadro aggiustato poco più alto di prima
R2 = c(R2,summary(model2)$r.sq)

#Plot generale del modello
par(mfrow=c(2,2))
gam.check(model2) # Check buono, con residui tuttavia non gaussiani

#Plot dei coefficenti delle spline
par(mfrow=c(2,4))
plot(model2) #Spline marginali sensate (crescita e decrescita) e che giustificano l'uso di un metodo non lineare
# Interessante notare il fatto che maggiore è il numero di reviews, minore è il prezzo (sensato ma non scontato)

ks.test(x=model2$residuals,y='pnorm',alternative='two.sided') #residui non gaussiani, necessità di test permutazionali



#### PREDICTION SUL GAM COMPLETO ####
# Qui abbiamo testato il nostro modello su un validation set per verificarne l'efficacia

pred=exp(predict(model2,newdata = datasetnooutliers_valid)) #previsioni

par(mfrow = c(2, 2))

#Plot delle previsioni (nero) vs valori reali (verde)
#Ogni x è una casa con il prezzo predetto e reale associato sulla y
#Le previsioni sono buone, tranne quelle a prezzo alto che normalemnte sottostimano un po'
m = 15
M =max(pred,datasetnooutliers_valid$new_price)
plot(valid,pred,ylim = c(m,M),pch=1,main = "Modello 2")
points(valid,datasetnooutliers_valid$new_price,pch=3,col='green3')
segments(valid,pred,valid,datasetnooutliers_valid$new_price,lty=3)

#Focus sulla fascia (0,200)
scalato = 200
plot(valid,pred,ylim = c(m,scalato),pch=1,main = "Modello 2 scalato")
points(valid,datasetnooutliers_valid$new_price,pch=3,col='green3')
segments(valid,pred,valid,datasetnooutliers_valid$new_price,lty=3)

# Plot delle previsioni (nero) vs valori reali (verde) in ordine crescente
# Utile per vedere se vi sono particolari fasce di prezzo con problemi (qui nessuno)
plot(valid,sort(pred),ylim = c(m,M),type='l',main = "Modello 2 ordinato")
lines(valid,sort(datasetnooutliers_valid$new_price),col='green3',cex=0.1)

#Focus sulla fascia (0,200)
plot(valid,sort(pred),ylim = c(m,scalato),type='l',main = "Modello 2 ordinato scalato")
lines(valid,sort(datasetnooutliers_valid$new_price),col='green3',cex=0.1)

#Trova quante predizioni troppo sbagliate 
# 21 a più di 50 euro di distanza da quelle reali e 8 a più di 100, sono tutte osservazioni poco sensate che reputiamo errori di airbnb o dell'host
male2 = datasetnooutliers[valid[abs(pred-datasetnooutliers_valid$new_price)>50],]
nrow(male2)
male2 = datasetnooutliers[valid[abs(pred-datasetnooutliers_valid$new_price)>100],]
nrow(male2)



#### INTERVALLI DI PREDIZIONE SUL VALIDATION SET ####
# Abbiamo utilizzato split conformal predictions per ottenere intervalli di previsione sul validation set
# Split conformal per questioni computazionali
# Motivazione: capire quali prezzi sono poco sensati e quale è l'intervallo entro cui poter variare il prezzo a seconda dell'obiettivo e del "rischio desiderato"
attach(datasetnooutliers_train)

# Funzione che costruisce il GAM essenziale
train_gam=function(x,y,out=NULL){
  colnames(x)=c('var1','var2','var3','var4','var5','var6','var7','var8')
  train_data=data.frame(y,x)
  model_gam=gam(y ~ 
                  s(var1,bs='cr')  
                + s(var2,bs='cr')
                + s(var3,bs='cr')
                + var4 + var5 + var6 + var7 + var8,
                data = train_data)
}

# Funzione per fare predizioni con il GAM essenziale
predict_gam=function(obj, new_x){
  new_x=data.frame(new_x)
  colnames(new_x)=c('var1','var2','var3','var4','var5','var6','var7','var8')
  predict.gam(obj,new_x)
}

# Split conformal predictions a livello 95% con lo split sul training set a: 4200 osservazioni per il vero training, 1967 per il resto
c_preds=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                             new_log_price,cbind(datasetnooutliers_valid$distance,datasetnooutliers_valid$metratura,datasetnooutliers_valid$accommodates,datasetnooutliers_valid$basic,datasetnooutliers_valid$standard,datasetnooutliers_valid$business,datasetnooutliers_valid$superior,datasetnooutliers_valid$luxury),
                             alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)



#### PLOT DEGLI INTERVALLI DI PREDIZIONE SUL VALIDATION SET ####
# Parametri utili per il plot (bisogna ordinare i dati rispetto alle previsioni in ordine crescente)
preds1=sort(c_preds[['pred']], index.return=TRUE)
sorted1=numeric(dim(datasetnooutliers_valid)[1])
los1=numeric(dim(datasetnooutliers_valid)[1])
ups1=numeric(dim(datasetnooutliers_valid)[1])
for(i in 1:dim(datasetnooutliers_valid)[1]){
  sorted1[i]=datasetnooutliers_valid$new_log_price[preds1$ix][i]
  los1[i]=c_preds[['lo']][preds1$ix][i]
  ups1[i]=c_preds[['up']][preds1$ix][i]
}

# plot dei valori predetti (rosso) vs reali (grigio) in ordine crescente con corrispettivi intervalli di confidenza al 95% (blu)
# Questo plot è utile per notare la bontà della previsione (circa il 95% dei dati del validation set si trova all'interno dell'intervallo)
# Ogni x è una casa con associate sulla y i valori predetti e reali
par(mfrow=c(1,1))
plot(exp(sorted1) ,cex =.5, col =" darkgrey " )
lines(exp(preds1$x) ,lwd =2, col ="red")
matlines(cbind(exp(ups1),exp(los1)) ,lwd =1, col ="blue")

#Focus sulla fascia (0,200)
m=0
M=200
plot(exp(sorted1), ylim = c(m,M) ,cex =.5, col =" darkgrey ", xlab = "Ordered apartments", ylab = "Price")
lines(exp(preds1$x) ,lwd =2, col ="red")
matlines(cbind(exp(ups1),exp(los1)) ,lwd =1, col ="blue")
detach(datasetnooutliers_train)



#### INTERVALLI DI PREDIZIONE SU VALORI INTERESSANTI (TEST SET) ####
# Abbiamo provato a trovare intervalli di previsione per abitazioni nostre e di nostri amici differenti tra loro
# Da considerare le diversità tra casa in cui vivere e in cui fare airbnb (metratura, lusso)



#### INTERVALLI DI PREDIZIONE SU VALORI INTERESSANTI (TEST SET) CON IL MODELLO ESSENZIALE ####
# Servono per il primo modello coordinate, metratura, accommodates, lusso

attach(datasetnooutliers)
# Funzioni per calcolare la distanza e il "sightseeing score" a partire dalle coordinate
load('distanza.RData')
load('distancenew.RData')



# Casa Dominoni

# Scrivo le variabili 
distancetest=distancenew(c(45.46720634371883, 9.139691052458092))
metraturatest=90
accommodatestest=4
basictest=FALSE
standardtest=FALSE
businesstest=FALSE
superiortest=TRUE
luxurytest=FALSE

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,basictest,standardtest, businesstest,superiortest,luxurytest),
                                 alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 162.3885 196.4398 237.6313



# Casa Arcardini

# Scrivo le variabili 
distancetest=distancenew(c(45.486425268162975, 9.215948268258082))
metraturatest=80
accommodatestest=3
basictest=FALSE
standardtest=FALSE
businesstest=FALSE
superiortest=TRUE
luxurytest=FALSE

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,basictest,standardtest, businesstest,superiortest,luxurytest),
                                 alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 127.1266 153.7838 186.0308



# Casa Amico di Gabri

# Scrivo le variabili 
distancetest=distancenew(c(45.49097531900163, 9.220069585539083))
metraturatest=25
accommodatestest=2
basictest=FALSE
standardtest=FALSE
businesstest=TRUE
superiortest=FALSE
luxurytest=FALSE

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,basictest,standardtest, businesstest,superiortest,luxurytest),
                                 alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 55.44585 67.07231 81.13672



# Casa Camisa

# Scrivo le variabili 
distancetest=distancenew(c(45.475127583747245, 9.191783071196623))
metraturatest=170
accommodatestest=6
basictest=FALSE
standardtest=FALSE
businesstest=FALSE
superiortest=TRUE
luxurytest=FALSE

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,basictest,standardtest, businesstest,superiortest,luxurytest),
                                 alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 330.8879 400.2719 484.205



# Casa Piazza

# Scrivo le variabili 
distancetest=distancenew(c(45.47976615563778, 9.18264333432099))
metraturatest=37
accommodatestest=2
basictest=FALSE
standardtest=TRUE
businesstest=FALSE
superiortest=FALSE
luxurytest=FALSE

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,basic,standard,business,superior,luxury),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,basictest,standardtest, businesstest,superiortest,luxurytest),
                                 alpha=0.2,verbose=T,train.fun = train_gam ,predict.fun = predict_gam, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 43.72001 52.88767 63.97771



#### INTERVALLI DI PREDIZIONE SU VALORI INTERESSANTI (TEST SET) CON IL MODELLO COMPLETO ####
# La predizione è sempre molto simile a quella del primo modello, con qualche accorgimento in più su alcuni punti meno importanti, utile per un'analisi approfondita

# Funzione che costruisce il GAM completo
train_gam2=function(x,y,out=NULL){
  colnames(x)=c('var1','var2','var3','var4','var5','var6','var7','var8','var9','var10','var11','var12','var13','var14','var15','var16','var17','var18','var19','var20','var21','var22','var23','var24','var25')
  train_data=data.frame(y,x)
  model_gam=gam(y ~ 
                  s(var1,bs='cr')  
                + s(var2,bs='cr')
                + s(var3,bs='cr')
                + s(var4,bs='cr')
                + s(var5,bs='cr')
                + s(var6,bs='cr',k=8)
                + s(var7,bs='cr')
                + var8 + var9 + var10 + var11+ var12
                + var13 + var14 + var15 + var16 + var17 + var18
                + var19 + var20 + var21 + var22 
                + var23 + var24+ var25,
                data = train_data)
}

# Funzione per fare predizioni con il GAM completo
predict_gam2=function(obj, new_x){
  new_x=data.frame(new_x)
  colnames(new_x)=c('var1','var2','var3','var4','var5','var6','var7','var8','var9','var10','var11','var12','var13','var14','var15','var16','var17','var18','var19','var20','var21','var22','var23','var24','var25')
  predict.gam(obj,new_x)
}



#casa Dominoni

# Scrivo le variabili 
distancetest=distancenew(c(45.46720634371883, 9.139691052458092))
metraturatest=90
accommodatestest=4
new_log_cleantest=log(50) #tra 10 e 100
review_scores_ratingtest=95 #mediana 95 usabile quando questo dato non è noto, tra 70 e 100
guests_includedtest=2
log_reviewstest=log(21) #mediana 21 usabile quando questo dato non è noto, tra 3 e 700
basictest=FALSE
standardtest=FALSE
businesstest=FALSE
superiortest=TRUE
luxurytest=FALSE
cancellation_policytest=1 #1 for possible cancellation, 0 otherwise
TVtest=1 #1 for yes, 0 for no
WiFitest= 1 #1 for yes, 0 for no
Air_Conditiontest=0 #1 for yes, 0 for no
Wheelchair_accessibletest=0 #1 for yes, 0 for no
Elevatortest=1 #1 for yes, 0 for no
Kitchentest=1 #1 for yes, 0 for no
Luggage_dropoff_allowedtest=1 #1 for yes, 0 for no
Long_term_stays_allowedtest=0 #1 for yes, 0 for no
Doormantest=1 #1 for yes, 0 for no
Smoking_allowedtest=0 #1 for yes, 0 for no
Suitable_for_eventstest=0 #1 for yes, 0 for no
X24_hour_check_intest=1 #1 for yes, 0 for no

# Split conformal predictions
c_predstest=conformal.pred.split(cbind(distance,metratura,accommodates,
                                       new_log_clean,review_scores_rating,guests_included,log_reviews,
                                       basic,standard,business,superior,luxury,
                                       cancellation_policy,TV,WiFi,Air_Condition,
                                       Wheelchair_accessible,Elevator,Kitchen,
                                       Luggage_dropoff_allowed,Long_term_stays_allowed,
                                       Doorman,Smoking_allowed,Suitable_for_events,X24_hour_check_in),
                                 new_log_price,cbind(distancetest,metraturatest,accommodatestest,
                                                     new_log_cleantest,review_scores_ratingtest,guests_includedtest,log_reviewstest,
                                                     basictest,standardtest,businesstest,superiortest,luxurytest,
                                                     cancellation_policytest,TVtest,WiFitest,Air_Conditiontest,
                                                     Wheelchair_accessibletest,Elevatortest,Kitchentest,
                                                     Luggage_dropoff_allowedtest,Long_term_stays_allowedtest,
                                                     Doormantest,Smoking_allowedtest,Suitable_for_eventstest,X24_hour_check_intest),
                                 alpha=0.2,verbose=T,train.fun = train_gam2 ,predict.fun = predict_gam2, split = 1:4200)
predictionintervaltest=exp(cbind(c_predstest[['lo']],c_predstest[['pred']],c_predstest[['up']]))
predictionintervaltest

# 156.6922 188.1357 225.8891

detach(datasetnooutliers)
