#############################
#  TEST PERMUTAZIONALI GAM  #
#############################
# Questo codice è una base che è stata utilizzata per i test permutazionali sui coefficienti dei regressori nei diversi modelli GAM 
# che sono stati implementati. 


# Dati da completare:
# RIGA 30: importare il dataset
# RIGA 38 modellocompleto: il modello con tutti i regressori 
# RIGA 41 B: iterazioni
# RIGA 50 n_t: numero di parametri lineari che vuoi testare
# RIGA 51 n_s: numero di parametri smoothed che vuoi testare
# RIGHE 50-55  prestare attenzione agli indici
# RIGHE 64-? modelli: lista dei modelli sotto H0
# RIGA 100 modello completo permutato per i test lineari (è uguale al modello inserito alla riga 38)
# RIGA 131 modello completo permutato per i test smoothed (è uguale al modello inserito alla riga 38)


#---------------------------#
# Librerie, dataset e seed  #
#---------------------------#
rm(list=ls())
set.seed(27091997)
library(mgcv)
library(car)
library(pbapply)

# load workspace con il dataset
load("Dataset_for_Regression.RData")
attach(datasetnooutliers)



#---------------------------#
# Modello completo sotto H1 #
#---------------------------#
modellocompleto = gam(response ~ dati)

# Set del numero di regressori lineari, smoothed, numero delle permutazioni
B = 1000
n_t = ??? #termini lineari
n_s = ??? #termini smooth
T0_lin = rep(0,n_t)
T0_s = rep(0,n_s)

# Prendo i relativi T0_stat associati ai singoli coefficienti nel modello completo.
# Bisogna prestare attenzione agli indici delle righe di p.table e s.table perché rispettano l'ordine con cui sono stati scritti i regressori

if(n_t){T0_lin = (n_t>0)*abs(summary(modellocompleto)$p.table[2:(n_t+1),3])}
if(n_s){T0_s = abs(summary(modellocompleto)$s.table[,3])} 

# Check se i nomi delle varibili in T0_lin e T0_s sono quelli che si vogliono testare
names(T0_lin)
names(T0_s)




#---------------------------#
#     Modelli sotto H0      #
#---------------------------#
# Sono gli n_t+n_s (controllare) modelli diversi, ognuno senza il reggressore di cui si vuole testare la significatività
{
  numeromodelli = n_t+n_s
  listamodelli = list()
  i = 1
  #covariata i
  listamodelli[[i]]= gam(modello senza il regressore i)
  i = i+1
  #covariata j
  listamodelli[[i]]= gam(modello senza il regressore j)
  
  
}

pval = rep(0, length=numeromodelli)
T_H0 = matrix(0,nrow=B,ncol=numeromodelli)




#---------------------------#
#  Test regressori lineari  #
#---------------------------#
for (i in 1:n_t){
  print(paste0(i,': ',names(T0_lin)[i]))
  # Modello senza la covariata i
  gam.H0 = listamodelli[[i]] 
  residui.H0 <- gam.H0$residuals #residui del modello senza il regressore i
  
  T0=abs(T0_lin[i]) #T stat del coefficiente del regressore i
  
  wrapper=function(){
    permutazione <- sample(length(y)) #creo una permutazione di indici
    residui.H0.perm <- residui.H0[permutazione] #permuto i residui
    Y.perm.H0 <- gam.H0$fitted + residui.H0.perm #permuto la risposta
    
    #MODELLO COMPLETO CON RISPOSTA PERMUTED
    gam.perm = gam(Y.perm.H0 ~ ???)
    
    # Prestare attenzione agli indici delle righe di p.table 
    return(abs(summary(gam.perm)$p.table[i+1,3]))#T_stat 
    
  }
  
  T_H0[,i]=pbreplicate(B,wrapper(),simplify = 'vector')
  pval[i]=sum(T_H0[,i]>=T0)/B
  print(pval[i])
}



#---------------------------#
#  Test regressori smoothed #
#---------------------------#
for (i in 1:n_s){
  indice_s = n_t+i
  print(paste0(indice_s,': ',names(T0_s)[i]))
  gam.H0 = listamodelli[[indice_s]] #modello senza la covariata i
  residui.H0 <- gam.H0$residuals #residui del modello senza la covariata i
  
  T0=abs(T0_s[i]) #t stat del coefficiente della covariata i
  
  wrapper=function(){
    permutazione <- sample(length(daily_price))#creo una permutazione di indici
    residui.H0.perm <- residui.H0[permutazione] #permuto i residuo
    Y.perm.H0 <- gam.H0$fitted + residui.H0.perm #permuto la risposta
    
    #MODELLO COMPLETO CON RISPOSTA PERMUTED
    gam.perm = gam(Y.perm.H0 ~ ???)
    
    # Prestare attenzione agli indici delle righe di s.table 
    return(abs(summary(gam.perm)$s.table[i,3]))#T_stat 
  }
  
  T_H0[,indice_s]=pbreplicate(B,wrapper(),simplify = 'vector')
  
  pval[indice_s]=sum(T_H0[,indice_s]>=T0)/B
  print(pval[indice_s])
}
