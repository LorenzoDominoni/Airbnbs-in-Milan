rm(list=ls())
set.seed(27091997)

#load the raw dataset
dataset = read.csv("Airbnb_Milan.csv")



#### FUNZIONE DISTANZA ####

distanza = function(lat_long1, lat_long2){
  #Calcola la distanza tra due punti dati le coordinate geografiche
  raggio = 6372.795
  latA = lat_long1[1] * pi / 180
  lonA = lat_long1[2] * pi / 180
  latB = lat_long2[1] * pi / 180
  lonB = lat_long2[2] * pi / 180
  dist = as.numeric(raggio * acos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB)))
  return(dist)
}



#### COVARIATA DISTANZA DAL DUOMO ####

#Calcola la distanza di ogni airbnb dal duomo
lat_long=dataset[,c("latitude","longitude")]
piazzaduomo=c(45.464195, 9.189670)
dist_duomo=numeric(nrow(dataset))

pb <- txtProgressBar(min = 1, max = nrow(dataset), initial = 1, char= '*',style = 3)
for (i in 1:nrow(dataset)){ #9322
  dist_duomo[i]=distanza(piazzaduomo,lat_long[i,])
  setTxtProgressBar(pb, i)
}



#### COVARIATA LUOGHI DI INTERESSE ####

#In elenco i 20 principali luoghi di interesse milanesi
duomo=c(45.464195, 9.189670)
cairoli=c(45.468468, 9.181966)
cordusio=c(45.465631, 9.185670)
centrale=c(45.484217, 9.202633)
sansiro=c(45.476508, 9.121967)
sanbabila=c(45.466252, 9.197211)
navigli=c(45.452532, 9.176364)
rhofiera=c(45.521113, 9.089215)
garibaldi=c(45.483442, 9.189148)
tretorri=c(45.476321, 9.156713)
moscova=c(45.478355, 9.185123)
colonne=c(45.458154, 9.181245)
portavenezia=c(45.473607, 9.204124)
ambrogio=c(45.462849, 9.175327)
grazie=c(45.465796, 9.171690)
arco=c(45.476109, 9.171848)
brera=c(45.471646, 9.185664)
scala=c(45.467136, 9.189774)
montenapoleone=c(45.469144, 9.194163)
monumentale=c(45.483833, 9.179391)

places=t(rbind(duomo,cairoli,cordusio,centrale,sansiro,sanbabila,navigli,rhofiera,garibaldi,tretorri,moscova,colonne,portavenezia,ambrogio,grazie,arco,brera,scala,montenapoleone,monumentale))
head(places)

#Creo la matrice con le distanze da tutti i luoghi
dist_place=matrix(nrow=nrow(dataset),ncol=ncol(places)) #9322x20
pb <- txtProgressBar(min = 1, max = nrow(dataset)*ncol(places), initial = 1, char= '*',style = 3)

for(j in 1:nrow(dataset)){ #9322
  for (i in 1:ncol(places)){ #20
    dist_place[j,i]=distanza(places[,i],lat_long[j,])
  }
  setTxtProgressBar(pb, i*j)
}

dist_place=as.data.frame(dist_place)
colnames(dist_place)=colnames(places)

#Do un limite minimo alla distanza da un luogo di interesse per evitare successive divisioni per numeri quasi zero
pb <- txtProgressBar(min = 1, max = nrow(dataset)*ncol(places), initial = 1, char= '*',style = 3)
for(i in 1:nrow(dist_place)){
  for (j in 1:ncol(dist_place)){
    if(dist_place[i,j]<0.05)
      dist_place[i,j]=0.05
  }
  setTxtProgressBar(pb, i*j)
}

#Do un peso a ciascuna attrazione a seconda della sua importanza
weighted_dist=dist_place
weighted_dist[,"duomo"]=dist_place[,"duomo"]/400
weighted_dist[,"cairoli"]=dist_place[,"cairoli"]/250
weighted_dist[,"navigli"]=dist_place[,"navigli"]/140
weighted_dist[,"brera"]=dist_place[,"brera"]/130
weighted_dist[,"cordusio"]=dist_place[,"cordusio"]/100
weighted_dist[,"centrale"]=dist_place[,"centrale"]/50
weighted_dist[,"sansiro"]=dist_place[,"sansiro"]/20
weighted_dist[,"sanbabila"]=dist_place[,"sanbabila"]/100
weighted_dist[,"rhofiera"]=dist_place[,"rhofiera"]/1
weighted_dist[,"garibaldi"]=dist_place[,"garibaldi"]/80
weighted_dist[,"tretorri"]=dist_place[,"tretorri"]/60
weighted_dist[,"moscova"]=dist_place[,"moscova"]/100
weighted_dist[,"colonne"]=dist_place[,"colonne"]/70
weighted_dist[,"portavenezia"]=dist_place[,"portavenezia"]/100
weighted_dist[,"ambrogio"]=dist_place[,"ambrogio"]/100
weighted_dist[,"grazie"]=dist_place[,"grazie"]/50
weighted_dist[,"arco"]=dist_place[,"arco"]/50
weighted_dist[,"scala"]=dist_place[,"scala"]/100
weighted_dist[,"montenapoleone"]=dist_place[,"montenapoleone"]/220
weighted_dist[,"monumentale"]=dist_place[,"monumentale"]/20

#Trovo per ogni airbnb le distanze dei 5 luoghi di interesse più vicini per "distanza pesata", 
#sommo i reciproci (stima più accurata dell'importanza della location),
#porto i risultati in scala logaritmica (più utile per visualizzazione).

#Il risultato è una variabile interpretabile come uno score dell'appetibilità della zona da parte di un turista (la scala non ha senso fisico)

top5=numeric(nrow(weighted_dist))
pb <- txtProgressBar(min = 1, max = nrow(weighted_dist)*5, initial = 1, char= '*',style = 3)
for (i in 1:nrow(weighted_dist)){
  temp=sort(weighted_dist[i,])
  for(j in 1:5){
    top5[i]=top5[i]+1/temp[[j]]
  }
  setTxtProgressBar(pb, i*j)
}
top5=log(top5)

#Visualizzo la nuova variabile per capire se ha senso
#Ovviamente il centro è la location migliore, tuttavia le zone a stessa distanza dal centro possono avere diversa importanza
#è il comportamento che vogliamo
dataperplot=cbind(lat_long,top5)
rbPal <- colorRampPalette(c('blue','yellow'))
dataperplot$Col <- rbPal(50)[as.numeric(cut(dataperplot$top5,breaks = 50))]
plot(dataperplot$longitude,dataperplot$latitude,col = dataperplot$Col,asp = 1)



#### COVARIATA DISTANZA DALLA METRO ####  
#Carico un dataset con le coordinate delle stazioni
metro = read.csv('metro.csv')
metro = metro[,-c(1,2,7)]
nomi = metro$nome
metro_coord = cbind(metro$LAT_Y_4326,metro$LONG_X_4326)
colnames(metro_coord)=c('lat','long')

#Creo la matrice delle distanze degli airbnb dalle metro
dist_metro=matrix(0,nrow=nrow(dataset),ncol=nrow(metro)) #9322x20
pb <- txtProgressBar(min = 1, max = nrow(dataset)*nrow(metro), initial = 1, char= 'O',style = 3)

for(j in 1:nrow(dataset)){ #9322
  for (i in 1:nrow(metro)){ #110
    dist_metro[j,i]=distanza(metro_coord[i,],lat_long[j,])
  }
  setTxtProgressBar(pb, i*j)
}

#Trovo per ogni airbnb la metro più vicina
dist_fermata = rep(0,nrow(dataset))
pb <- txtProgressBar(min = 1, max = nrow(dataset), initial = 1, char= 'O',style = 3)

for(j in 1:nrow(dataset)){ #9322
    dist_fermata[j]=min(dist_metro[j,])
    setTxtProgressBar(pb, j)
}
summary(dist_fermata)

#Creo anche una variabile che indica se c'è una metro a distanza minore di 200 metri
metro_vicina = dist_fermata<0.25
sum(metro_vicina)



#Metto le nuove variabili nel dataset
dataset[,'duomo']=dist_duomo
dataset[,'distance']=top5
dataset[,'fermata']=dist_fermata
dataset[,'metrocheck']=metro_vicina
save(dataset,file='Dataset.RData')



