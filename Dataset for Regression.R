rm(list=ls())
set.seed(27091997)

#load the raw dataset
load("Dataset.RData")

##### CREAZIONE DATASET  #####
#Prima di tutto rimuoviamo alcune osservazioni dal dataset per 3 principali motivi
#1. Anche se non tutte queste osservazioni sono considerabili outlier, questi dati esulano dal nostro scopo di trovare le migliori misure da adottare per scegliere o affittare un appartamento (ad esempio se sei un host che non risponde mai alle richieste di affitto non ti consideriamo nella nostra indagine)
#2. Abbiamo molte osservazioni e i dati hanno molte code lunghe: abbiamo la possibilità di rimuovere dati che probabilmente possono invalidare l'analisi
#3. Alcuni dati su airbnb non sono corretti (Esempio: alcuni mettono un prezzo di 3000 o più euro a notte per modalità di affitto che non vogliono proporre)

datasetnooutliers=dataset
soglia = 1500 
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price < soglia),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$daily_price>20),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$number_of_reviews >=3),]
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$host_response_rate > 49),]

attach(datasetnooutliers)


#ATTENZIONE DA QUI FINO A riga 119 EDO COMMENTA IL CODICE

#### COVARIATA METRATURA ####  
# L'idea e quella di stimare la metratura di una casa a partire dai dati a disposione.
# Infatti in Italia abbiamo per legge alcune Indicazioni sulla costruzione delle stanze da letto, bagni e cucina.
# Il numero di bagni e di letti e' gia presente nel database. Di conseguenda se riuscissimo a stimare il numero e la tipologia 
# di stanze da letto saremmo in grado di completare la nostra stima.

# In primo luogo cerco di capire quante persone effettivamente dormano su di un letto. Infatti le variabili accommodates e 
# beds, che intuitivamente dovrebbero essere uguali, sono spesso diverse in quanto vi puo essere (oltre ai letti matrimoniali)
# che il posto letto sia un divano o un letto da campeggio i quali sono solo usufruibili di notte e dunque non influenzano troppo
# sulla stima della metratura

for (i in 1:dim(datasetnooutliers)[1])
{
  # Se l'alloggio non ha letti, non lo considero per questa variabile e lo elimino dal database
  # In totale sono 2 osservazioni su 9322
  if(datasetnooutliers$beds[i] == 0)
  {
    datasetnooutliers = datasetnooutliers[-c(i),]
    next()
  }
  
  # Se il Nr Ospiti > 2*Nr letti (ossia 1 dorme sul divano) rimuovo 1 persona dagli accommodates fino a quando non sono presenti
  # almeno solo letti matrimoniali
  if ( datasetnooutliers$accommodates[i] / datasetnooutliers$beds[i] > 2)
  {
    persone = datasetnooutliers$accommodates[i]
    letti = datasetnooutliers$beds[i]
    ratio = persone / datasetnooutliers$beds[i]
    while (ratio > 2)
    {
      persone = persone -1
      ratio = persone / datasetnooutliers$beds[i]
      if (persone == 0)
      {
        datasetnooutliers = datasetnooutliers[-c(i),]
        break()
      }
    }
    datasetnooutliers$accommodates[i] = persone
  }
}

# Ho dunque ora un numero di accommmodates che e' sicuramenre associabile ad un quantita di letti finita possibilmente
# identificabile con letti singoli e matrimoniali

for (i in 1:dim(datasetnooutliers)[1])
{
  # Se Nr letti > Nr Ospiti, allora sicuramete quelli eccedenti saranno letti singoli, in quanto un letto matrimoniale sarebbe sprecato
  if (datasetnooutliers$beds[i] > datasetnooutliers$accommodates[i])
  {
    datasetnooutliers$singole[i] = datasetnooutliers$accommodates[i]
  }
  # Negli altri casi inzio a sottrarre 1 letto matrimoniale alla volta, fino a quando non no finiscono gli accommodates o
  # rimane solamente 1 accommodates che verra assegnato ad un letto singolo
  else
  {
    persone = datasetnooutliers$accommodates[i]
    camere = datasetnooutliers$beds[i]
    datasetnooutliers$singole[i] = 0
    datasetnooutliers$matrimoniali[i] = 0
    
    while (persone/camere != 1 & persone > 0)
    {
      datasetnooutliers$matrimoniali[i] = datasetnooutliers$matrimoniali[i] + 1
      camere = camere - 1
      persone = persone - 2
    }
    if ( persone < 0)
    {
      persone = persone + 2
    }
    datasetnooutliers$singole[i] = persone
  }
}

# Avendo dunque stimato il numero di stanze da letto, e dunque possibile avere una stima della metratura.
# Si fa riferimento alle indicazioni di legge, ossia:
#   -> Stanza Singola: ca 9m^2
#   -> Stanza Matrimoniale: ca 14m^2
#   -> Cuciona: ca 14m^2
#   -> Bagno: ca 5m^2

datasetnooutliers$metratura = 9*datasetnooutliers$singole + 14* datasetnooutliers$matrimoniali + 14* Kitchen + 5*bathrooms



#### COVARIATA NEW CLEANING E NEW PRICE ####  
# Volendo sfruttare al meglio le careatteristiche del nostro dataset, abbiamo deciso che era necessario uniformare il dataset
# Infatti, per esperienza personale e per riscontro effettivo nel dataset, alcun host includono la cleaning fee nel prezzo, indicandola simbolicamnete poi con 1EUR,
# altri invece tengono le due cose separate.
# Abbiamo deciso di uniformare tutto il dataset nella secondo direzione in quanto ci sembrava piu naturale

# Da notare e' il fatto che la Cleaning fee venga applicata solamente una volta durante il soggiorno mentre il daily_price
# giornalmente

# Di conseguenza siamo andati a ritoccare tutti quelle osservazioni che hanno cleaning fee molto bassa

# L'idea a supporto di cio e' che un soggiorno medio presso un AirBNB generico si attesta intorno ai 3.6 giorni
# In questo modo e stato possibile infatti trasferire parte del costo giornaliero alle spese di pulizia effettive per quelle
# osservazioni che non avevo segnalata


# In primo luogo ossevo qual e' la media di pulizia per m^2 per ogni quartiere
clean_quartiere  = replicate(9,0)
for (i in 1:9)
{
  
  prova = datasetnooutliers[which(datasetnooutliers$cleaning_fee > 11 & datasetnooutliers$neighbourhood_cleansed == i) ,] 
  clean_quartiere[i] = mean(prova$cleaning_fee / prova$metratura)
}

for (i in 1:dim(datasetnooutliers)[1])
{
  if (datasetnooutliers$cleaning_fee[i] <= 10)
  {
    # A questo punto se l'osservazione e una di quelle senza cleaning fee, la stimo a secondo della metratura dell appartamento
    # e della media di pulizia del quartiere per m^2 ( lidea di base e che pulire in centro abbia un costo maggiore he pulire in 
    # periferia proprio per gli standard diversi del quartiere)
    datasetnooutliers$new_cleaning[i] = clean_quartiere[ datasetnooutliers$neighbourhood_cleansed[i] ] * datasetnooutliers$metratura
    # Avendo stimato la nuova spesa d pulizia, definisco il nuovo prezzo toglendo dal prezzo gonfiato dalla cleaning fee
    # la nuovo cleaning fee spalmata su 3.6 giorni che e' il soggiorno medio
    datasetnooutliers$new_price[i] = datasetnooutliers$daily_price[i] - datasetnooutliers$new_cleaning[i]/3.6
  }
  else
  {
    datasetnooutliers$new_cleaning[i] = datasetnooutliers$cleaning_fee[i]
    datasetnooutliers$new_price[i] = datasetnooutliers$daily_price[i]
  }
  
  # Di seguito correggiamo leccezione in cui il costo di pulizi sia maggiore dell costo giornaliero dell appartamento sempre 
  # seguendo la stessa filosofia usata in precedenza
  if(datasetnooutliers$new_price[i] < datasetnooutliers$new_cleaning[i])
  {
    datasetnooutliers$new_price[i] = datasetnooutliers$new_price[i] + datasetnooutliers$new_cleaning[i]/2/3.6
    datasetnooutliers$new_cleaning[i] = datasetnooutliers$new_cleaning[i]/2
    
  }
  
  #Un altra eccezzione da considerare e' quella in cui il costo di pulizia per ospite e' spropositato rispetto al costo della casa stessa
  if( datasetnooutliers$new_cleaning[i] > 0.7 *datasetnooutliers$new_price[i] & datasetnooutliers$new_cleaning[i]/datasetnooutliers$accommodates[i] > 20)
  {
    datasetnooutliers$new_price[i] = datasetnooutliers$new_price[i] + datasetnooutliers$new_cleaning[i]/2/3.6
    datasetnooutliers$new_cleaning[i] = datasetnooutliers$new_cleaning[i]/2
  }
  
  
}

# Avendo fatto questo procedimento, rimuviamo le poche osservazioni che ora hanno un prezzo troppo basso
datasetnooutliers=datasetnooutliers[which(datasetnooutliers$new_price > 10),]


#### COVARIATE FACILITIES RAGGRUPPATE #### 
#Abbiamo creato nuove variabili per tenere conto delle caratteristiche della casa che si erano rilevate significative secondo i test permutazionali contemporaneamente (prima c'erano una trentina di variabili in più)

datasetnooutliers[,"facilities"] = cancellation_policy+TV + WiFi + Air_Condition + Wheelchair_accessible + Elevator + Kitchen +Luggage_dropoff_allowed + Long_term_stays_allowed + Doorman + Smoking_allowed + Suitable_for_events + X24_hour_check_in
datasetnooutliers[,"host"]= host_is_superhost + host_identity_verified + host_location
datasetnooutliers[,"facilities_casa"] = TV + WiFi + Air_Condition + Wheelchair_accessible + Elevator + Kitchen



#### COVARIATA LUSSO #### 
# Questa covariata è probabilmente la più importante: è creata ad hoc sulla base del lusso che stimiamo per una casa.
# Purtroppo non abbiamo nei nostri dati questa variabili come ovvio ma crediamo sia di estrema importanza e spieghi una buona parte dei dati.
# é molto utile per fare predizioni con case non presenti nel dataset di base: se ci mettiamo nei panni dell'host ma anche del guest possiamo stimare il "lusso" della casa per esempio tramite le foto.
# é costruita dividendo le case in sezioni in base al prezzo per persona.
datasetnooutliers$basic = (datasetnooutliers$new_price/datasetnooutliers$accommodates < 20) # meno di 20 euro a notte a persona
datasetnooutliers$standard = (datasetnooutliers$new_price/datasetnooutliers$accommodates >= 20 & datasetnooutliers$new_price/datasetnooutliers$accommodates < 30) # più di 20 e meno di 30 euro a notte a persona
datasetnooutliers$business = (datasetnooutliers$new_price/datasetnooutliers$accommodates >= 30 & datasetnooutliers$new_price/datasetnooutliers$accommodates < 40) # più di 30 e meno di 40 euro a notte a persona
datasetnooutliers$superior = (datasetnooutliers$new_price/datasetnooutliers$accommodates >= 40 & datasetnooutliers$new_price/datasetnooutliers$accommodates < 80) # più di 40 e meno di 80 euro a notte a persona
datasetnooutliers$luxury = (datasetnooutliers$new_price/datasetnooutliers$accommodates >= 80) # più di 80 euro a notte a persona



#### COVARIATE LOGARITMICHE PER ALCUNE DISTRIBUZIONI MOLTO ASIMMETRICHE #### 
datasetnooutliers$log_price = log(daily_price)
datasetnooutliers$log_reviews = log(number_of_reviews)
datasetnooutliers$log_listings = log(host_total_listings_count+1)
datasetnooutliers$log_cleaning = log(cleaning_fee+1)
datasetnooutliers$new_log_price = log(datasetnooutliers$new_price)
datasetnooutliers$new_log_clean = log(datasetnooutliers$new_cleaning)



save(datasetnooutliers,file='Dataset_for_Regression.RData')
detach(datasetnooutliers)
