#Auswertung der Daten
source("helper.R") #Hilfsfunktionen
source("funktionen.R")

##Daten laden
#Zugriff auf die Variablen wie folgt: daten$VARIABLENNAME
#Durch setzen des Arguments str = TRUE wird zusaetzlich die Struktur beim Laden  
#auf der Konsole ausgegeben
daten = getData(str = TRUE) 

metrischeVariablen(daten$Alter)




