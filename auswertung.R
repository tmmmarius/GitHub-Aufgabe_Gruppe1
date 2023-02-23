#Enthaelt Funktionen fuer die statistische Auswertung
source("helper.R") #Hilfsfunktionen

##Daten laden
#Zugriff auf die Variablen wie folgt: daten$VARIABLENNAME
#Durch setzen des Arguments str = TRUE wird zusaetzlich die Struktur beim Laden  
#auf der Konsole ausgegeben
daten = getData(str = TRUE) 


#Berechnung und Ausgabe von Statistiken fuer metrische Variablen
metrischeVariablen = function(x){
  
  mittelwert = mean(x)
  
  
  return(list("arithmetisches Mittel" = mittelwert))
}
metrischeVariablen(daten$Alter)