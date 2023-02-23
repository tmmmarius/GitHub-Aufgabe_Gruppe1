#Berechnung und Ausgabe von Statistiken fuer metrische Variablen
metrischeVariablen = function(x){
  
  mittelwert = mean(x)
  
  
  return(list("arithmetisches Mittel" = mittelwert))
}
metrischeVariablen(daten$Alter)