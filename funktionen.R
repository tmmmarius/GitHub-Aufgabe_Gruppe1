#Berechnung und Ausgabe von Statistiken fuer metrische Variablen
metrischeVariablen = function(x){
  
  mittelwert = mean(x)
  
  
  return(list("arithmetisches Mittel" = mittelwert))
}
metrischeVariablen(daten$Alter)

# 3e) Quantilbasiert kategorisiert von ordinal skalierter Variable
qk <- function(x){
  Kategorie <- 1:length(x)
  Kategorie[which(x < 3)] <- "niedrig"
  Kategorie[which(x > 2 & x < 6)] <- "mittel"
  Kategorie[which(x > 5)] <- "hoch"
  
  return(list("Kategorisiert" = Kategorie))
}
