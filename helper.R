getData = function(str = FALSE){
  
  #Daten aus der CSV-Datei in data Objekt einlesen
  data = read.csv2("daten.csv", header = TRUE)
  #Ueberschrift der ersten Spalte in "ID" aendern
  colnames(data)[1] = "ID"
  
  #Optional: Struktur des Datensatzes auf der Konsole ausgeben
  if(str == TRUE){
    str(data)
  }
  
  return(data)
  
}
