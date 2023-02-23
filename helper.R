getData = function(str = FALSE){
  
  #Struktur des Datensatzes auf der Konsole ausgeben
  if(str == TRUE){
    str(data)
  }
  
  return(read.csv2("daten.csv", header = TRUE))
  
}