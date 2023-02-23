#Testdaten erzeugen 
simulationData = data.frame("Beobachtungen"= 201:300)

#Export der Simulationsdaten in eine CSV-Datei
#id automatisch inkludiert
write.csv2(simulationData ,"daten.csv")
