# Github Datensatz

# Variablen erstellen
ID <- 1:100
Alter <- rnorm(100, 25, 2)
Studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathe"), size= 100, TRUE, prob = c(0.35, 0.35, 0.20, 0.1))
# Studienfachzuweisung wobei Statistik und Data Science die selbe Wahrscheinlichkeit von 35% haben.Informatik hat eine W'keit von 20% und Mathe eine von 10%

InteresseMathe <- integer(100)
InteresseMathe[which(Studienfach == "Statistik")] <- sample(c(4,5,6), size = length(which(Studienfach == "Statistik")), TRUE) 
# Statistiker haben von 4 bis 6 Interesse an Mathe
InteresseMathe[which(Studienfach == "Data Science")] <- sample(c(3,4,5), size = length(which(Studienfach == "Data Science")), TRUE) 
# Data Scientists haben von 3 bis 5 Interesse an Mathe
InteresseMathe[which(Studienfach == "Informatik")] <- sample(c(4,5,6), size = length(which(Studienfach == "Informatik")), TRUE)
# Informatiker haben in etwa das gleiche Interesse an Mathe wie Statistiker
InteresseMathe[which(Studienfach == "Mathe")] <- sample(c(5,6,7), size = length(which(Studienfach == "Mathe")), TRUE, prob = c(0.2, 0.4, 0.4)) 
# Mathematiker interessieren sich sehr fuer Mathe. Ein kleiner Teil waehlt es jedoch an, weil dieser es nicht besser weiß (das sind hier 20%)

InteresseInformatik <- integer(100)
InteresseInformatik[which(Studienfach == "Statistik")] <- sample(c(3,4,5), size = length(which(Studienfach == "Statistik")), TRUE)
# Statistiker haben von 3 bis 5 Interesse an Programmieren
InteresseInformatik[which(Studienfach == "Data Science")] <- sample(c(4,5,6,7), size = length(which(Studienfach == "Data Science")), TRUE) 
# Data Scientists haben von 4 bis 7 Interesse an Programmieren
InteresseInformatik[which(Studienfach == "Informatik")] <- sample(c(6,7), size = length(which(Studienfach == "Informatik")), TRUE)
# Informatiker lieben programmieren
InteresseInformatik[which(Studienfach == "Mathe")] <- sample(c(3,4,5), size = length(which(Studienfach == "Mathe")), TRUE) 
# Mathematiker haben ein ahnliches Interesse an Programmieren wie Statistiker

MatheLK <- integer(100)
for (i in 1:100)
{
  if(InteresseMathe[i] > 3)
    MatheLK[i] <- 1
  if(InteresseMathe[i] <= 3)
    MatheLK[i] <- 0
}
# Die Menschen, welche ein Interesse an Mathe hoeher als 3 haben werden mit einem Mathe LK zugewiesen und jene welche 3 oder niedriger sind, bekommen keins.
# damit sind die Variablen erstellt, nun werden diese noch in einem Datensatz ueberfuehrt. 

datenStudenten <-data.frame(ID, Alter, Studienfach, InteresseMathe, InteresseProgrammieren = InteresseInformatik, MatheLK)

#Export der Simulationsdaten in die CSV-Datei
#id automatisch inkludiert, daher ausschliessen
write.csv2(datenStudenten[-1] , "daten.csv")