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

# 3 d) Zusammenhang zwischen metrischer und dichotomer Variable
md <- function(x,y){
  if(sum(y != 0 & y != 1) != 0){return("y muss dichotom sein")}
  
  ja <- mean(x[which(y == 1)])
  nein <- mean(x[which(y == 0)])
  return(list("Durchschnittsalter von Leuten, die Mathe-LK hatten" = ja, 
              "Durchschnittsalter von Leuten, die kein Mathe-LK hatten" = nein))
}

# 3 c) Zusammenhang zwischen zwei kategorialen Variablen
zk <- function(Studienfach, MatheLK){
  st1 <- sum(Studienfach[which(MatheLK == 1)] == "Statistik")
  st0 <- sum(Studienfach[which(MatheLK == 0)] == "Statistik")
  ds1 <- sum(Studienfach[which(MatheLK == 1)] == "Data Science")
  ds0 <- sum(Studienfach[which(MatheLK == 0)] == "Data Science")
  m1 <- sum(Studienfach[which(MatheLK == 1)] == "Mathe")
  m0 <- sum(Studienfach[which(MatheLK == 0)] == "Mathe")
  i1 <- sum(Studienfach[which(MatheLK == 1)] == "Informatik")
  i0 <- sum(Studienfach[which(MatheLK == 1)] == "Informatik")
  mx <- matrix(c(st1, st0, ds1, ds0, m1, m0, i1, i0), nrow = 2)
  barplot(mx, beside = TRUE, names.arg = c("Statistik", "Data Science","Mathe", "Informatik"),
          main = "Verteilung der Studienfaecher von Studenten mit und ohne Mathe-LK", col = c("black", "white"), xlab = "Studienfaecher", ylab = "Absolute Haeufigkeit")
  legend("topleft", c("Mit Mathe-LK","Ohne Mathe-LK"), fill = c("black", "white"), cex = 0.8)  
}
