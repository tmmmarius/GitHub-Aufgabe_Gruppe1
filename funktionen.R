# 3a) deskriptive Statistiken fuer metrische Variablen
#Input: numerischer Vektor (kein NA handling); Rueckgabe: Liste 
#Optional kann ueber das Argument graphic = TRUE ein Boxplot ausgegeben werden. 
#Ueber den cut Paramter kann fuer das getrimmte arithemtische Mittel die zu 
#ignorierenden Werte eingestellt werden. Standard ist: 0.1. Das Bedeutet: 
#Werte unterhalb des 10% Quantil und oberhalb des 90% Qunatil werden ignoriert
metrischeVariablen = function(x, graphic = FALSE, cut = 0.1){
  
  metrischeStatisitken = list(
  
    ##Lageparameter
    
    "arithmetisches_Mittel" = mean(x),
    
    "getrimmtes artihmetisches Mittel" = mean(x, trim = cut),
    
    "Median" = median(x),
    
    "Haeufigkeitstabelle/ Modus" = table(x),
    
  )

  return(metrischeStatisitken)
  
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

####################

# 3f 

VisualizierungStudienfaecher <- function(daten)
{
  # daten muss kategoriale daten enthalten und ein Vektor sein
  # es gibt zwei plots
  par(mfrow = c(1,2))
  
  # absolute Haeufigkeit
  H <- data.frame(table(daten))
  barplot(height = H$Freq, names.arg = H$daten, ylim = c(0,length(daten)), xlab = "Studienfach", ylab = "absolute Haeufigkeit")
  
  # relative Haufigkeit
  relativerAnteile <- table(daten)/length(daten)
  P <- data.frame(relativerAnteile)
  barplot(height = P$Freq, names.arg = P$daten, ylim = c(0,1), xlab = "Studienfach", ylab = "relative Haeufigkeit")
}
