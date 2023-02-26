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
    
    
    ##Streuungsparameter/ -masse
    
    "Minimum" = min(x),
    "Maximum" = max(x),
    "Spannweite" = max(x) - min(x),
    "Varianz" = var(x),
    "Standardabweichung" = sd(x),
    "Variationskoeffizient" = sd(x)/mean(x),
    
    
    ##Quantile
    
    "Quantile" = quantile(x, probs = c(0.25, 0.75)),
    "Interquartilsabstand" = IQR(x)
    
  )
  
  #Optional: Boxplot ausgeben
  if(graphic == TRUE) boxplot(x, 
                              horizontal = TRUE, #Horizontal anzeigen
                              outline = TRUE, #Ausreisser als Whisker
                              xlab = names(x) #Achsenbeschriftung
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


# 3b)
# Hauefigkeitstabelle
deskfun <- function(x){
  list("Haeufigkeitstabelle" = table(x) / length(x)) 
}

# Modus
deskfun2 <- getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# 3c)
# Barplot für Verteilung der Studienfaecher von Studenten mit und ohne Mathe-LK
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
