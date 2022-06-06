#___________________________________________5________________________________
#Zadanie_5.1

#Przebadano 200 losowo wybranych 5-sekundowych okresów pracy centrali telefonicznej. R
#ejestrowano liczbê zg³oszeñ. 
#Wyniki s¹ zawarte w pliku Centrala.RData. 
#Wyznacz (trzema metodami) przedzia³ ufnoœci dla parametru rozk³adu teoretycznego.

#Jest to zmienna dyskretna, iloœciowa, przyjmuj¹ca wartoœci ca³kowite od 0 do 5.

load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
library(EnvStats)
estimation1 <- function(x, conf_level = 0.95) {
  estimate <- epois(
    x,
    ci = TRUE,
    ci.method = "pearson",
    conf.level = conf_level
  )$interval$limits
  return(c(estimate))
}
estimation1(Centrala$Liczba)
estimation2 <- function(x, conf_level = 0.95) {
  estimate <- epois(
    x,
    ci = TRUE,
    ci.method = "pearson.hartley.approx",
    conf.level = conf_level
  )$interval$limits
  return(c(estimate))
}
estimation2(Centrala$Liczba)
estimation3 <- function(x, conf_level = 0.95) {
  estimate <- epois(
    x,
    ci = TRUE,
    ci.method = "normal.approx",
    conf.level = conf_level
  )$interval$limits
  return(c(estimate))
}
estimation3(Centrala$Liczba)

#Zadanie_5.2
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiarów czasu bezawaryjnej pracy danego urz¹dzenia (w godzinach).
    #1.Wyznacz przedzia³ ufnoœci dla parametru rozk³adu teoretycznego.
    #2.Oblicz wartoœæ ENW i granice przedzia³u ufnoœci dla wartoœci oczekiwanej i wariancji rozk³adu teoretycznego.

data <- read.delim(url("http://ls.home.amu.edu.pl/data_sets/awarie.txt"), header =
                     FALSE)$V1
install.packages("fitdistrplus")
install.packages("logspline")
library(fitdistrplus)
library(logspline)
descdist(data, discrete = FALSE)
fit.exp <- fitdist(data/10, "exp")
fit.beta <- fitdist(data/10000, "beta")
plot(fit.exp)
plot(fit.beta)
library(EnvStats)
conf_intervals <- eexp(
  data,
  ci = TRUE,
  ci.method = "exact",
  conf.level = 0.95)$interval$limits[c("UCL", "LCL")]
expected_value <- conf_intervals ** -1
expected_value
variation <- conf_intervals ** -2
variation

#Zadanie_5.3

#Niech X=(X1,…,Xn)^??? bêdzie prób¹ prost¹ z populacji o rozk³adzie Rayleigha R(??),??>0.

    #1.Napisz funkcjê median_cint(), która implementuje nastêpuj¹cy przybli¿ony przedzia³ ufno9œci dla mediany ???(??*ln*2)
      #tego rozk³adu:

      #spójrz do pdf

      #gdzie z(ß) oznacza kwantyl rzêdu ß z rozk³adu normalnego N(0,1). 
      #Funkcja ta powinna mieæ dwa argumenty: x - wektor zawieraj¹cy dane, conf_level - poziom ufnoœci. 
      #Funkcja zwraca obiekt typu list klasy confint o nastêpuj¹cych elementach: title - nazwa estymowanej funkcji parametrycznej, est - wartoœæ ENW funkcji parametrycznej

      ##spójrz do pdf

      #l - lewy kraniec przedzia³u ufnoœci, r - prawy kraniec przedzia³u ufnoœci, conf_level - poziom ufnoœci.

    #2.Nastêpuj¹ce dane to pomiary œredniej szybkoœci wiatru w odstêpach 15 minutowych odnotowane wokó³ nowo powstaj¹cej elektrowni wiatrowej:
        #0.9 6.2 2.1 4.1 7.3
        #1.0 4.6 6.4 3.8 5.0
        #2.7 9.2 5.9 7.4 3.0
        #4.9 8.2 5.0 1.2 10.1
        #12.2 2.8 5.9 8.2 0.5

    #Teoretyczny rozk³ad œredniej szybkoœci wiatru to rozk³ad Rayleigha  R(??),??> 0. 
    #U¿ywaj¹c funkcji median_cint(), oblicz wartoœæ ENW i krañce 95 % przedzia³u ufnoœci dla mediany œredniej szybkoœci wiatru. 
    #Wskazówka: Przed wywo³aniem funkcji median_cint(), najpierw za³aduj nastêpuj¹ce funkcje przeci¹¿one print() i summary():


install.packages("SciViews")
library(SciViews)
print.confint <- function(x) {
  cat(x$conf_level * 100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
}
summary.confint <- function(x) {
  cat("\n", "Confidence interval of", x$title, "\n", "\n")
  cat(x$conf_level * 100, "percent confidence interval:", "\n")
  cat(x$l, " ", x$r, "\n")
  cat("sample estimate", "\n")
  cat(x$est, "\n")
}
median_cint <- function(x, conf_level = 0.95) {
  value = 1 - (1 - conf_level) / 2
  z = qnorm(value, mean = 0, sd = 1)
  LCL = sqrt(ln(2) * mean(x ** 2) * (1 - z / sqrt(length(x))))
  UCL = sqrt(ln(2) * mean(x ** 2) * (1 + z / sqrt(length(x))))
  ENW = mean(cbind(LCL, UCL))
  result = list(
    title = "mediana",
    est = ENW,
    l = LCL,
    r = UCL,
    conf_level = conf_level
  )
  class(result) <- "confint"
  return(result)
}
data = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0,
         1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)
print(median_cint(data))
summary(median_cint(data))

#Zadanie_5.4

# Dla danego wektora obserwacji i poziomu ufnoœci napisz funkcjê okreœlaj¹c¹ granice przedzia³u ufnoœci na poziomie ufnoœci 1-??,?????(0,1)
#dla wartoœci oczekiwanej w rozk³adzie normalnym. Domyœlny poziom ufnoœci powinien wynosiæ 0,95. 
#Nastêpnie przeprowadŸ symulacje (z liczb¹ powtórzeñ nr = 1000) sprawdzaj¹c prawdopodobieñstwo pokrycia tego przedzia³u ufnoœci (tj. prawdopodobieñstwo, ¿e ten przedzia³ ufnoœci zawiera wartoœæ oczekiwan¹) dla rozk³adów  
#N(1,3),??^2(3)i Ex(3)osobno. 
#Rozwa¿ liczby obserwacji n=10,50,100. 
#Zinterpretuj wyniki. Wskazówka: Symulacja powinna przebiegaæ wed³ug nastêpuj¹cych kroków:

    #1,Przyjmij poziom istotnoœci,n,nr, rozk³ad generowanych danych oraz temp = 0.
    #2.Wygeneruj n obserwacji z zadanego rozk³adu.
    #3.Wyznacz granice przedzia³u ufnoœci dla danych wygenerowanych w kroku 2.
    #4.Jeœli teoretyczna wartoœæ oczekiwana nale¿y do przedzia³u otrzymanego w kroku 3, zwiêksz temp o jeden.
    #5.Powtórz kroki 2-4 nr razy.
    #6,Wyznacz temp / nr.


conf_nd <- function(x, conf_level = 0.95) {
  result <- enorm(
    x,
    ci = TRUE,
    ci.type = "two-sided",
    ci.method = "exact",
    conf.level = conf_level,
    ci.param = "mean")
  return(result)
}
calculate <- function(func, n) {
  parameters = conf_nd(data)$parameters
  mean = parameters[1]
  sd = parameters[2]
  nr <- 1000
  temp <- 0
  for (i in 1:nr) {
    if (func == "rnorm") {
      observations <- rnorm(n, mean, sd)
    }
    if (func == "rchisq") {
      observations <- rchisq(n, mean)
    }
    if (func == "rexp") {
      observations <- rexp(n, 1/mean)
    }
    limits <- conf_nd(observations)$interval$limits
    if (mean >= limits[1] && mean <= limits[2]) {
      temp <- temp + 1
    }
  }
  return(temp/nr)
}
for (n in c(10, 50, 100)) {
  cat("n = ", n, "\n")
  print(calculate("rnorm", n))
  print(calculate("rchisq", n))
  print(calculate("rexp", n))
}

    

