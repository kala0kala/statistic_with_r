#___________________________________________5________________________________
#Zadanie_5.1

#Przebadano 200 losowo wybranych 5-sekundowych okres�w pracy centrali telefonicznej. R
#ejestrowano liczb� zg�osze�. 
#Wyniki s� zawarte w pliku Centrala.RData. 
#Wyznacz (trzema metodami) przedzia� ufno�ci dla parametru rozk�adu teoretycznego.

#Jest to zmienna dyskretna, ilo�ciowa, przyjmuj�ca warto�ci ca�kowite od 0 do 5.

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
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiar�w czasu bezawaryjnej pracy danego urz�dzenia (w godzinach).
    #1.Wyznacz przedzia� ufno�ci dla parametru rozk�adu teoretycznego.
    #2.Oblicz warto�� ENW i granice przedzia�u ufno�ci dla warto�ci oczekiwanej i wariancji rozk�adu teoretycznego.

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

#Niech X=(X1,�,Xn)^??? b�dzie pr�b� prost� z populacji o rozk�adzie Rayleigha R(??),??>0.

    #1.Napisz funkcj� median_cint(), kt�ra implementuje nast�puj�cy przybli�ony przedzia� ufno9�ci dla mediany ???(??*ln*2)
      #tego rozk�adu:

      #sp�jrz do pdf

      #gdzie z(�) oznacza kwantyl rz�du � z rozk�adu normalnego N(0,1). 
      #Funkcja ta powinna mie� dwa argumenty: x - wektor zawieraj�cy dane, conf_level - poziom ufno�ci. 
      #Funkcja zwraca obiekt typu list klasy confint o nast�puj�cych elementach: title - nazwa estymowanej funkcji parametrycznej, est - warto�� ENW funkcji parametrycznej

      ##sp�jrz do pdf

      #l - lewy kraniec przedzia�u ufno�ci, r - prawy kraniec przedzia�u ufno�ci, conf_level - poziom ufno�ci.

    #2.Nast�puj�ce dane to pomiary �redniej szybko�ci wiatru w odst�pach 15 minutowych odnotowane wok� nowo powstaj�cej elektrowni wiatrowej:
        #0.9 6.2 2.1 4.1 7.3
        #1.0 4.6 6.4 3.8 5.0
        #2.7 9.2 5.9 7.4 3.0
        #4.9 8.2 5.0 1.2 10.1
        #12.2 2.8 5.9 8.2 0.5

    #Teoretyczny rozk�ad �redniej szybko�ci wiatru to rozk�ad Rayleigha  R(??),??> 0. 
    #U�ywaj�c funkcji median_cint(), oblicz warto�� ENW i kra�ce 95 % przedzia�u ufno�ci dla mediany �redniej szybko�ci wiatru. 
    #Wskaz�wka: Przed wywo�aniem funkcji median_cint(), najpierw za�aduj nast�puj�ce funkcje przeci��one print() i summary():


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

# Dla danego wektora obserwacji i poziomu ufno�ci napisz funkcj� okre�laj�c� granice przedzia�u ufno�ci na poziomie ufno�ci 1-??,?????(0,1)
#dla warto�ci oczekiwanej w rozk�adzie normalnym. Domy�lny poziom ufno�ci powinien wynosi� 0,95. 
#Nast�pnie przeprowad� symulacje (z liczb� powt�rze� nr = 1000) sprawdzaj�c prawdopodobie�stwo pokrycia tego przedzia�u ufno�ci (tj. prawdopodobie�stwo, �e ten przedzia� ufno�ci zawiera warto�� oczekiwan�) dla rozk�ad�w  
#N(1,3),??^2(3)i Ex(3)osobno. 
#Rozwa� liczby obserwacji n=10,50,100. 
#Zinterpretuj wyniki. Wskaz�wka: Symulacja powinna przebiega� wed�ug nast�puj�cych krok�w:

    #1,Przyjmij poziom istotno�ci,n,nr, rozk�ad generowanych danych oraz temp = 0.
    #2.Wygeneruj n obserwacji z zadanego rozk�adu.
    #3.Wyznacz granice przedzia�u ufno�ci dla danych wygenerowanych w kroku 2.
    #4.Je�li teoretyczna warto�� oczekiwana nale�y do przedzia�u otrzymanego w kroku 3, zwi�ksz temp o jeden.
    #5.Powt�rz kroki 2-4 nr razy.
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

    

