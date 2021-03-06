
# Zadanie 4.1 -------------------------------------------------------------
# Dla danych dotycz�cych d�ugo�ci drogi hamowania w przyk�adzie 2 z wyk�adu,
# bior�c pod uwag� przyj�ty rozk�ad teoretyczny, obliczy� warto�ci trzech estymator�w odchylenia standardowego.
# Zilustruj otrzymane trzy teoretyczne funkcje g�sto�ci na histogramie.

#### spos�b 1 ####
hamulce <- read.table("http://ls.home.amu.edu.pl/data_sets/hamulce.txt", dec = ",")
head(hamulce)

(mu_est <- mean(hamulce$V1))

hist(hamulce$V1, 
     xlab = "Dlugosc drogi hamowania", 
     main = "Rozklad empiryczny dlugosci drogi hamowania",
     probability = TRUE, 
     col = "lightgreen")
lines(density(hamulce$V1), col = "red", lwd = 2)

n = 50

sigma_est <- sd(hamulce$V1)
enw = sqrt(sum((hamulce$V1-mean(hamulce$V1))^2)/n)
enmw = sqrt((n-1)/2)*((gamma((n-1)/2))/(gamma((n)/2)))*sd(hamulce$V1)


hist(hamulce$V1, 
     xlab = "Dlugosc drogi hamowania", 
     main = "Rozklad empiryczny i teoretyczny dlugosci drogi hamowania",
     probability = TRUE, 
     col = "lightgreen")
lines(density(hamulce$V1), col = "red", lwd = 2)
curve(dnorm(x, mu_est, sigma_est), 
      add = TRUE, col = "yellow", lwd = 2)
curve(dnorm(x, mu_est, enw), 
      add = TRUE, col = "blue", lwd = 2)
curve(dnorm(x, mu_est, enmw), 
      add = TRUE, col = "orange", lwd = 2)
legend("topleft", legend = c("s","enw","enmw"), col = c("yellow", "blue", "orange"), lwd = 2)

#### spos�b 2 ####
hamulce <- read.table("http://ls.home.amu.edu.pl/data_sets/hamulce.txt", dec = ",")

hist(hamulce$V1, xlab = "D�ugo�� drogi hamowania",
     main = "Rozk�ad empiryczny i teoretyczny d�ugo�ci hamowania",
     probability = TRUE)
lines(density(hamulce$V1), col = "red", lwd = 2)

n <- length(hamulce$V1)
mu_est <- mean(hamulce$V1)
s <- sd(hamulce$V1)
enw <- sd(hamulce$V1)/sqrt(n/(n-1))
enmw <- sqrt((n-1)/2)*gamma((n-1)/2)/gamma((n/2))* sqrt(n/2)

dnorm(c(0,18,40),mean = mu_est,sd = s)
curve(dnorm(x, mean = mu_est, sd = s),
      add = TRUE, col = 'blue', lwd = 2)

curve(dnorm(x, mean = mu_est, sd = enw),
      add = TRUE, col = 'green', lwd = 2)


# Zadanie 4.2 -------------------------------------------------------------
# Przebadano 200 losowo wybranych 5-sekundowych okres�w pracy centrali telefonicznej. 
# Rejestrowano liczb� zg�osze�. Wyniki s� zawarte w pliku Centrala.RData.
# 1.Zasugeruj rozk�ad teoretyczny badanej zmiennej.
# 2.Oblicz warto�� estymatora parametru rozk�adu teoretycznego.
# 
# 3.Por�wnaj empiryczne prawdopodobie�stwa wyst�pienia poszczeg�lnych warto�ci liczby zg�osze� w pr�bie 
# z warto�ciami teoretycznymi uzyskanymi na podstawie rozk�adu teoretycznego.
# 
# 4.Sprawd� dopasowanie rozk�adu teoretycznego za pomoc� wykresy kwantyl-kwantyl.
# 
# 5.Czy na podstawie powy�szych rozwa�a� rozk�ad teoretyczny wydaje si� odpowiedni?
# 6.Oblicz prawdopodobie�stwo empiryczne i teoretyczne, �e liczba zg�osze� jest mniejsza ni� 4.


load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
Centrala <- Centrala$Liczba

# do tych danych pasuje rozk�ad Poissona
# estymator = parametr rozk�adu teoretycznego
est <-  mean(Centrala)

# suma prawdopodobie�stw rozk�adu teoretycznego 
# (powinna by� < 1, bo ten rozk�ad teoretyczny stanowi jedynie przybli�enie)
probs <- dpois(sort(unique(Centrala)), lambda = est)
sum(probs)

# por�wnanie prawdopodobie�stw
counts <- matrix(c(prop.table(table(Centrala)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala))
counts
barplot(counts, 
        xlab = "Liczba b��d�w", ylab = "Prawdopodobie�stwo",
        main = "Rozk�ady empiryczny i teoretyczny liczby b��d�w",
        col = c("red", "blue"), 
        legend = rownames(counts), 
        beside = TRUE)

# wykres kwantyl-kwantyl
qqplot(rpois(length(Centrala), lambda = est), 
       Centrala, 
       main = "Wykres kwantyl-kwantyl dla liczby zg�osze�",
       xlab = "Kwantyle teoretyczne", 
       ylab = "Kwantyle empiryczne")
qqline(Centrala, distribution = function(probs) { qpois(probs, lambda = est) })
# inny spos�b
install.packages("EnvStats")
library(EnvStats)
qqPlot(Centrala, 
       distribution = "pois", 
       param.list = list(lambda = est),
       add.line = TRUE, 
       main = "Wykres kwantyl-kwantyl dla liczby zg�osze�",
       xlab = "Kwantyle teoretyczne", 
       ylab = "Kwantyle empiryczne")

# prawdopodobie�stwo empiryczne
mean(Centrala < 4)
# prawdopodobie�stwo teoretyczne
ppois(4, lambda = est)

# Zadanie 4.3 -------------------------------------------------------------
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiar�w czasu bezawaryjnej pracy danego urz�dzenia (w godzinach).
# 
# 1.Zasugeruj rozk�ad teoretyczny badanej zmiennej.
# 2.Oblicz warto�� ENW parametru rozk�adu teoretycznego.
# 
# 3.Por�wnaj rozk�ad empiryczny wyst�pienia poszczeg�lnych warto�ci czasu bezawaryjnej pracy w pr�bie 
# z warto�ciami teoretycznymi uzyskanymi na podstawie rozk�adu teoretycznego.
# 
# 4.Sprawd� dopasowanie rozk�adu teoretycznego za pomoc� wykresy kwantyl-kwantyl.
# 
# 5.Czy na podstawie powy�szych rozwa�a� rozk�ad teoretyczny wydaje si� odpowiedni?
# 6.Oblicz empiryczne i teoretyczne prawdopodobie�stwo, �e czas bezawaryjnej pracy zawarty jest w przedziale  [1000, 1500].

awarie <- read.table("http://ls.home.amu.edu.pl/data_sets/awarie.txt", dec = ",")
awarie <- awarie$V1
n <- 50

# dane pasuj� do rozk�adu wyk�adniczego
# estymaotr ENW
ENW <- 1/mean(awarie)

# histogram z estymatorem j�drowym g�sto�ci
hist(awarie, 
     xlab = "Czas bezawaryjnej pracy", 
     main = "Rozk�ady empiryczny i teoretyczny czasu bezawaryjnej pracy",
     probability = TRUE, 
     col = "green")
lines(density(awarie), col = "red", lwd = 2)
# zobrazowanie wyliczonych estymator�w na tym samym wykresie
curve(dexp(x, rate = ENW), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("empiryczny", "teoretyczny"), col = c("red", "blue"), lwd = 2)

# wykres kwantyl-kwantyl
#install.packages("EnvStats")
library(EnvStats)
qqPlot(awarie, 
       distribution = "exp", 
       param.list = list(rate = ENW),
       add.line = TRUE, 
       main = "Wykres kwantyl-kwantyl dla czasu bezawaryjnej pracy",
       xlab = "Kwantyle teoretyczne", 
       ylab = "Kwantyle empiryczne")

# prawdopodobie�stwo empiryczne
mean(1000 <= awarie & awarie <= 1500)
# prawdopodobie�stwo teoretyczne
pexp(1500, rate = ENW) - pexp(1000, rate = ENW)

# Zadanie 4.4 -------------------------------------------------------------
## do zrobienia na kartce ##

# Zadanie 4.5 -------------------------------------------------------------
# Notowano pomiary �redniej szybko�ci wiatru w odst�pach 15 minutowych wok� nowo powstaj�cej elektrowni wiatrowej. Wyniki s� nast�puj�ce:
# 
#   0.9 6.2 2.1 4.1 7.3
#   1.0 4.6 6.4 3.8 5.0
#   2.7 9.2 5.9 7.4 3.0
#   4.9 8.2 5.0 1.2 10.1
#   12.2 2.8 5.9 8.2 0.5
# 
# 1.Zasugeruj rozk�ad teoretyczny badanej zmiennej.
# 2.Oblicz warto�� ENW parametru rozk�adu teoretycznego.
# 
# 3.Por�wnaj rozk�ad empiryczny wyst�pienia poszczeg�lnych warto�ci �redniej szybko�ci wiatru w pr�bie 
# z warto�ciami teoretycznymi uzyskanymi na podstawie rozk�adu teoretycznego.
# 
# 4.Sprawd� dopasowanie rozk�adu teoretycznego za pomoc� wykresy kwantyl-kwantyl.
#  
# 5.Czy na podstawie powy�szych rozwa�a� rozk�ad teoretyczny wydaje si� odpowiedni?
# 6.Oblicz empiryczne i teoretyczne prawdopodobie�stwo, �e �rednia szybko�� wiatru jest zawarta w przedziale  [4, 8].
# 7.Oblicz warto�� ENW dla warto�ci oczekiwanej i wariancji rozk�adu teoretycznego.

pomiary = c(0.9,6.2,2.1,4.1,7.3,1.0,4.6,6.4,3.8,5.0,2.7,9.2,5.9,7.4,3.0,4.9,8.2,5.0,1.2,10.1,12.2,2.8,5.9,8.2,0.5)

# dane pasuj� do rozk�adu Rayleigha 
# estymaotr ENW
ENW <- 0.429*var(pomiary)

# histogram z estymatorem j�drowym g�sto�ci
hist(pomiary, 
     xlab = "�rednia szybko�� wiatru", 
     main = "Rozk�ady empiryczny i teoretyczny �redniej szybko�ci wiatru",
     probability = TRUE, 
     col = "green")
lines(density(pomiary), col = "red", lwd = 2)
# zobrazowanie wyliczonych estymator�w na tym samym wykresie
curve(VGAM::drayleigh(x, ENW), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("empiryczny", "teoretyczny"), col = c("red", "blue"), lwd = 2)
 
# Zadanie 4.6 -------------------------------------------------------------
## do zrobienia na kartce ##

# Zadanie 4.7 -------------------------------------------------------------
# Estymator nazywamy zgodnym, gdy d��y on wed�ug prawdopodobie�stwa lub z prawdopodobie�stwem 1 do parametru, kt�ry estymuje, przy liczebno�ci pr�by n � ???. 
# W pr�bie prostej, przy za�o�eniu, �e zmienna ma sko�czon� i niezerow� wariancj�, estymatorem zgodnym warto�ci oczekiwanej jest �rednia z pr�by.
# 1.Z jakiego wyniku rachunku prawdopodobie�stwa to wnioskujemy?
# 2.Zilustruj ten fakt graficznie?

## to te� jest bardziej teoretyczne##

