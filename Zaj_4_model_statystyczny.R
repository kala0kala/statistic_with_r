
# Zadanie 4.1 -------------------------------------------------------------
# Dla danych dotycz¹cych d³ugoœci drogi hamowania w przyk³adzie 2 z wyk³adu,
# bior¹c pod uwagê przyjêty rozk³ad teoretyczny, obliczyæ wartoœci trzech estymatorów odchylenia standardowego.
# Zilustruj otrzymane trzy teoretyczne funkcje gêstoœci na histogramie.

#### sposób 1 ####
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

#### sposób 2 ####
hamulce <- read.table("http://ls.home.amu.edu.pl/data_sets/hamulce.txt", dec = ",")

hist(hamulce$V1, xlab = "D³ugoœæ drogi hamowania",
     main = "Rozk³ad empiryczny i teoretyczny d³ugoœci hamowania",
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
# Przebadano 200 losowo wybranych 5-sekundowych okresów pracy centrali telefonicznej. 
# Rejestrowano liczbê zg³oszeñ. Wyniki s¹ zawarte w pliku Centrala.RData.
# 1.Zasugeruj rozk³ad teoretyczny badanej zmiennej.
# 2.Oblicz wartoœæ estymatora parametru rozk³adu teoretycznego.
# 
# 3.Porównaj empiryczne prawdopodobieñstwa wyst¹pienia poszczególnych wartoœci liczby zg³oszeñ w próbie 
# z wartoœciami teoretycznymi uzyskanymi na podstawie rozk³adu teoretycznego.
# 
# 4.SprawdŸ dopasowanie rozk³adu teoretycznego za pomoc¹ wykresy kwantyl-kwantyl.
# 
# 5.Czy na podstawie powy¿szych rozwa¿añ rozk³ad teoretyczny wydaje siê odpowiedni?
# 6.Oblicz prawdopodobieñstwo empiryczne i teoretyczne, ¿e liczba zg³oszeñ jest mniejsza ni¿ 4.


load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
Centrala <- Centrala$Liczba

# do tych danych pasuje rozk³ad Poissona
# estymator = parametr rozk³adu teoretycznego
est <-  mean(Centrala)

# suma prawdopodobieñstw rozk³adu teoretycznego 
# (powinna byæ < 1, bo ten rozk³ad teoretyczny stanowi jedynie przybli¿enie)
probs <- dpois(sort(unique(Centrala)), lambda = est)
sum(probs)

# porównanie prawdopodobieñstw
counts <- matrix(c(prop.table(table(Centrala)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala))
counts
barplot(counts, 
        xlab = "Liczba b³êdów", ylab = "Prawdopodobieñstwo",
        main = "Rozk³ady empiryczny i teoretyczny liczby b³êdów",
        col = c("red", "blue"), 
        legend = rownames(counts), 
        beside = TRUE)

# wykres kwantyl-kwantyl
qqplot(rpois(length(Centrala), lambda = est), 
       Centrala, 
       main = "Wykres kwantyl-kwantyl dla liczby zg³oszeñ",
       xlab = "Kwantyle teoretyczne", 
       ylab = "Kwantyle empiryczne")
qqline(Centrala, distribution = function(probs) { qpois(probs, lambda = est) })
# inny sposób
install.packages("EnvStats")
library(EnvStats)
qqPlot(Centrala, 
       distribution = "pois", 
       param.list = list(lambda = est),
       add.line = TRUE, 
       main = "Wykres kwantyl-kwantyl dla liczby zg³oszeñ",
       xlab = "Kwantyle teoretyczne", 
       ylab = "Kwantyle empiryczne")

# prawdopodobieñstwo empiryczne
mean(Centrala < 4)
# prawdopodobieñstwo teoretyczne
ppois(4, lambda = est)

# Zadanie 4.3 -------------------------------------------------------------
# Zmienna w pliku awarie.txt opisuje wyniki 50 pomiarów czasu bezawaryjnej pracy danego urz¹dzenia (w godzinach).
# 
# 1.Zasugeruj rozk³ad teoretyczny badanej zmiennej.
# 2.Oblicz wartoœæ ENW parametru rozk³adu teoretycznego.
# 
# 3.Porównaj rozk³ad empiryczny wyst¹pienia poszczególnych wartoœci czasu bezawaryjnej pracy w próbie 
# z wartoœciami teoretycznymi uzyskanymi na podstawie rozk³adu teoretycznego.
# 
# 4.SprawdŸ dopasowanie rozk³adu teoretycznego za pomoc¹ wykresy kwantyl-kwantyl.
# 
# 5.Czy na podstawie powy¿szych rozwa¿añ rozk³ad teoretyczny wydaje siê odpowiedni?
# 6.Oblicz empiryczne i teoretyczne prawdopodobieñstwo, ¿e czas bezawaryjnej pracy zawarty jest w przedziale  [1000, 1500].

awarie <- read.table("http://ls.home.amu.edu.pl/data_sets/awarie.txt", dec = ",")
awarie <- awarie$V1
n <- 50

# dane pasuj¹ do rozk³adu wyk³adniczego
# estymaotr ENW
ENW <- 1/mean(awarie)

# histogram z estymatorem j¹drowym gêstoœci
hist(awarie, 
     xlab = "Czas bezawaryjnej pracy", 
     main = "Rozk³ady empiryczny i teoretyczny czasu bezawaryjnej pracy",
     probability = TRUE, 
     col = "green")
lines(density(awarie), col = "red", lwd = 2)
# zobrazowanie wyliczonych estymatorów na tym samym wykresie
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

# prawdopodobieñstwo empiryczne
mean(1000 <= awarie & awarie <= 1500)
# prawdopodobieñstwo teoretyczne
pexp(1500, rate = ENW) - pexp(1000, rate = ENW)

# Zadanie 4.4 -------------------------------------------------------------
## do zrobienia na kartce ##

# Zadanie 4.5 -------------------------------------------------------------
# Notowano pomiary œredniej szybkoœci wiatru w odstêpach 15 minutowych wokó³ nowo powstaj¹cej elektrowni wiatrowej. Wyniki s¹ nastêpuj¹ce:
# 
#   0.9 6.2 2.1 4.1 7.3
#   1.0 4.6 6.4 3.8 5.0
#   2.7 9.2 5.9 7.4 3.0
#   4.9 8.2 5.0 1.2 10.1
#   12.2 2.8 5.9 8.2 0.5
# 
# 1.Zasugeruj rozk³ad teoretyczny badanej zmiennej.
# 2.Oblicz wartoœæ ENW parametru rozk³adu teoretycznego.
# 
# 3.Porównaj rozk³ad empiryczny wyst¹pienia poszczególnych wartoœci œredniej szybkoœci wiatru w próbie 
# z wartoœciami teoretycznymi uzyskanymi na podstawie rozk³adu teoretycznego.
# 
# 4.SprawdŸ dopasowanie rozk³adu teoretycznego za pomoc¹ wykresy kwantyl-kwantyl.
#  
# 5.Czy na podstawie powy¿szych rozwa¿añ rozk³ad teoretyczny wydaje siê odpowiedni?
# 6.Oblicz empiryczne i teoretyczne prawdopodobieñstwo, ¿e œrednia szybkoœæ wiatru jest zawarta w przedziale  [4, 8].
# 7.Oblicz wartoœæ ENW dla wartoœci oczekiwanej i wariancji rozk³adu teoretycznego.

pomiary = c(0.9,6.2,2.1,4.1,7.3,1.0,4.6,6.4,3.8,5.0,2.7,9.2,5.9,7.4,3.0,4.9,8.2,5.0,1.2,10.1,12.2,2.8,5.9,8.2,0.5)

# dane pasuj¹ do rozk³adu Rayleigha 
# estymaotr ENW
ENW <- 0.429*var(pomiary)

# histogram z estymatorem j¹drowym gêstoœci
hist(pomiary, 
     xlab = "Œrednia szybkoœæ wiatru", 
     main = "Rozk³ady empiryczny i teoretyczny œredniej szybkoœci wiatru",
     probability = TRUE, 
     col = "green")
lines(density(pomiary), col = "red", lwd = 2)
# zobrazowanie wyliczonych estymatorów na tym samym wykresie
curve(VGAM::drayleigh(x, ENW), 
      add = TRUE, col = "blue", lwd = 2)
legend("topright", legend = c("empiryczny", "teoretyczny"), col = c("red", "blue"), lwd = 2)
 
# Zadanie 4.6 -------------------------------------------------------------
## do zrobienia na kartce ##

# Zadanie 4.7 -------------------------------------------------------------
# Estymator nazywamy zgodnym, gdy d¹¿y on wed³ug prawdopodobieñstwa lub z prawdopodobieñstwem 1 do parametru, który estymuje, przy liczebnoœci próby n › ???. 
# W próbie prostej, przy za³o¿eniu, ¿e zmienna ma skoñczon¹ i niezerow¹ wariancjê, estymatorem zgodnym wartoœci oczekiwanej jest œrednia z próby.
# 1.Z jakiego wyniku rachunku prawdopodobieñstwa to wnioskujemy?
# 2.Zilustruj ten fakt graficznie?

## to te¿ jest bardziej teoretyczne##

