#_______________________________3____________________________________
#Zadanie_3.1
#Zmienna wynik w pliku ankieta.txt opisuje wyniki badania dzia³alnoœci prezydenta pewnego miasta. 
#Wybrano losowo 100 mieszkañców miasta i zadano im nastêpuj¹ce pytanie: 
    #Jak oceniasz dzia³alnoœæ prezydenta miasta? Dostêpne by³y nastêpuj¹ce odpowiedzi: 
        #zdecydowanie dobrze (a),
        #dobrze (b), 
        #Ÿle (c), 
        #zdecydowanie Ÿle (d),
        #nie mam zdania (e). 
    #Jakiego typu jest ta zmienna? 
    #Jakie s¹ mo¿liwe wartoœci tej zmiennej?

#1.Zaimportuj dane z pliku ankieta.txt do zmiennej ankieta.

ankieta <-  read.delim(url("http://ls.home.amu.edu.pl/data_sets/ankieta.txt"))
ankieta

#2.Przedstaw rozk³ad empiryczny zmiennej wynik za pomoc¹ szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(ankieta$wynik),
                 procent = prop.table(table(ankieta$wynik))))

#3.Przedstaw rozk³ad empiryczny zmiennej wynik tylko dla osób z wykszta³ceniem podstawowym za pomoc¹ szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(ankieta$wynik[ankieta$szkola == "p"]),
                 procent = prop.table(table(ankieta$wynik[ankieta$szkola == "p"]))))

#4.Zilustruj wyniki ankiety za pomoc¹ wykresu s³upkowego i ko³owego.
barplot(table(ankieta$wynik),
        xlab = "Odpowiedzi", ylab = "Liczebnoœæ",
        col = c("black", "red", "green", "blue", "cyan"),
        main = "Rozk³ad empiryczny zmiennej wynik")
barplot(prop.table(table(ankieta$wynik)),
        xlab = "Odpowiedzi", ylab = "Liczebnoœæ",
        col = c("black", "red", "green", "blue", "cyan"),
        main = "Rozk³ad empiryczny zmiennej wynik")
pie(table(ankieta$wynik))


#5.Zilustruj wyniki ankiety za pomoc¹ wykresu s³upkowego z podzia³em na kobiety i mê¿czyzn

height <-  cbind(table(ankieta$wynik[ankieta$plec == "k"]), table(ankieta$
                                                                   wynik[ankieta$plec == "m"]))
barplot(height, beside = TRUE, legend.text = TRUE,
        col=c("black", "red", "green", "blue", "cyan"),
        names = c("k","m"))
#6.Zinterpretuj powy¿sze wyniki (tabelaryczne i graficzne).
"..."

#Zadanie_3.2
#Przebadano 200 losowo wybranych 5-sekundowych okresów pracy centrali telefonicznej. 
#Rejestrowano liczbê zg³oszeñ. Wyniki s¹ zawarte w pliku Centrala.RData. 
#Jakiego typu jest ta zmienna? Jakie s¹ mo¿liwe wartoœci tej zmiennej?

#1.Zaimportuj dane z pliku Centrala.RData.

load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
head(Centrala)

#2.Przedstaw rozk³ad empiryczny liczby zg³oszeñ za pomoc¹ szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(Centrala),
                 procent = prop.table(table(Centrala))))

#3.Zilustruj liczbê zg³oszeñ za pomoc¹ wykresu s³upkowego i ko³owego.

barplot(table(Centrala),
        xlab = "Liczba zg³oszeñ", ylab = "Liczebnoœæ",
        col = c("black", "red", "green", "blue", "cyan", "magenta"),
        main = "Rozk³ad empiryczny listy zg³oszeñ")

barplot(prop.table(table(Centrala)),
        xlab = "Liczba zg³oszeñ", ylab = "Prawdopodobieñstwo",
        col = c("black", "red", "green", "blue", "cyan", "magenta"),
        main = "Rozk³ad empiryczny listy zg³oszeñ")

pie(table(Centrala))

#4.Obliczyæ œredni¹ z liczby zg³oszeñ, medianê liczby zg³oszeñ, 
  #odchylenie standardowe liczby zg³oszeñ i wspó³czynnik zmiennoœci liczby zg³oszeñ.
mean(Centrala$Liczba)

median(Centrala$Liczba)

sd(Centrala$Liczba)

sd(Centrala$Liczba) / mean(Centrala$Liczba)*100

#5.Zinterpretuj powy¿sze wyniki (tabelaryczne, graficzne i liczbowe).

"Najwiêkszy udzia³ mia³y zg³oszenia w liczebnoœci równej 1 (ok.34% wszystkich
zg³oszeñ).
Œrednio na okres 5 sekund przypada³y 2 zg³oszenia.
Wystêpuje tu asymetria prawostronna rozk³adu.
Odchylenie standardowe wskazuje na znacz¹ce odchylenie wartoœci od œredniej.
Wspó³czynnik zmiennoœci wskazuje na bardzo du¿e zró¿nicowanie populacji.
"

#Zadanie_3.3

#Zmienna w pliku awarie.txt opisuje wyniki 50 pomiarów czasu 
#bezawaryjnej pracy danego urz¹dzenia (w godzinach). 
#Jakiego typu jest ta zmienna? 
#Jakie s¹ mo¿liwe wartoœci tej zmiennej?

#1.Zaimportuj dane z pliku awarie.txt.

awarie <- read.table("http://ls.home.amu.edu.pl/data_sets/awarie.txt")

#2.Przedstaw rozk³ad empiryczny czasu bezawaryjnej pracy za pomoc¹ szeregu rozdzielczego.

breaksH <- hist(awarie$V1, plot = FALSE)$breaks
data.frame(cbind(liczenosc = table(cut(awarie$V1, breaks = breaksH)),
                 procent = prop.table(table(cut(awarie$V1, breaks = breaksH)))))

#3.Zilustruj rozk³ad empiryczny czasu bezawaryjnej pracy za pomoc¹ histogramu.

hist(awarie$V1, 
     xlab = 'Czas bezwarunkowej pracy',
     main = 'Rozk³ad empiryczny czasu bezawaryjnej pracy',
     probability = TRUE)
rug(jitter(awarie$V1))
lines(density(awarie$V1), lwd = 3)

#4.Obliczyæ œredni¹, medianê, kwartyle rzedów 0, 0.25, 0.5, 0.75, 1, odchylenie standardowe 
  #i wspó³czynnik zmiennoœci czasu bezawaryjnej pracy.
#wyliczanie kwantyli
awarie <- awarie$V1
mean(awarie)
median(awarie)
quantile(awarie, probs = c(0, 0.25, 0.5, 0.75, 1))
sd(awarie)
sd(awarie)/mean(awarie)*100

#5.Zilustruj rozk³ad empiryczny czasu bezawaryjnej pracy za pomoc¹ wykresu pude³kowego

#wykres pude³kowy
boxplot(awarie, 
        ylab = "Czas bezwarunkowej pracy",
        main = "Rozk³ad empiryczny czasu bezawaryjnej pracy")


#6.Zinterpretuj powy¿sze wyniki (tabelaryczne, graficzne i liczbowe).
"..."



#Zadanie_3.4
#Notowano pomiary œredniej szybkoœci wiatru w odstêpach  
#15 minutowych wokó³ nowo powstaj¹cej elektrowni wiatrowej. 
#Wyniki s¹ nastêpuj¹ce:
    #0.9,6.2,2.1,4.1,7.3,
    #1.0,4.6,6.4,3.8,5.0,
    #2.7,9.2,5.9,7.4,3.0,
    #4.9,8.2,5.0,1.2,10.1,
    #12.2,2.8.5.9,8.2,0.5

#Jakiego typu jest ta zmienna? 
#Jest to iloœciowa zmienna dyskretna.

#Jakie s¹ mo¿liwe wartoœci tej zmiennej?
#Zmienna przyjmuje wartoœci w przedziale 0.5 do 12.2.

#1.Przedstaw rozk³ad empiryczny badanej zmiennej za pomoc¹ szeregu rozdzielczego.

vector <-  c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9,
            8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)
vector_hist <-  hist(vector, plot=FALSE)$breaks
data.frame(cbind(liczebnosc = table(cut(vector, breaks = vector_hist)),
                 procent = prop.table(table(cut(vector, breaks = vector_hist
                 )))))

#2.Zilustruj rozk³ad empiryczny œredniej szybkoœci wiatru za pomoc¹ histogramu i wykresu pude³kowego.

hist(vector,
     xlab = "Œrednia szybkoœæ wiatru",
     main = "Rozk³ad empiryczny œredniej szybkoœci wiatru")
rug(jitter(vector))

hist(vector,
     xlab = "Œrednia szybkoœæ wiatru",
     main = "Rozk³ad empiryczny œredniej szybkoœci wiatru",
     probability = TRUE,
     col = "lightblue")
lines(density(vector), col = "green", lwd = 2)

boxplot(vector,
        ylab = "Œrednia szybkoœæ wiatru",
        main = "Rozk³ad empiryczny œredniej szybkoœci wiatru")

#3.Obliczyæ œredni¹, medianê, odchylenie standardowe i wspó³czynnik zmiennoœci œredniej szybkoœci wiatru.
mean(vector)

median(vector)

sd(vector)

sd(vector) / mean(vector)*100

#install.packages("e1071")
library(e1071)
skewness(vector)

library(e1071)
kurtosis(vector)

#4.Zinterpretuj powy¿sze wyniki (tabelaryczne, graficzne i liczbowe).

"Najwiêkszy udzia³ rozk³adu posiadaj¹ wartoœci œredniej prêdkoœci wiatru w
przedziale od 4 do 6 (28%).
Œrednia prêdkoœæ wiatru w 15-minutowych odstêpach wynosi 5.
Odchylenie wartoœci od œredniej jest doœæ du¿e.
Wspó³czynnik zmiennoœci wskazuje na du¿e zró¿nicowanie wartoœci populacji.
Wspó³czynnik skoœnoœci wskazuje na prawostronn¹ asymetriê rozk³adu.
Ujemna kurtoza wskazuje na sp³aszczony rozk³ad zmiennej."

#Zadanie_3.5
#Napisz funkcjê wspolczynnik_zmiennosci(), 
  #która oblicza wartoœæ wspó³czynnika zmiennoœci dla danego wektora obserwacji. 
#Funkcja powinna mieæ dwa argumenty:
    #x - wektor zawieraj¹cy dane,
    #na.rm - wartoœæ logiczna (domyœlnie FALSE), która wskazuje czy braki danych (obiekty NA) maj¹ byæ zignorowane.
#Funkcja zwraca wartoœæ wspó³czynnika zmiennoœci wyra¿on¹ w procentach. 
#Ponadto funkcja sprawdza, czy wektor x jest wektorem numerycznym. 
#W przeciwnym razie zostanie zwrócony b³¹d z nastêpuj¹cym komunikatem: 
#,,argument nie jest liczb¹’’.
#Przyk³adowe wywo³ania i wyniki funkcji s¹ nastêpuj¹ce:

wspolczynnik_zmiennosci <- function(x, na.rm = FALSE){
  if(!is.numeric(x)) stop("argument x nie jest liczb¹")
  if (na.rm) x = na.omit(x)
  return(sd(x)/mean(x)*100)
}

wspolczynnik_zmiennosci(c(1, NA, 3))
wspolczynnik_zmiennosci(c(1, NA, 3), na.rm = TRUE)
wspolczynnik_zmiennosci()
wspolczynnik_zmiennosci(c("x", "y"))

#Zadanie_3.6

#Za obserwacje odstaj¹ce czêsto uznaje siê takie obserwacje z danego ci¹gu liczb, które s¹ mniejsze od  
#Q1-(3/2)R lub wiêksze ni¿  
#Q3+(3/2)R,gdzie Q1 i Q3
#oznaczaj¹ kwartyl pierwszy i trzeci, odpowiednio, a  
#R=Q3-Q1
#jest odstêpem miêdzykwartylowym z próby. 
#Napisz funkcjê, która dla zadanego wektora obserwacji, zwraca³a bêdzie listê o dwóch elementach: 
#pierwszy bêdzie wektorem podaj¹cym indeksy obserwacji odstaj¹cych a drugi same obserwacje odstaj¹ce.

#znajdowanie obserwacji odstaj¹cych i ich indeksów
obserwacje_odstajace <- function(x){
  Q1 = quantile(x, probs = 0.25)
  Q3 = quantile(x, probs = 0.75)
  R = Q3 - Q1
  indeksy = which(x < (Q1 - 1.5*R) | x > (Q3 + 1.5*R))
  wartosci = x[indeksy]
  return(list(indeksy, wartosci))
}

x = c(100, 130, 111, 120, 500)
obserwacje_odstajace(x)