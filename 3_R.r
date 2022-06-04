#_______________________________3____________________________________
#Zadanie_3.1
#Zmienna wynik w pliku ankieta.txt opisuje wyniki badania dzia�alno�ci prezydenta pewnego miasta. 
#Wybrano losowo 100 mieszka�c�w miasta i zadano im nast�puj�ce pytanie: 
    #Jak oceniasz dzia�alno�� prezydenta miasta? Dost�pne by�y nast�puj�ce odpowiedzi: 
        #zdecydowanie dobrze (a),
        #dobrze (b), 
        #�le (c), 
        #zdecydowanie �le (d),
        #nie mam zdania (e). 
    #Jakiego typu jest ta zmienna? 
    #Jakie s� mo�liwe warto�ci tej zmiennej?

#1.Zaimportuj dane z pliku ankieta.txt do zmiennej ankieta.

ankieta <-  read.delim(url("http://ls.home.amu.edu.pl/data_sets/ankieta.txt"))
ankieta

#2.Przedstaw rozk�ad empiryczny zmiennej wynik za pomoc� szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(ankieta$wynik),
                 procent = prop.table(table(ankieta$wynik))))

#3.Przedstaw rozk�ad empiryczny zmiennej wynik tylko dla os�b z wykszta�ceniem podstawowym za pomoc� szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(ankieta$wynik[ankieta$szkola == "p"]),
                 procent = prop.table(table(ankieta$wynik[ankieta$szkola == "p"]))))

#4.Zilustruj wyniki ankiety za pomoc� wykresu s�upkowego i ko�owego.
barplot(table(ankieta$wynik),
        xlab = "Odpowiedzi", ylab = "Liczebno��",
        col = c("black", "red", "green", "blue", "cyan"),
        main = "Rozk�ad empiryczny zmiennej wynik")
barplot(prop.table(table(ankieta$wynik)),
        xlab = "Odpowiedzi", ylab = "Liczebno��",
        col = c("black", "red", "green", "blue", "cyan"),
        main = "Rozk�ad empiryczny zmiennej wynik")
pie(table(ankieta$wynik))


#5.Zilustruj wyniki ankiety za pomoc� wykresu s�upkowego z podzia�em na kobiety i m�czyzn

height <-  cbind(table(ankieta$wynik[ankieta$plec == "k"]), table(ankieta$
                                                                   wynik[ankieta$plec == "m"]))
barplot(height, beside = TRUE, legend.text = TRUE,
        col=c("black", "red", "green", "blue", "cyan"),
        names = c("k","m"))
#6.Zinterpretuj powy�sze wyniki (tabelaryczne i graficzne).
"..."

#Zadanie_3.2
#Przebadano 200 losowo wybranych 5-sekundowych okres�w pracy centrali telefonicznej. 
#Rejestrowano liczb� zg�osze�. Wyniki s� zawarte w pliku Centrala.RData. 
#Jakiego typu jest ta zmienna? Jakie s� mo�liwe warto�ci tej zmiennej?

#1.Zaimportuj dane z pliku Centrala.RData.

load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))
head(Centrala)

#2.Przedstaw rozk�ad empiryczny liczby zg�osze� za pomoc� szeregu rozdzielczego.

data.frame(cbind(liczebnosc = table(Centrala),
                 procent = prop.table(table(Centrala))))

#3.Zilustruj liczb� zg�osze� za pomoc� wykresu s�upkowego i ko�owego.

barplot(table(Centrala),
        xlab = "Liczba zg�osze�", ylab = "Liczebno��",
        col = c("black", "red", "green", "blue", "cyan", "magenta"),
        main = "Rozk�ad empiryczny listy zg�osze�")

barplot(prop.table(table(Centrala)),
        xlab = "Liczba zg�osze�", ylab = "Prawdopodobie�stwo",
        col = c("black", "red", "green", "blue", "cyan", "magenta"),
        main = "Rozk�ad empiryczny listy zg�osze�")

pie(table(Centrala))

#4.Obliczy� �redni� z liczby zg�osze�, median� liczby zg�osze�, 
  #odchylenie standardowe liczby zg�osze� i wsp�czynnik zmienno�ci liczby zg�osze�.
mean(Centrala$Liczba)

median(Centrala$Liczba)

sd(Centrala$Liczba)

sd(Centrala$Liczba) / mean(Centrala$Liczba)*100

#5.Zinterpretuj powy�sze wyniki (tabelaryczne, graficzne i liczbowe).

"Najwi�kszy udzia� mia�y zg�oszenia w liczebno�ci r�wnej 1 (ok.34% wszystkich
zg�osze�).
�rednio na okres 5 sekund przypada�y 2 zg�oszenia.
Wyst�puje tu asymetria prawostronna rozk�adu.
Odchylenie standardowe wskazuje na znacz�ce odchylenie warto�ci od �redniej.
Wsp�czynnik zmienno�ci wskazuje na bardzo du�e zr�nicowanie populacji.
"

#Zadanie_3.3

#Zmienna w pliku awarie.txt opisuje wyniki 50 pomiar�w czasu 
#bezawaryjnej pracy danego urz�dzenia (w godzinach). 
#Jakiego typu jest ta zmienna? 
#Jakie s� mo�liwe warto�ci tej zmiennej?

#1.Zaimportuj dane z pliku awarie.txt.

awarie <- read.table("http://ls.home.amu.edu.pl/data_sets/awarie.txt")

#2.Przedstaw rozk�ad empiryczny czasu bezawaryjnej pracy za pomoc� szeregu rozdzielczego.

breaksH <- hist(awarie$V1, plot = FALSE)$breaks
data.frame(cbind(liczenosc = table(cut(awarie$V1, breaks = breaksH)),
                 procent = prop.table(table(cut(awarie$V1, breaks = breaksH)))))

#3.Zilustruj rozk�ad empiryczny czasu bezawaryjnej pracy za pomoc� histogramu.

hist(awarie$V1, 
     xlab = 'Czas bezwarunkowej pracy',
     main = 'Rozk�ad empiryczny czasu bezawaryjnej pracy',
     probability = TRUE)
rug(jitter(awarie$V1))
lines(density(awarie$V1), lwd = 3)

#4.Obliczy� �redni�, median�, kwartyle rzed�w 0, 0.25, 0.5, 0.75, 1, odchylenie standardowe 
  #i wsp�czynnik zmienno�ci czasu bezawaryjnej pracy.
#wyliczanie kwantyli
awarie <- awarie$V1
mean(awarie)
median(awarie)
quantile(awarie, probs = c(0, 0.25, 0.5, 0.75, 1))
sd(awarie)
sd(awarie)/mean(awarie)*100

#5.Zilustruj rozk�ad empiryczny czasu bezawaryjnej pracy za pomoc� wykresu pude�kowego

#wykres pude�kowy
boxplot(awarie, 
        ylab = "Czas bezwarunkowej pracy",
        main = "Rozk�ad empiryczny czasu bezawaryjnej pracy")


#6.Zinterpretuj powy�sze wyniki (tabelaryczne, graficzne i liczbowe).
"..."



#Zadanie_3.4
#Notowano pomiary �redniej szybko�ci wiatru w odst�pach  
#15 minutowych wok� nowo powstaj�cej elektrowni wiatrowej. 
#Wyniki s� nast�puj�ce:
    #0.9,6.2,2.1,4.1,7.3,
    #1.0,4.6,6.4,3.8,5.0,
    #2.7,9.2,5.9,7.4,3.0,
    #4.9,8.2,5.0,1.2,10.1,
    #12.2,2.8.5.9,8.2,0.5

#Jakiego typu jest ta zmienna? 
#Jest to ilo�ciowa zmienna dyskretna.

#Jakie s� mo�liwe warto�ci tej zmiennej?
#Zmienna przyjmuje warto�ci w przedziale 0.5 do 12.2.

#1.Przedstaw rozk�ad empiryczny badanej zmiennej za pomoc� szeregu rozdzielczego.

vector <-  c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9,
            8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)
vector_hist <-  hist(vector, plot=FALSE)$breaks
data.frame(cbind(liczebnosc = table(cut(vector, breaks = vector_hist)),
                 procent = prop.table(table(cut(vector, breaks = vector_hist
                 )))))

#2.Zilustruj rozk�ad empiryczny �redniej szybko�ci wiatru za pomoc� histogramu i wykresu pude�kowego.

hist(vector,
     xlab = "�rednia szybko�� wiatru",
     main = "Rozk�ad empiryczny �redniej szybko�ci wiatru")
rug(jitter(vector))

hist(vector,
     xlab = "�rednia szybko�� wiatru",
     main = "Rozk�ad empiryczny �redniej szybko�ci wiatru",
     probability = TRUE,
     col = "lightblue")
lines(density(vector), col = "green", lwd = 2)

boxplot(vector,
        ylab = "�rednia szybko�� wiatru",
        main = "Rozk�ad empiryczny �redniej szybko�ci wiatru")

#3.Obliczy� �redni�, median�, odchylenie standardowe i wsp�czynnik zmienno�ci �redniej szybko�ci wiatru.
mean(vector)

median(vector)

sd(vector)

sd(vector) / mean(vector)*100

#install.packages("e1071")
library(e1071)
skewness(vector)

library(e1071)
kurtosis(vector)

#4.Zinterpretuj powy�sze wyniki (tabelaryczne, graficzne i liczbowe).

"Najwi�kszy udzia� rozk�adu posiadaj� warto�ci �redniej pr�dko�ci wiatru w
przedziale od 4 do 6 (28%).
�rednia pr�dko�� wiatru w 15-minutowych odst�pach wynosi 5.
Odchylenie warto�ci od �redniej jest do�� du�e.
Wsp�czynnik zmienno�ci wskazuje na du�e zr�nicowanie warto�ci populacji.
Wsp�czynnik sko�no�ci wskazuje na prawostronn� asymetri� rozk�adu.
Ujemna kurtoza wskazuje na sp�aszczony rozk�ad zmiennej."

#Zadanie_3.5
#Napisz funkcj� wspolczynnik_zmiennosci(), 
  #kt�ra oblicza warto�� wsp�czynnika zmienno�ci dla danego wektora obserwacji. 
#Funkcja powinna mie� dwa argumenty:
    #x - wektor zawieraj�cy dane,
    #na.rm - warto�� logiczna (domy�lnie FALSE), kt�ra wskazuje czy braki danych (obiekty NA) maj� by� zignorowane.
#Funkcja zwraca warto�� wsp�czynnika zmienno�ci wyra�on� w procentach. 
#Ponadto funkcja sprawdza, czy wektor x jest wektorem numerycznym. 
#W przeciwnym razie zostanie zwr�cony b��d z nast�puj�cym komunikatem: 
#,,argument nie jest liczb���.
#Przyk�adowe wywo�ania i wyniki funkcji s� nast�puj�ce:

wspolczynnik_zmiennosci <- function(x, na.rm = FALSE){
  if(!is.numeric(x)) stop("argument x nie jest liczb�")
  if (na.rm) x = na.omit(x)
  return(sd(x)/mean(x)*100)
}

wspolczynnik_zmiennosci(c(1, NA, 3))
wspolczynnik_zmiennosci(c(1, NA, 3), na.rm = TRUE)
wspolczynnik_zmiennosci()
wspolczynnik_zmiennosci(c("x", "y"))

#Zadanie_3.6

#Za obserwacje odstaj�ce cz�sto uznaje si� takie obserwacje z danego ci�gu liczb, kt�re s� mniejsze od  
#Q1-(3/2)R lub wi�ksze ni�  
#Q3+(3/2)R,gdzie Q1 i Q3
#oznaczaj� kwartyl pierwszy i trzeci, odpowiednio, a  
#R=Q3-Q1
#jest odst�pem mi�dzykwartylowym z pr�by. 
#Napisz funkcj�, kt�ra dla zadanego wektora obserwacji, zwraca�a b�dzie list� o dw�ch elementach: 
#pierwszy b�dzie wektorem podaj�cym indeksy obserwacji odstaj�cych a drugi same obserwacje odstaj�ce.

#znajdowanie obserwacji odstaj�cych i ich indeks�w
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