#____________________________________1_________________________________________
#Zadanie_1.1
# Otwórz program RStudio. Nastêpnie utwórz nowy skrypt i zapisz go jako, na przyk³ad,
#wprowadzenie_do_R_zadania.R. W tym skrypcie mo¿esz napisaæ rozwi¹zania kolejnych zadañ.

CTRL + SHIFT + N


#Zadanie_1.2
#U¿yj funkcji rep(), aby utworzyæ wektor logiczny, 
#zaczynaj¹c od trzech wartoœci prawda, nastêpnie czterech wartoœci fa³sz, 
#po których nastêpuj¹ dwie wartoœci prawda i wreszcie piêæ wartoœci fa³sz.

x <-  c(rep(TRUE,3), rep(FALSE,4), rep(TRUE,2), rep(FALSE,5))
x

#Zadanie_1.3
#Palindromem nazywamy wektor, którego elementy czytane od koñca tworz¹ ten sam wektor co elementy czytane od pocz¹tku. 
#Utwórz taki wektor 100 liczb przy czym pierwsze 20 liczb to kolejne liczby naturalne, nastêpnie wystêpuje 10 zer, 
#nastêpnie 20 kolejnych liczb parzystych, a pozosta³e elementy okreœlone s¹ przez palindromicznoœæ (warunek symetrii).

vector <-  c(1:20, rep(0,10), seq(2,40,by=2))
vector <-  c(vector, rev(vector))
vector

#Zadanie_1.4
#Czy w programie R dzia³a mechanizm czêœciowego dopasowania (ang. partial matching) 
#lub autouzupe³nienie, tj. nie trzeba (zazwyczaj) odwo³ywaæ siê do pe³nych nazw argumentów funkcji przy ich wywo³ywaniu? 
#SprawdŸ to na wybranym przyk³adzie funkcji jêzyka R.

rep(TRUE, times=3)
rep(TRUE, t=3)

#Zadanie_1.5
#Utwórz wektor 30 napisów nastêpuj¹cej postaci: liczba.litera, 
#gdzie liczba to kolejne liczby naturalne od 1 do 30 a litera to trzy wielkie litery X, Y, Z wystêpuj¹ce cyklicznie.

paste(1:30, c("X","Y","Z"), sep=".")


#Zadanie_1.6
#Wyznacz wszystkie kombinacje wartoœci wektorów (a,b) i (1,2,3)
#za pomoc¹ funkcji rep() i paste()

vector1 <-  c("a","b")
vector2 <-  c(1,2,3)
c(paste(rep(vector1[1], length(vector2)), vector2, sep=""),
  paste(rep(vector1[2], length(vector2)), vector2, sep=""))

#Zadanie_1.7
#SprawdŸ typ nastêpuj¹cych obiektów jêzyka R:

typeof(FALSE); mode(FALSE)
typeof(1); mode(1)
typeof(1L); mode(1L)
typeof(2.2); mode(2.2)
typeof("statystyka"); mode("statystyka")
typeof(list(TRUE, 2L, 3.5, 2i, "statystyka")); mode(list(TRUE, 2L, 3.5, 2i, "statystyka"))


#Zadanie_1.8
#W jêzyku R mamy hierarchiê typów dla wektorów atomowych. 
#Przechowuj¹ one elementy jednego œciœle okreœlonego typu (maj¹ jednorodn¹ strukturê). 
#Gdy w wektorze pojawi¹ siê elementy ró¿nego typu, R uzgodni ich typ tak, 
#aby informacjê o najbardziej ,,ogólnym’’ z obiektów da³o siê przechowaæ bez znacz¹cej straty. 
#Zbadaj jak dzia³a hierarchia typów w R zbieraj¹c w jeden wektor:

    #wartoœæ logiczn¹, ca³kowitoliczbow¹, rzeczywistoliczbow¹, napis,
    #wartoœæ logiczn¹, ca³kowitoliczbow¹, rzeczywistoliczbow¹,
    #wartoœæ logiczn¹, ca³kowitoliczbow¹.

c(TRUE, 2, 1.5, "statystyka")
c(TRUE, 2, 1.5)
c(TRUE, 2)

#Zadanie_1.9
#Wektory danego typu mo¿na rzutowaæ (rzutowanie typów) na inny typ za pomoc¹ funkcji: 
#as.character(), as.double(), as.numeric(), as.integer(), as.logical(). 
#Mo¿emy rzutowaæ wektory ogólniejszego typu do bardziej szczegó³owego. 
#Wartoœci logiczne TRUE i FALSE liczbowo s¹ zawsze przedstawione jako 1 i 0, odpowiednio. 
#Wartoœæ liczbowa ró¿na od zera jest zawsze rzutowana do wartoœci logicznej TRUE. Dokonaj nastêpuj¹cych rzutowañ:

    #wektora logicznego na wektor numeryczny,
    #wektora logicznego na wektor napisów,
    #wektora liczbowego (ma zawieraæ liczby ujemne, zero i liczby dodatnie) na wektor logiczny.

w9 = c(TRUE, FALSE)
as.numeric(w9)
as.character(w9)
w9 = -2:2
as.logical(w9)


#Zadanie_1.10
#Do sprawdzenia czy dany obiekt jest okreœlonego typu s³u¿¹ funkcje: 
#is.character(), is.double(), is.numeric(), is.integer(), is.logical(). 
#Korzysta siê z nich miedzy innymi przy sprawdzaniu poprawnoœci argumentów funkcji. 
#SprawdŸ dzia³anie tych funkcji na wektorze 1:4.


vector <-  1:4
is.character(vector)
is.double(vector)
is.numeric(vector)
is.integer(vector)
is.logical(vector)

#Zadanie_1.11
#Nazwij wektor z Zadania 2. przez x, a nastêpnie korzystaj¹c z tej nazwy 
#dokonaj konwersji na wektor numeryczny.

x <- rep(c(TRUE, FALSE, TRUE, FALSE), c(3,4,2,5))
as.integer(x)

#Zadanie_1.12
#W nazwach obiektów mo¿emy u¿ywaæ liter, cyfr, kropek i podkreœlników, 
#przy czym nazwy nie rozpoczynamy od cyfry, podkreœlnika oraz cyfry poprzedzonej kropk¹. 
#SprawdŸ to na kilku przyk³adach.

w12.1 = 1:3
w12_2 = 4:6
.w123 = 7:9

#Zadanie_1.13
#Z wektora letters wybierz litery na pozycjach 5, 10, 15, 20, 25.

letters[seq(5,25,by=5)]
#//
c(letters[5],letters[10],letters[15],letters[20],letters[25])

#Zadanie_1.14
#Utwórz wektor liczb naturalnych od 1 do 1000, 
#a nastêpnie zamieñ liczby parzyste na ich odwrotnoœci.

vector <- 1:1000
vector[vector %% 2 == 0] <- 1/vector[vector %% 2 == 0]
vector


#Zadanie_1.15
#Wyznacz znaki elementów wektora  (-1,876;-1,123;-0,123;0;0,123;1,123;1,876).
#Nastêpnie zaokr¹glij elementy tego wektora do dwóch miejsc po przecinku. 
#Na koniec wyznacz czêœæ ca³kowit¹ ka¿dego elementu nowego wektora.

vector <-  c(-1.876, -1.123, -0.123, 0, 0.123, 1.123, 1.876)
as.integer(vector)
round(vector, digits=2)
floor(vector)


#Zadanie_1.16
#Wyznacz pierwiastek kwadratowy z ka¿dej liczby naturalnej od 1 do 100 milionów. 
#Najpierw wykonaj to polecenie korzystaj¹c z odpowiedniej funkcji wbudowanej w R, 
#a nastêpnie wykorzystuj¹c potêgowanie. 
#Który sposób dzia³a szybciej? Wskazówka: 
#Do badania d³ugoœci czasu dzia³ania programu mo¿na wykorzystaæ funkcjê Sys.time().

start <-  Sys.time()
temp <-  sqrt(1:100000000)
end <-  Sys.time()
difftime(end, start)
start <-  Sys.time()
temp <-  (1:100000000)^0.5
end <-  Sys.time()
difftime(end, start)

#Zadanie_1.17
# W pakiecie schoolmath znajduje siê zbiór danych primlist, 
#który zawiera liczby pierwsze pomiêdzy 1 a 9999999.

    #ZnajdŸ najwiêksz¹ liczbê pierwsz¹ mniejsz¹ od 1000.
    #Ile jest liczb pierwszych wiêkszych od 100 a mniejszych od 500?

install.packages("schoolmath")
library(schoolmath)
data(primlist)
max(primlist[primlist < 1000])
length(primlist[primlist > 100 & primlist < 500])


#Zadanie_1.18
#Uporz¹dkuj elementy wektora (6, 3, 4, 5, 2, 3) 
#od najwiêkszego do najmniejszego wykorzystuj¹c funkcjê order().

vector <-  c(6,3,4,5,2,3)
vector[order(vector, decreasing=TRUE)]
--------------------------------
#Zadanie_1.19
#Utwórz wektor x o elementach NA, 3, 14, NA, 33, 17, NA, 41.
  
    #Oblicz liczbê braków danych.
    #Oblicz œredni¹ arytmetyczn¹ elementów tego wektora bez braków danych.
    #Usuñ braki danych.
    #Zamieñ braki danych na liczbê 11.
  
vector <-  c(NA, 3, 14, NA, 33, 17, NA, 41)
length(vector[is.na(vector)])
mean(vector[!is.na(vector)])
vector_2 = vector[!is.na(vector)]
vector[is.na(vector)] = 11
vector
----------------------------------
#Zadanie_1.20
#Skonstruuj listê o nazwie moja_lista, 
#której pierwszym elementem bêdzie dwuelementowy wektor napisów zawieraj¹cy: 
    #Twoje imiê i nazwisko, 
    #drugim elementem bêdzie liczba ??
    #trzecim funkcja s³u¿¹ca do obliczania pierwiastka kwadratowego 
    #ostatni element listy to wektor z³o¿ony z liczb 0,02;0,04;…;1.

#Nastêpnie usuñ elementy numer jeden i trzy z tej listy. 
#Na zakoñczenie, wyznacz listê zawieraj¹c¹ wartoœci funkcji gamma Eulera dla elementów listy moja_lista.
  
mojalista <- (list(c("Imie","Nazwisko"), pi, sqrt,seq(0.02,1,0.02)))
str(mojalista)
mojalista[c(1,3)] <-  NULL
str(mojalista)
sapply(mojalista,gamma)

#Zadanie_1.21
#Utwórz listê o nazwie list_0, której trzy elementy s¹ nastêpuj¹ce:

    #1.wektor liczb 1, 2, 3, 4, 5, NA, 7, 6, 5, 4, 3, NaN, Inf,
    #2.wektor napisów klient_1, klient_2, ..., klient_10,
    #3.wektor logiczny o d³ugoœci 11 i wartoœciach TRUE i FALSE pojawiaj¹cych siê na przemian.

#Nazwy powy¿szych trzech elementów powinny byæ nastêpuj¹ce: obserwacje, klienci, tak_nie. 
#Nastêpnie wyznacz liczbê unikatowych wartoœci w ka¿dym elemencie listy."

list_0 = list(obserwacje=c(1, 2, 3, 4, 5, NA, 7, 6, 5, 4, 3, NaN, Inf),klienci=paste(c('klient_'),1:10,sep=''),tak_nie=rep(c(TRUE,FALSE),length.out=11))
list_0

lapply(list_0,function(x) length(unique(x)))

#Zadanie_1.22
#Wyznacz: 
    #rz¹d,
    #wyznacznik,
    #odwrotnoœæ, 
    #wartoœci w³asne,
    #wektory w³asne oraz sumy i œrednie arytmetyczne dla kolejnych wierszy i kolumn dla nastêpuj¹cej macierzy:
    #|1 5 3|
    #|2 0 5|
    #|1 2 1|
#Ponadto, pomnó¿ tê macierz przez jej 
  
library(Matrix)
x <- cbind(c(1,2,1),c(5,0,2),c(3,5,1))
#//x <- matrix(c(1,5,3,2,0,5,1,2,1),ncol=3,nrow=3)
det(x)
rankMatrix(x)
eigen(x)
solve(x)
rowSums(x)
rowSums(x)/3
colSums(x)
colSums(x)/3
x %*% solve(x)

#Zadanie_1.23
#Skonstruuj macierz diagonaln¹ x o wymiarze 3×3 i elementach 4,1,2.
#Nastêpnie przemnó¿ (,,na dwa sposoby’’) 
#tê macierz przez macierz z poprzedniego zadania raz z lewej a raz z prawej strony. 
#Czy otrzymujemy takie same wyniki?"
a <-  cbind(c(1,2,1),c(5,0,2),c(3,5,1))
x <- matrix(c(4,0,0,0,1,0,0,0,2),ncol=3)
a
x
x*a
a*x 

#Zadanie_1.24
#Zmienna ftv w zbiorze danych birthwt z pakietu MASS, 
#zawiera liczbê wizyt matek u lekarza w pierwszym trymestrze ci¹¿y. 
#Przekszta³æ j¹ do czynnika o trzech poziomach 0, 1 oraz ,,2 lub wiêcej’’ (u¿yj funkcji factor() oraz levels()).

library(MASS)
data(birthwt)
head(birthwt)
tail(birthwt)
ftv <- factor(birthwt$ftv)
ftv
levels(ftv)[3:6] <- "wiecej niz 2"
levels(ftv)


#Zadanie_1.25
#Utwórz wektor kwadratów 100 pierwszych liczb naturalnych. 
#Nastêpnie zlicz, które cyfry oraz jak czêsto wystêpuj¹ na pozycji jednoœci w kolejnych elementach tego wektora."

kwardraty <- (1:100)^2
kwardraty
table(kwardraty %% 10)

#Zadanie_1.26
#Poni¿sze dane s¹ œrednimi miesiêcznymi temperaturami (w^oF) w Nowym Yorku.

    #Styczeñ-32,Luty-33,Luty-33,Marzec-41,Kwiecieñ-52,Maj-62,Czerwiec-72
    #Lipiec-77,Sierpieñ-75,Wrzesieñ-68,PaŸdziernik-58,Listopad-47,Listopad-47,Grudzieñ-35

    #1.WprowadŸ te dane do ramki danych o jednej zmiennej NY_F.
    #2.Utwórz now¹ zmienn¹ NY_C podaj¹c¹ temperaturê w stopniach Celsiusza (zaokr¹glon¹ do dwóch miejsc po przecinku). 
    #Wskazówka:(xoF)=(x-32)·5/9(oC)
    #3.Zamieñ nazwy kolumn na NY_Fahrenheit i NY_Celsiusz.
    #4.Usuñ kolumnê z temperatur¹ w Fahrenheitach.

zad26 <- data.frame(NYF = c(32,33,41,52,62,72,77,75,68,58,47,35))
colnames(zad26)[1] <-  "NY_F"
row.names(zad26) <- c("Styczeñ","Luty","Marzec","Kwiecieñ","Maj","Czerwiec","Lipiec","
Sierpieñ","Wrzesieñ","PaŸdziernik","Listopad","Grudzieñ")
zad26
zad26$NY_C <- (zad26$NY_F -32)*(5/9)
zad26
colnames(zad26)[1] <- "NY_Fahrenheit"
colnames(zad26)[2] <- "NY_Celsiusz"
zad26
zad26$NY_Fahrenheit <- NULL
zad26
write.table(zad26, "NYtemp.RData")

#Zadanie_1.27
#SprawdŸ, które zmienne w zbiorze danych Cars93 (sprzeda¿ samochodów w USA w roku 1993) z pakietu MASS s¹ czynnikami. 
#Ponadto, wyznacz w postaci tabeli liczbê samochodów dla miejsca pochodzenia (zmienna Origin) oraz rodzaju samochodu (zmienna Type)"

data("Cars93")
sapply(Cars93,is.factor)
table(Cars93$Origin, Cars93$Type)

#Zadanie_1.28
#Odczytaj zbiór danych dane1.csv a nastêpnie:
    #1.Z odczytanej ramki danych wyœwietl tylko parzyste wiersze.
    #2.Jakie s¹ mo¿liwe wartoœci zmiennej Rozmiar.guza i ile razy wystêpuje ka¿da z nich?
    #3.Korzystaj¹c z operatorów logicznych wyœwietl tylko wiersze odpowiadaj¹ce pacjentkom starszym ni¿ 50 lat z przerzutami do wêz³ów ch³onnych (Wezly.chlonne = 1).
    #4.Wyznacz, dla ka¿dej zmiennej z osobna, liczbê braków danych."

w <- read.table("http://ls.home.amu.edu.pl/data_sets/dane1.csv", 
                header = TRUE , sep = ";")
w
w[1:nrow(w)%%2 == 0, ]
table(w$Rozmiar.guza)
w[w$Wiek >50 & w$Wezly.chlonne == 1,]
sapply(w, function(x) sum(is.na(x)))
--------------------------------------------------------------------
#Zadanie_1.29
#Odczytaj dane z pliku daneBiotechnologia.csv. Zapisz te dane pod nazw¹ dane_biotech.
#Wybierz z tej ramki danych dziesiêæ pierwszych wierszy i trzy kolejne kolumny. 
#Zapisz ten fragment danych do pliku male_dane_biotechn.txt w dowolnym miejscu na dysku. 
#Kolejne pola rozdzielaj znakiem tabulacji a kropk¹ dziesiêtn¹ ma byæ kropka. Ponadto, dla danych w ramce danych dane_biotech wykonaj nastêpuj¹ce polecenia:
    #1.Zamieñ dane liczbowe z kolumny Wiek na zmienn¹ czynnikow¹, dziel¹c pacjentki na trzy grupy: o wieku do 45 lat, o wieku powy¿ej 55 lat i o wieku poœrednim. 
      #Poziomy w tej zmiennej powinny nazywaæ siê nastêpuj¹co: wiek<45, 45<=wiek<=55, wiek>55.
    #2.Nastêpnie wyœwietl macierz kontyngencji dla zmiennej z poprzedniego podpunktu.
    #3.Wyœwietl równie¿ macierz kontyngencji dla pary zmiennych czynnikowych, uzyskanych na bazie wieku (podpunkt 1) oraz dla p³ci. 
     #Dodaj do niej sumy brzegowe.

dane_biotech<- read.table("http://ls.home.amu.edu.pl/data_sets/daneBiotechnologia.csv", 
                          header = TRUE , sep = ",", dec= ",")
dane_biotech
View(dane_biotech)
write.table(dane_biotech[1:10,1:3],
            "male_dane_biotech.txt",
            sep = "\t", dec =",")
w <- cut(dane_biotech$Wiek,
         breaks = c(-Inf,45,55,Inf),
         labels = c("wiek<45","45<=wiek<=55","wiek>55"))
w
table(w)
table(dane_biotech$Plec,w)
addmargins(table(dane_biotech$Plec,w))

#Zadanie_1.30
# Zapisz otrzymane w punkcie 4. zadania 18 dane w pliku NY_temp.RData.

data_frame <- data.frame(NY_F = c(32,33,41,52,62,72,77,75,68,58,47,35))
data_frame
rownames(data_frame) <- c("Styczeñ", "Luty", "Marzec", "Kwiecieñ", "Maj","Czerwiec","Lipiec","Sierpieñ","Wrzesieñ","PaŸdziernik","Listopad","Grudzieñ")
data_frame
data_frame$NY_C <- round((data_frame$NY_F -32)*5/9,2)
data_frame 
colnames(data_frame) <- c("NY_Fahrenheit", "NY_Celsiusz")
data_frame
data_frame$NY_Fahrenheit <- NULL
data_frame

save(data_frame, file ="NY_temp.RData")