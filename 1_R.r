#____________________________________1_________________________________________
#Zadanie_1.1
# Otw�rz program RStudio. Nast�pnie utw�rz nowy skrypt i zapisz go jako, na przyk�ad,
#wprowadzenie_do_R_zadania.R. W tym skrypcie mo�esz napisa� rozwi�zania kolejnych zada�.

CTRL + SHIFT + N


#Zadanie_1.2
#U�yj funkcji rep(), aby utworzy� wektor logiczny, 
#zaczynaj�c od trzech warto�ci prawda, nast�pnie czterech warto�ci fa�sz, 
#po kt�rych nast�puj� dwie warto�ci prawda i wreszcie pi�� warto�ci fa�sz.

x <-  c(rep(TRUE,3), rep(FALSE,4), rep(TRUE,2), rep(FALSE,5))
x

#Zadanie_1.3
#Palindromem nazywamy wektor, kt�rego elementy czytane od ko�ca tworz� ten sam wektor co elementy czytane od pocz�tku. 
#Utw�rz taki wektor 100 liczb przy czym pierwsze 20 liczb to kolejne liczby naturalne, nast�pnie wyst�puje 10 zer, 
#nast�pnie 20 kolejnych liczb parzystych, a pozosta�e elementy okre�lone s� przez palindromiczno�� (warunek symetrii).

vector <-  c(1:20, rep(0,10), seq(2,40,by=2))
vector <-  c(vector, rev(vector))
vector

#Zadanie_1.4
#Czy w programie R dzia�a mechanizm cz�ciowego dopasowania (ang. partial matching) 
#lub autouzupe�nienie, tj. nie trzeba (zazwyczaj) odwo�ywa� si� do pe�nych nazw argument�w funkcji przy ich wywo�ywaniu? 
#Sprawd� to na wybranym przyk�adzie funkcji j�zyka R.

rep(TRUE, times=3)
rep(TRUE, t=3)

#Zadanie_1.5
#Utw�rz wektor 30 napis�w nast�puj�cej postaci: liczba.litera, 
#gdzie liczba to kolejne liczby naturalne od 1 do 30 a litera to trzy wielkie litery X, Y, Z wyst�puj�ce cyklicznie.

paste(1:30, c("X","Y","Z"), sep=".")


#Zadanie_1.6
#Wyznacz wszystkie kombinacje warto�ci wektor�w (a,b) i (1,2,3)
#za pomoc� funkcji rep() i paste()

vector1 <-  c("a","b")
vector2 <-  c(1,2,3)
c(paste(rep(vector1[1], length(vector2)), vector2, sep=""),
  paste(rep(vector1[2], length(vector2)), vector2, sep=""))

#Zadanie_1.7
#Sprawd� typ nast�puj�cych obiekt�w j�zyka R:

typeof(FALSE); mode(FALSE)
typeof(1); mode(1)
typeof(1L); mode(1L)
typeof(2.2); mode(2.2)
typeof("statystyka"); mode("statystyka")
typeof(list(TRUE, 2L, 3.5, 2i, "statystyka")); mode(list(TRUE, 2L, 3.5, 2i, "statystyka"))


#Zadanie_1.8
#W j�zyku R mamy hierarchi� typ�w dla wektor�w atomowych. 
#Przechowuj� one elementy jednego �ci�le okre�lonego typu (maj� jednorodn� struktur�). 
#Gdy w wektorze pojawi� si� elementy r�nego typu, R uzgodni ich typ tak, 
#aby informacj� o najbardziej ,,og�lnym�� z obiekt�w da�o si� przechowa� bez znacz�cej straty. 
#Zbadaj jak dzia�a hierarchia typ�w w R zbieraj�c w jeden wektor:

    #warto�� logiczn�, ca�kowitoliczbow�, rzeczywistoliczbow�, napis,
    #warto�� logiczn�, ca�kowitoliczbow�, rzeczywistoliczbow�,
    #warto�� logiczn�, ca�kowitoliczbow�.

c(TRUE, 2, 1.5, "statystyka")
c(TRUE, 2, 1.5)
c(TRUE, 2)

#Zadanie_1.9
#Wektory danego typu mo�na rzutowa� (rzutowanie typ�w) na inny typ za pomoc� funkcji: 
#as.character(), as.double(), as.numeric(), as.integer(), as.logical(). 
#Mo�emy rzutowa� wektory og�lniejszego typu do bardziej szczeg�owego. 
#Warto�ci logiczne TRUE i FALSE liczbowo s� zawsze przedstawione jako 1 i 0, odpowiednio. 
#Warto�� liczbowa r�na od zera jest zawsze rzutowana do warto�ci logicznej TRUE. Dokonaj nast�puj�cych rzutowa�:

    #wektora logicznego na wektor numeryczny,
    #wektora logicznego na wektor napis�w,
    #wektora liczbowego (ma zawiera� liczby ujemne, zero i liczby dodatnie) na wektor logiczny.

w9 = c(TRUE, FALSE)
as.numeric(w9)
as.character(w9)
w9 = -2:2
as.logical(w9)


#Zadanie_1.10
#Do sprawdzenia czy dany obiekt jest okre�lonego typu s�u�� funkcje: 
#is.character(), is.double(), is.numeric(), is.integer(), is.logical(). 
#Korzysta si� z nich miedzy innymi przy sprawdzaniu poprawno�ci argument�w funkcji. 
#Sprawd� dzia�anie tych funkcji na wektorze 1:4.


vector <-  1:4
is.character(vector)
is.double(vector)
is.numeric(vector)
is.integer(vector)
is.logical(vector)

#Zadanie_1.11
#Nazwij wektor z Zadania 2. przez x, a nast�pnie korzystaj�c z tej nazwy 
#dokonaj konwersji na wektor numeryczny.

x <- rep(c(TRUE, FALSE, TRUE, FALSE), c(3,4,2,5))
as.integer(x)

#Zadanie_1.12
#W nazwach obiekt�w mo�emy u�ywa� liter, cyfr, kropek i podkre�lnik�w, 
#przy czym nazwy nie rozpoczynamy od cyfry, podkre�lnika oraz cyfry poprzedzonej kropk�. 
#Sprawd� to na kilku przyk�adach.

w12.1 = 1:3
w12_2 = 4:6
.w123 = 7:9

#Zadanie_1.13
#Z wektora letters wybierz litery na pozycjach 5, 10, 15, 20, 25.

letters[seq(5,25,by=5)]
#//
c(letters[5],letters[10],letters[15],letters[20],letters[25])

#Zadanie_1.14
#Utw�rz wektor liczb naturalnych od 1 do 1000, 
#a nast�pnie zamie� liczby parzyste na ich odwrotno�ci.

vector <- 1:1000
vector[vector %% 2 == 0] <- 1/vector[vector %% 2 == 0]
vector


#Zadanie_1.15
#Wyznacz znaki element�w wektora  (-1,876;-1,123;-0,123;0;0,123;1,123;1,876).
#Nast�pnie zaokr�glij elementy tego wektora do dw�ch miejsc po przecinku. 
#Na koniec wyznacz cz�� ca�kowit� ka�dego elementu nowego wektora.

vector <-  c(-1.876, -1.123, -0.123, 0, 0.123, 1.123, 1.876)
as.integer(vector)
round(vector, digits=2)
floor(vector)


#Zadanie_1.16
#Wyznacz pierwiastek kwadratowy z ka�dej liczby naturalnej od 1 do 100 milion�w. 
#Najpierw wykonaj to polecenie korzystaj�c z odpowiedniej funkcji wbudowanej w R, 
#a nast�pnie wykorzystuj�c pot�gowanie. 
#Kt�ry spos�b dzia�a szybciej? Wskaz�wka: 
#Do badania d�ugo�ci czasu dzia�ania programu mo�na wykorzysta� funkcj� Sys.time().

start <-  Sys.time()
temp <-  sqrt(1:100000000)
end <-  Sys.time()
difftime(end, start)
start <-  Sys.time()
temp <-  (1:100000000)^0.5
end <-  Sys.time()
difftime(end, start)

#Zadanie_1.17
# W pakiecie schoolmath znajduje si� zbi�r danych primlist, 
#kt�ry zawiera liczby pierwsze pomi�dzy 1 a 9999999.

    #Znajd� najwi�ksz� liczb� pierwsz� mniejsz� od 1000.
    #Ile jest liczb pierwszych wi�kszych od 100 a mniejszych od 500?

install.packages("schoolmath")
library(schoolmath)
data(primlist)
max(primlist[primlist < 1000])
length(primlist[primlist > 100 & primlist < 500])


#Zadanie_1.18
#Uporz�dkuj elementy wektora (6, 3, 4, 5, 2, 3) 
#od najwi�kszego do najmniejszego wykorzystuj�c funkcj� order().

vector <-  c(6,3,4,5,2,3)
vector[order(vector, decreasing=TRUE)]
--------------------------------
#Zadanie_1.19
#Utw�rz wektor x o elementach NA, 3, 14, NA, 33, 17, NA, 41.
  
    #Oblicz liczb� brak�w danych.
    #Oblicz �redni� arytmetyczn� element�w tego wektora bez brak�w danych.
    #Usu� braki danych.
    #Zamie� braki danych na liczb� 11.
  
vector <-  c(NA, 3, 14, NA, 33, 17, NA, 41)
length(vector[is.na(vector)])
mean(vector[!is.na(vector)])
vector_2 = vector[!is.na(vector)]
vector[is.na(vector)] = 11
vector
----------------------------------
#Zadanie_1.20
#Skonstruuj list� o nazwie moja_lista, 
#kt�rej pierwszym elementem b�dzie dwuelementowy wektor napis�w zawieraj�cy: 
    #Twoje imi� i nazwisko, 
    #drugim elementem b�dzie liczba ??
    #trzecim funkcja s�u��ca do obliczania pierwiastka kwadratowego 
    #ostatni element listy to wektor z�o�ony z liczb 0,02;0,04;�;1.

#Nast�pnie usu� elementy numer jeden i trzy z tej listy. 
#Na zako�czenie, wyznacz list� zawieraj�c� warto�ci funkcji gamma Eulera dla element�w listy moja_lista.
  
mojalista <- (list(c("Imie","Nazwisko"), pi, sqrt,seq(0.02,1,0.02)))
str(mojalista)
mojalista[c(1,3)] <-  NULL
str(mojalista)
sapply(mojalista,gamma)

#Zadanie_1.21
#Utw�rz list� o nazwie list_0, kt�rej trzy elementy s� nast�puj�ce:

    #1.wektor liczb 1, 2, 3, 4, 5, NA, 7, 6, 5, 4, 3, NaN, Inf,
    #2.wektor napis�w klient_1, klient_2, ..., klient_10,
    #3.wektor logiczny o d�ugo�ci 11 i warto�ciach TRUE i FALSE pojawiaj�cych si� na przemian.

#Nazwy powy�szych trzech element�w powinny by� nast�puj�ce: obserwacje, klienci, tak_nie. 
#Nast�pnie wyznacz liczb� unikatowych warto�ci w ka�dym elemencie listy."

list_0 = list(obserwacje=c(1, 2, 3, 4, 5, NA, 7, 6, 5, 4, 3, NaN, Inf),klienci=paste(c('klient_'),1:10,sep=''),tak_nie=rep(c(TRUE,FALSE),length.out=11))
list_0

lapply(list_0,function(x) length(unique(x)))

#Zadanie_1.22
#Wyznacz: 
    #rz�d,
    #wyznacznik,
    #odwrotno��, 
    #warto�ci w�asne,
    #wektory w�asne oraz sumy i �rednie arytmetyczne dla kolejnych wierszy i kolumn dla nast�puj�cej macierzy:
    #|1 5 3|
    #|2 0 5|
    #|1 2 1|
#Ponadto, pomn� t� macierz przez jej 
  
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
#Skonstruuj macierz diagonaln� x o wymiarze 3�3 i elementach 4,1,2.
#Nast�pnie przemn� (,,na dwa sposoby��) 
#t� macierz przez macierz z poprzedniego zadania raz z lewej a raz z prawej strony. 
#Czy otrzymujemy takie same wyniki?"
a <-  cbind(c(1,2,1),c(5,0,2),c(3,5,1))
x <- matrix(c(4,0,0,0,1,0,0,0,2),ncol=3)
a
x
x*a
a*x 

#Zadanie_1.24
#Zmienna ftv w zbiorze danych birthwt z pakietu MASS, 
#zawiera liczb� wizyt matek u lekarza w pierwszym trymestrze ci��y. 
#Przekszta�� j� do czynnika o trzech poziomach 0, 1 oraz ,,2 lub wi�cej�� (u�yj funkcji factor() oraz levels()).

library(MASS)
data(birthwt)
head(birthwt)
tail(birthwt)
ftv <- factor(birthwt$ftv)
ftv
levels(ftv)[3:6] <- "wiecej niz 2"
levels(ftv)


#Zadanie_1.25
#Utw�rz wektor kwadrat�w 100 pierwszych liczb naturalnych. 
#Nast�pnie zlicz, kt�re cyfry oraz jak cz�sto wyst�puj� na pozycji jedno�ci w kolejnych elementach tego wektora."

kwardraty <- (1:100)^2
kwardraty
table(kwardraty %% 10)

#Zadanie_1.26
#Poni�sze dane s� �rednimi miesi�cznymi temperaturami (w^oF) w Nowym Yorku.

    #Stycze�-32,Luty-33,Luty-33,Marzec-41,Kwiecie�-52,Maj-62,Czerwiec-72
    #Lipiec-77,Sierpie�-75,Wrzesie�-68,Pa�dziernik-58,Listopad-47,Listopad-47,Grudzie�-35

    #1.Wprowad� te dane do ramki danych o jednej zmiennej NY_F.
    #2.Utw�rz now� zmienn� NY_C podaj�c� temperatur� w stopniach Celsiusza (zaokr�glon� do dw�ch miejsc po przecinku). 
    #Wskaz�wka:(xoF)=(x-32)�5/9(oC)
    #3.Zamie� nazwy kolumn na NY_Fahrenheit i NY_Celsiusz.
    #4.Usu� kolumn� z temperatur� w Fahrenheitach.

zad26 <- data.frame(NYF = c(32,33,41,52,62,72,77,75,68,58,47,35))
colnames(zad26)[1] <-  "NY_F"
row.names(zad26) <- c("Stycze�","Luty","Marzec","Kwiecie�","Maj","Czerwiec","Lipiec","
Sierpie�","Wrzesie�","Pa�dziernik","Listopad","Grudzie�")
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
#Sprawd�, kt�re zmienne w zbiorze danych Cars93 (sprzeda� samochod�w w USA w roku 1993) z pakietu MASS s� czynnikami. 
#Ponadto, wyznacz w postaci tabeli liczb� samochod�w dla miejsca pochodzenia (zmienna Origin) oraz rodzaju samochodu (zmienna Type)"

data("Cars93")
sapply(Cars93,is.factor)
table(Cars93$Origin, Cars93$Type)

#Zadanie_1.28
#Odczytaj zbi�r danych dane1.csv a nast�pnie:
    #1.Z odczytanej ramki danych wy�wietl tylko parzyste wiersze.
    #2.Jakie s� mo�liwe warto�ci zmiennej Rozmiar.guza i ile razy wyst�puje ka�da z nich?
    #3.Korzystaj�c z operator�w logicznych wy�wietl tylko wiersze odpowiadaj�ce pacjentkom starszym ni� 50 lat z przerzutami do w�z��w ch�onnych (Wezly.chlonne = 1).
    #4.Wyznacz, dla ka�dej zmiennej z osobna, liczb� brak�w danych."

w <- read.table("http://ls.home.amu.edu.pl/data_sets/dane1.csv", 
                header = TRUE , sep = ";")
w
w[1:nrow(w)%%2 == 0, ]
table(w$Rozmiar.guza)
w[w$Wiek >50 & w$Wezly.chlonne == 1,]
sapply(w, function(x) sum(is.na(x)))
--------------------------------------------------------------------
#Zadanie_1.29
#Odczytaj dane z pliku daneBiotechnologia.csv. Zapisz te dane pod nazw� dane_biotech.
#Wybierz z tej ramki danych dziesi�� pierwszych wierszy i trzy kolejne kolumny. 
#Zapisz ten fragment danych do pliku male_dane_biotechn.txt w dowolnym miejscu na dysku. 
#Kolejne pola rozdzielaj znakiem tabulacji a kropk� dziesi�tn� ma by� kropka. Ponadto, dla danych w ramce danych dane_biotech wykonaj nast�puj�ce polecenia:
    #1.Zamie� dane liczbowe z kolumny Wiek na zmienn� czynnikow�, dziel�c pacjentki na trzy grupy: o wieku do 45 lat, o wieku powy�ej 55 lat i o wieku po�rednim. 
      #Poziomy w tej zmiennej powinny nazywa� si� nast�puj�co: wiek<45, 45<=wiek<=55, wiek>55.
    #2.Nast�pnie wy�wietl macierz kontyngencji dla zmiennej z poprzedniego podpunktu.
    #3.Wy�wietl r�wnie� macierz kontyngencji dla pary zmiennych czynnikowych, uzyskanych na bazie wieku (podpunkt 1) oraz dla p�ci. 
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
rownames(data_frame) <- c("Stycze�", "Luty", "Marzec", "Kwiecie�", "Maj","Czerwiec","Lipiec","Sierpie�","Wrzesie�","Pa�dziernik","Listopad","Grudzie�")
data_frame
data_frame$NY_C <- round((data_frame$NY_F -32)*5/9,2)
data_frame 
colnames(data_frame) <- c("NY_Fahrenheit", "NY_Celsiusz")
data_frame
data_frame$NY_Fahrenheit <- NULL
data_frame

save(data_frame, file ="NY_temp.RData")