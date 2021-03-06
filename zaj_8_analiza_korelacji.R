
# Zadanie 8.1 ---------------------------------------------------------------
library(rcompanion)
# Przeprowadzono badanie zale�no�ci pomi�dzy czasem dojazdu do szko�y, a uzyskiwanymi wynikami w nauce. 
# Wyniki badania losowo wybranych uczni�w szk� �rednich zebrano w pliku Szkola.txt.
# 1.Przedstaw rozk�ad ��czny czasu dojazdu i wynik�w w nauce w postaci tablicy dwudzielczej. Przyjmij nast�puj�c� kategoryzacj� zmiennej �czas�: 
#   1 oznacza czas dojazdu poni�ej 1 godziny, 
#   2 czas dojazdu pomi�dzy 1 a 2 godzinami, 
#   3 czas dojazdu powy�ej 2 godzin.
# 
# 2.Przedstaw rozk�ad warunkowy wynik�w w nauce ze wzgl�du na czas dojazdu do szko�y w postaci wykres�w s�upkowych.
# 3.Opisz si�� zale�no�ci pomi�dzy badanymi cechami za pomoc� wsp�czynnika V-Cramera.
# 4.Czy pomi�dzy czasem dojazdu do szko�y, a wynikami w nauce istnieje istotna zale�no��? 
#   Wykonaj odpowiednie procedury testowe, przyjmuj�c poziom istotno�ci 0.05.

szkola <-  read.csv('http://ls.home.amu.edu.pl/data_sets_dpas/Szkola.txt',sep='\t')

### pkt.1 ###
szkola$czas_dojazdu = cut(szkola$czas_dojazdu,breaks=c(0,59,120,180))

levels(szkola$czas_dojazdu) = c("ponizej 1 godziny","pomiedzy 1 a 2 godzinami",
                                "powyzej 2 godzin")
szkola$wyniki_w_nauce = as.factor(szkola$wyniki_w_nauce)
levels(szkola$wyniki_w_nauce) = c('Slaby uczen','Sredni uczen','Dobry uczen')

table(szkola$czas_dojazdu,szkola$wyniki_w_nauce)
addmargins(table(szkola$czas_dojazdu,szkola$wyniki_w_nauce))

#### pkt.2 ###
d1 = subset(szkola,czas_dojazdu == "ponizej 1 godziny")
d2 = subset(szkola,czas_dojazdu == "pomiedzy 1 a 2 godzinami")
d3 = subset(szkola,czas_dojazdu == "powyzej 2 godzin")

par(mfrow = c(1,3))
barplot(table(d1$wyniki_w_nauce),main="ponizej 1 godziny",ylim=c(0, 20))
barplot(table(d2$wyniki_w_nauce),main="pomiedzy 1 a 2 godzinami",ylim=c(0, 20))
barplot(table(d3$wyniki_w_nauce),main="powyzej 2 godzin",ylim=c(0, 20))

### pkt.3 ###
szkola_m = as.matrix(table(szkola))
szkola_m
cramerV(szkola_m)
# V jest r�wne 0,1799, oznacza to s�aby zwi�zek mi�dzy zmiennymi

#V< 0,3 � s�aby zwi�zek
#V< 0,5 � umiarkowany zwi�zek
#V> 0,5 � silny zwi�zek

### pkt.4 ###
chisq.test(szkola_m) #pearson
#prawdopodobie�stwo (p-value) jest wi�ksze od 0,05, oznacza to, �e nie stnieje istotny zwi�zek pomi�dzy wynikami w nauce, a czasem dojazdu do szko�y
# je�li prawdopodobie�stwo by�oby mniejsze lub r�wwne 0,05, to przechodzimy do analizy
fisher.test(szkola_m, alternative = "greater",conf.level=0.95) #fisher


# Zadanie 8.2 ---------------------------------------------------------------
# Zbi�r danych mtcars zawiera dane dotycz�ce pewnych cech samochod�w. Interesuje nas zbadanie korelacji mi�dzy zmiennymi mpg i wg.
# 1.Wykonaj wykres rozrzutu dla badanych cech.
# 2.Sprawd� za�o�enia testu istotno�ci dla wsp�czynnika korelacji.
# 3.Wykonaj test istotno�ci dla wsp�czynnika korelacji dla zmiennych mpg i wg. 
#   Oszacuj punktowo i przedzia�owo wsp�czynnik korelacji.


library(rcompanion) #biblioteka potrzebna do korelacji 
### pkt.1 ###
mtcars = mtcars

par(mfrow=c(1,1))
plot(mtcars$mpg,mtcars$wt)

### pkt.2 ###
shapiro.test(mtcars$mpg)
# mo�emy stwierdzi�, �e b��dy losowe maj� rozk�ad normalny
shapiro.test(mtcars$wt)
# mo�emy stwierdzi�, �e b��dy losowe maj� rozk�ad normalny

qqnorm(mtcars$wt)
qqline(mtcars$wt, col = "red")

qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")

### pkt.3 ###
cor.test(mtcars$mpg,mtcars$wt, method = "pearson")

#Nie istnieje istotny zwi�zek pomi�dzy zmiennymi,
#gdyby p-value<=0,05, istnia�aby silna korelacja lewostronna

# Zadanie 8.3 ---------------------------------------------------------------
# Zbi�r danych zawarty w pliku braking.RData zawiera informacje o d�ugo�ci drogi hamowania przy danej pr�dko�ci okre�lonego modelu samochodu. 
# W tym zbiorze danych wyst�puje obserwacja odstaj�ca. 
# 1.Zidentyfikuj j� za pomoc� wykresu rozrzutu. 
# 2.Zbadaj wyst�powanie korelacji mi�dzy d�ugo�ci� drogi hamowania a pr�dko�ci� przy u�yciu pe�nych danych i danych bez obserwacji odstaj�cej. 
# 3.Dok�adniej wykonaj polecenia Zadania 1 w stosunku do odpowiednich zmiennych.

load('braking.RData')
#pe�ne dane
plot(braking$speed, braking$distance, 
     xlab = "speed", ylab = "distance", pch = 16)

shapiro.test(braking$speed)
#Mo�emy stwierdzi�, �e zmienna ma rozk�ad normalny
shapiro.test(braking$distance)
#Nie mo�emy stwierdzi�, �e zmienna ma rozk�ad normalny

qqnorm(braking$speed)
qqline(braking$speed, col = "red")

qqnorm(braking$distance)
qqline(braking$distance, col = "red")

cor(braking$speed,braking$distance)
cor.test(braking$speed,braking$distance,method="pearson")
#Istnieje silny zwi�zek prawostronny


#usuni�cie zmiennych odstaj�cych

braking2 = braking[-c(27),] #usuwa wiersz 27
braking2

plot(braking2$speed, braking2$distance, 
     xlab = "speed", ylab = "distance", pch = 16)

shapiro.test(braking2$speed)
#posiada rozk��d normalny

shapiro.test(braking2$distance)
#nie posiada rozk�adu normalnego

qqnorm(braking2$speed)
qqline(braking2$speed, col = "red")

qqnorm(braking2$distance)
qqline(braking2$distance, col = "red")

cor(braking2$speed, braking2$distance)
cor.test(braking2$speed, braking2$distance,method="pearson")
#Istnieje silny zwi�zek prawostronny

# Zadanie 8.4 -------------------------------------------------------------
# Sprawd� wspomnian� powy�ej niezmienniczo�� wsp�czynnika korelacji liniowej Pearsona na wybranym zbiorze danych.

