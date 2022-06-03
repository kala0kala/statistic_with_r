
# Zadanie 8.1 ---------------------------------------------------------------
library(rcompanion)
# Przeprowadzono badanie zale¿noœci pomiêdzy czasem dojazdu do szko³y, a uzyskiwanymi wynikami w nauce. 
# Wyniki badania losowo wybranych uczniów szkó³ œrednich zebrano w pliku Szkola.txt.
# 1.Przedstaw rozk³ad ³¹czny czasu dojazdu i wyników w nauce w postaci tablicy dwudzielczej. Przyjmij nastêpuj¹c¹ kategoryzacjê zmiennej ‘czas’: 
#   1 oznacza czas dojazdu poni¿ej 1 godziny, 
#   2 czas dojazdu pomiêdzy 1 a 2 godzinami, 
#   3 czas dojazdu powy¿ej 2 godzin.
# 
# 2.Przedstaw rozk³ad warunkowy wyników w nauce ze wzglêdu na czas dojazdu do szko³y w postaci wykresów s³upkowych.
# 3.Opisz si³ê zale¿noœci pomiêdzy badanymi cechami za pomoc¹ wspó³czynnika V-Cramera.
# 4.Czy pomiêdzy czasem dojazdu do szko³y, a wynikami w nauce istnieje istotna zale¿noœæ? 
#   Wykonaj odpowiednie procedury testowe, przyjmuj¹c poziom istotnoœci 0.05.

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
# V jest równe 0,1799, oznacza to s³aby zwi¹zek miêdzy zmiennymi

#V< 0,3 – s³aby zwi¹zek
#V< 0,5 – umiarkowany zwi¹zek
#V> 0,5 – silny zwi¹zek

### pkt.4 ###
chisq.test(szkola_m) #pearson
#prawdopodobieñstwo (p-value) jest wiêksze od 0,05, oznacza to, ¿e nie stnieje istotny zwi¹zek pomiêdzy wynikami w nauce, a czasem dojazdu do szko³y
# jeœli prawdopodobieñstwo by³oby mniejsze lub rówwne 0,05, to przechodzimy do analizy
fisher.test(szkola_m, alternative = "greater",conf.level=0.95) #fisher


# Zadanie 8.2 ---------------------------------------------------------------
# Zbiór danych mtcars zawiera dane dotycz¹ce pewnych cech samochodów. Interesuje nas zbadanie korelacji miêdzy zmiennymi mpg i wg.
# 1.Wykonaj wykres rozrzutu dla badanych cech.
# 2.SprawdŸ za³o¿enia testu istotnoœci dla wspó³czynnika korelacji.
# 3.Wykonaj test istotnoœci dla wspó³czynnika korelacji dla zmiennych mpg i wg. 
#   Oszacuj punktowo i przedzia³owo wspó³czynnik korelacji.


library(rcompanion) #biblioteka potrzebna do korelacji 
### pkt.1 ###
mtcars = mtcars

par(mfrow=c(1,1))
plot(mtcars$mpg,mtcars$wt)

### pkt.2 ###
shapiro.test(mtcars$mpg)
# mo¿emy stwierdziæ, ¿e b³êdy losowe maj¹ rozk³ad normalny
shapiro.test(mtcars$wt)
# mo¿emy stwierdziæ, ¿e b³êdy losowe maj¹ rozk³ad normalny

qqnorm(mtcars$wt)
qqline(mtcars$wt, col = "red")

qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")

### pkt.3 ###
cor.test(mtcars$mpg,mtcars$wt, method = "pearson")

#Nie istnieje istotny zwi¹zek pomiêdzy zmiennymi,
#gdyby p-value<=0,05, istnia³aby silna korelacja lewostronna

# Zadanie 8.3 ---------------------------------------------------------------
# Zbiór danych zawarty w pliku braking.RData zawiera informacje o d³ugoœci drogi hamowania przy danej prêdkoœci okreœlonego modelu samochodu. 
# W tym zbiorze danych wystêpuje obserwacja odstaj¹ca. 
# 1.Zidentyfikuj j¹ za pomoc¹ wykresu rozrzutu. 
# 2.Zbadaj wystêpowanie korelacji miêdzy d³ugoœci¹ drogi hamowania a prêdkoœci¹ przy u¿yciu pe³nych danych i danych bez obserwacji odstaj¹cej. 
# 3.Dok³adniej wykonaj polecenia Zadania 1 w stosunku do odpowiednich zmiennych.

load('braking.RData')
#pe³ne dane
plot(braking$speed, braking$distance, 
     xlab = "speed", ylab = "distance", pch = 16)

shapiro.test(braking$speed)
shapiro.test(braking$distance)
#Mo¿emy stwierdziæ, ¿e obie zmienne maj¹ rozk³ad normalny

qqnorm(braking$speed)
qqline(braking$speed, col = "red")

qqnorm(braking$distance)
qqline(braking$distance, col = "red")

cor(braking$speed,braking$distance)
cor.test(braking$speed,braking$distance,method="pearson")
#Nie istnieje istotny zwi¹zek pomiêdzy zmiennymi,


#usuniêcie zmiennych odstaj¹cych

braking2 = braking[-c(27),] #usuwa wiersz 27
braking2

plot(braking2$speed, braking2$distance, 
     xlab = "speed", ylab = "distance", pch = 16)

shapiro.test(braking2$speed)
#posiada rozk³¹d normalny

shapiro.test(braking2$distance)
#nie posiada rozk³adu normalnego

qqnorm(braking2$speed)
qqline(braking2$speed, col = "red")

qqnorm(braking2$distance)
qqline(braking2$distance, col = "red")

cor(braking2$speed, braking2$distance)
cor.test(braking2$speed, braking2$distance,method="pearson")
#Nie istnieje istotny zwi¹zek pomiêdzy zmiennymi,

# Zadanie 8.4 -------------------------------------------------------------
# SprawdŸ wspomnian¹ powy¿ej niezmienniczoœæ wspó³czynnika korelacji liniowej Pearsona na wybranym zbiorze danych.

