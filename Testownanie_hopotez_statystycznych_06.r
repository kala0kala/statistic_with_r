# TESTOWANIE HIPOTEZ STATYSTYCZNYCH
# Zadanie 1
#W pewnym regionie wykonano dziesi�� niezale�nych pomiar�w g��boko�ci morza i uzyskano nast�puj�ce wyniki: 862, 870, 876, 866, 871, 865, 861, 873, 871, 872.
#Na poziomie istotno�ci ??= 0,05 zweryfikuj hipotez�, �e �rednia g��boko�� morza w tym regionie wynosi  870m
x <- c(862,870,876,866,871,865,861,873,871,872)
shapiro.test(x)
#W = 0.93936, p-value = 0.5459 > 0.05 = alpha
#Nie ma podstaw do odrzucenia hipotezy zerowej
#Badana cecha ma rozk�ad normalny
qqnorm(x)
qqline(x)
# H0: mu = 0
# H1: mu < 870
mean(x)
t.test(x, mu = 870, alternative = "less")
#Zadanie 2
#Producent proszku do prania A twierdzi, �e jego produkt jest znacznie lepszy ni� konkurencyjny proszek B
#Aby zweryfikowa� to zapewnienie, CTA (Consumer Test Agency) przetestowa�o oba proszki do prania. W tym celu przeprowadzono pomiary stopnia wyprania  
#7 kawa�k�w tkaniny z proszkiem A i uzyskano wyniki (w %). Jaki powinien by� wniosek CTA na temat jako�ci tych proszk�w?
#ilosciowa ci�g�a
a<- c(78.2,78.5,75.6,78.5,78.5,77.4,76.6)
shapiro.test(a)
#W = 0.8228, p-value = 0.06833
b <- c(76.1,75.2,75.8,77.3,77.3,77.0,74.4,76.2,73.5,77.4)
boxplot(a,b)
shapiro.test(b)
#W = 0.9062, p-value = 0.2559 < 0.05 = alpha
# H0: sigma_1^2 = sigma_2^2 
# H1: sigma_1^2 < sigma_2^2 
# r�wnosc wariancji TAK
#F-Snedecora
var(a)
var(b)
var.test(a,b, alternative = "less")

# H0:mu_1 = mu_2
# H1: mu_1 > mu_2
t.test(a,b,alternative = "greater",
       var.equal = TRUE)

#zadanie 3
#Grupa 10 os�b zosta�a poddana badaniu maj�cemu na celu zbadanie stosunku do szk� publicznych. 
#Nast�pnie grupa obejrza�a film edukacyjny maj�cy na celu popraw� podej�cia do tego typu szk�. 
#Wyniki s� nast�puj�ce (wy�sza warto�� oznacza lepsze podej�cie).
#Zweryfikuj, czy film znacznie poprawia stosunek do szk� publicznych.
#pr�by 2
# zale�nosc zmiennych
c <- c(84,87,87,90,90,90,90,93,93,96)
d <- c(89,92,98,95,95,92,95,92,98,101)
boxplot(c,d)
mean(c)
mean(d)
shapiro.test(c)
shapiro.test(d)
qqnorm(c)
qqline(c)
qqnorm(d)
qqline(d)
# H0: sigma_1^2 = sigma_2^2 
# H1: sigma_1^2 > sigma_2^2 
t.test(c, d, alternative = 'less', paired = TRUE)

#zadanie 4
#Zbadano wzrost 13 m�czyzn i 12 kobiet w pewnym centrum sportowym. 
#Czy mo�emy stwierdzi�, �e �redni wzrost m�czyzn jest znacznie wi�kszy ni� wzrost kobiet?
# bedzie normalnosc
#nie bedzie rOwnosci warancji
m<-c(171,176,179,189,176,182,173,179,184,186,189,167,177)
k<- c(161,162,163,162,166,164,168,165,168,157,161,172)
boxplot(m,k)
shapiro.test(m)
qqnorm(m)
qqline(m)
shapiro.test(k)
qqnorm(k)
qqline(k)
var(m)
var(k)
#valib na false 

#Zadanie_5
#U�yj odpowiednich test�w F i t-Studenta, aby rozwi�za� zadania 1-4,
#ale teraz u�yj obszar�w krytycznych zamiast warto�ci  
p
-warto�ci.
1
x <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)
n <- length(x)
#xx-�rednia
xx <- mean(x)
mu_0 <- 870
#odhylenie standardowe
s <- sd(x)
#warto�� testowa
( t<- sqrt(n)*(xx-mu_0)/s)
#warto�� krytyczna
alpha <- 0.05

(wk <- qt(alpha,n-1))
t<=wk

#brak podstaw od odrzucenia

#Zadanie 6
w_test <- function(x,lambda_zero, alternative=c("two.sided", "less","greater")){
  alternative<- match.arg(alternative)
  n<-length(x)
  t<- 2*lambda_zero*n*mean(x)
  df<- 2*n
  if(alternative == "greater"){
    p_value <- pchisq(t,df=df)
  } else if (alternative == "less"){
    p_value <- 1-pchisq(t,df=df)
  } else if (alternative == "two.sided"){
    p_value <- 2*min(pchisq(t,df=df),1-pchisq(t.df=df))
  }
  wynik <- list(statistic = t,
                parameter = df,
                p.value = p_value,
                alternative = alternative,
                method = "test chi-kwadrat w modelu wykladniczym",
                data.name = deparse(substitute(x)))
  class(wynik)<- "htest"
  return(wynik)
}
awarie<- read.table("awarie.txt",dec = ",")
head(awarie)
1/mean(awarie$V1)
w_test(awarie$V1,lambda_zero=0.001,alternative ="less")


#Zadanie 7
#W przypadku pewnego mikro RNA badacz chce przetestowa� hipotez�, 
#�e prawdopodobie�stwo wyst�pienia puryn wynosi 0,7.
#W przeprowadzonym eksperymencie mikro RNA o d�ugo�ci 22 zawiera�o 18 puryn.
#Zweryfikuj hipotez� badacza.
#Dyskretny 0,1
#H0:p=0,7
#H1:<>=0,7
#test istotno�ci dla wska�nika struktury, dwumianowy

#jak nieznamy parametru p to liczymy z pr�by (estymujemu)
# jakijest estymator p w dwumianowym 
#p=?
#p=18/22=0.81..
#H1:p>0,7
prop.test(x=18,n=22,p=0.7,alternative="greater")
#brak podstaw do odrzucenia

binom.test(x=18,n=22,p=0.7,alternative="greater")

#P(X>k),X-b(n,p)
#P(X>k)= 1-P(X<=k)=1-F(k)
1-pbinom(q=17,size=22,prob=0.7)

binom.test(x=18,n=22,p=0.7,alternative="greater")$p.value


#Zadanie 8
#Po por�wnaniu podobnych firm w dw�ch r�nych miastach A i B
#postawiono hipotez�, �e odsetek firm korzystaj�cych z reklam 
#w obu miastach jest znacz�co r�ny. Aby sprawdzi� t� hipotez�, wybrano  
#120 firm w mie�cie A z czego  20 wykorzysta�o reklam�, a spo�r�d 150 firm w mie�cie  
#B 45 firm skorzysta�o z reklamy. Ustal, czy r�nica mi�dzy odsetkami firm korzystaj�cych z reklam w miastach  
#A i B jest statystycznie istotna.
#dwie pr�by
#cecha 0,1
#H0:p_a=p_b
#H1:p_a!=p_b
#test dla dwuch wska�nik�w struktury

prop.test(x=c(20,45),n=c(120,150))
#H1:p_a:p_b
#POTRZEBUJEMY �REDNIEJ
20/120
45/150

prop.test(x=c(20,45)n=c(120,150),?="less")

#Zadanie 9
#W losowej pr�bie 1600 Amerykan�w uprawnionych do g�osowania 944 z nich pozytywnie oceni�o dzia�alno�� prezydenta. 
#Po miesi�cu ankieta zosta�a powt�rzona, a 880 respondent�w pozytywnie oceni�o dzia�alno�� prezydenta.
#Sprawd� hipotez� o nieistotnej r�nicy w odpowiedziach ankietowanych.
#0,1
#dwie pr�by
#zale�ne
#jaki test:test MaM...
#jaka bynkcja zwraca�a
mcnemar.test(matrix(c(794,86,150,570),2,2))
#odrzucamy