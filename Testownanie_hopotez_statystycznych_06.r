# TESTOWANIE HIPOTEZ STATYSTYCZNYCH
# Zadanie 1
#W pewnym regionie wykonano dziesiêæ niezale¿nych pomiarów g³êbokoœci morza i uzyskano nastêpuj¹ce wyniki: 862, 870, 876, 866, 871, 865, 861, 873, 871, 872.
#Na poziomie istotnoœci ??= 0,05 zweryfikuj hipotezê, ¿e œrednia g³êbokoœæ morza w tym regionie wynosi  870m
x <- c(862,870,876,866,871,865,861,873,871,872)
shapiro.test(x)
#W = 0.93936, p-value = 0.5459 > 0.05 = alpha
#Nie ma podstaw do odrzucenia hipotezy zerowej
#Badana cecha ma rozk³ad normalny
qqnorm(x)
qqline(x)
# H0: mu = 0
# H1: mu < 870
mean(x)
t.test(x, mu = 870, alternative = "less")
#Zadanie 2
#Producent proszku do prania A twierdzi, ¿e jego produkt jest znacznie lepszy ni¿ konkurencyjny proszek B
#Aby zweryfikowaæ to zapewnienie, CTA (Consumer Test Agency) przetestowa³o oba proszki do prania. W tym celu przeprowadzono pomiary stopnia wyprania  
#7 kawa³ków tkaniny z proszkiem A i uzyskano wyniki (w %). Jaki powinien byæ wniosek CTA na temat jakoœci tych proszków?
#ilosciowa ci¹g³a
a<- c(78.2,78.5,75.6,78.5,78.5,77.4,76.6)
shapiro.test(a)
#W = 0.8228, p-value = 0.06833
b <- c(76.1,75.2,75.8,77.3,77.3,77.0,74.4,76.2,73.5,77.4)
boxplot(a,b)
shapiro.test(b)
#W = 0.9062, p-value = 0.2559 < 0.05 = alpha
# H0: sigma_1^2 = sigma_2^2 
# H1: sigma_1^2 < sigma_2^2 
# równosc wariancji TAK
#F-Snedecora
var(a)
var(b)
var.test(a,b, alternative = "less")

# H0:mu_1 = mu_2
# H1: mu_1 > mu_2
t.test(a,b,alternative = "greater",
       var.equal = TRUE)

#zadanie 3
#Grupa 10 osób zosta³a poddana badaniu maj¹cemu na celu zbadanie stosunku do szkó³ publicznych. 
#Nastêpnie grupa obejrza³a film edukacyjny maj¹cy na celu poprawê podejœcia do tego typu szkó³. 
#Wyniki s¹ nastêpuj¹ce (wy¿sza wartoœæ oznacza lepsze podejœcie).
#Zweryfikuj, czy film znacznie poprawia stosunek do szkó³ publicznych.
#próby 2
# zale¿nosc zmiennych
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
#Zbadano wzrost 13 mê¿czyzn i 12 kobiet w pewnym centrum sportowym. 
#Czy mo¿emy stwierdziæ, ¿e œredni wzrost mê¿czyzn jest znacznie wiêkszy ni¿ wzrost kobiet?
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
#U¿yj odpowiednich testów F i t-Studenta, aby rozwi¹zaæ zadania 1-4,
#ale teraz u¿yj obszarów krytycznych zamiast wartoœci  
p
-wartoœci.
1
x <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)
n <- length(x)
#xx-œrednia
xx <- mean(x)
mu_0 <- 870
#odhylenie standardowe
s <- sd(x)
#wartoœæ testowa
( t<- sqrt(n)*(xx-mu_0)/s)
#wartoœæ krytyczna
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
#W przypadku pewnego mikro RNA badacz chce przetestowaæ hipotezê, 
#¿e prawdopodobieñstwo wyst¹pienia puryn wynosi 0,7.
#W przeprowadzonym eksperymencie mikro RNA o d³ugoœci 22 zawiera³o 18 puryn.
#Zweryfikuj hipotezê badacza.
#Dyskretny 0,1
#H0:p=0,7
#H1:<>=0,7
#test istotnoœci dla wskaŸnika struktury, dwumianowy

#jak nieznamy parametru p to liczymy z próby (estymujemu)
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
#Po porównaniu podobnych firm w dwóch ró¿nych miastach A i B
#postawiono hipotezê, ¿e odsetek firm korzystaj¹cych z reklam 
#w obu miastach jest znacz¹co ró¿ny. Aby sprawdziæ tê hipotezê, wybrano  
#120 firm w mieœcie A z czego  20 wykorzysta³o reklamê, a spoœród 150 firm w mieœcie  
#B 45 firm skorzysta³o z reklamy. Ustal, czy ró¿nica miêdzy odsetkami firm korzystaj¹cych z reklam w miastach  
#A i B jest statystycznie istotna.
#dwie próby
#cecha 0,1
#H0:p_a=p_b
#H1:p_a!=p_b
#test dla dwuch wskaŸników struktury

prop.test(x=c(20,45),n=c(120,150))
#H1:p_a:p_b
#POTRZEBUJEMY ŒREDNIEJ
20/120
45/150

prop.test(x=c(20,45)n=c(120,150),?="less")

#Zadanie 9
#W losowej próbie 1600 Amerykanów uprawnionych do g³osowania 944 z nich pozytywnie oceni³o dzia³alnoœæ prezydenta. 
#Po miesi¹cu ankieta zosta³a powtórzona, a 880 respondentów pozytywnie oceni³o dzia³alnoœæ prezydenta.
#SprawdŸ hipotezê o nieistotnej ró¿nicy w odpowiedziach ankietowanych.
#0,1
#dwie próby
#zale¿ne
#jaki test:test MaM...
#jaka bynkcja zwraca³a
mcnemar.test(matrix(c(794,86,150,570),2,2))
#odrzucamy