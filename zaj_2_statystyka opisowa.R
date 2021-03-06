
# Zadanie 2.1 -------------------------------------------------------------
# Napisz funkcj� zamieniaj�c� miar� k�ta podan� w stopniach na radiany. Sprawd� dzia�anie tej funkcji dla k�t�w o mierze:  
#   0�, 30�, 45�, 60�, 90�.
# Nast�pnie przygotuj ramk� danych, w kt�rej zebrane b�d� informacje o warto�ciach funkcji sinus, cosinus, tangens i cotangens dla k�t�w o takich miarach.

#### spos�b 1 ####
radians <- function(x) x*pi /180
rd <- radians(c(0., 30., 45., 60., 90.))
angles <- c(0., 30., 45., 60., 90.)
df = data.frame(sin = sin(rd), cos = cos(rd), tg = tan(rd), tcg = 1/tan(rd))

rownames(df) <- angles
df
#### spos�b 2 ####
funkcja = function(x) return(2*pi/360*x)

funkcja(c(0,30,45,60,90))

c(0,30,45,60,90)

cot = function(x) return(1/tan(x))

df = data.frame(sin=sin(c(0,30,45,60,90)),cos=cos(c(0,30,45,60,90)),tan=tan(c(0,30,45,60,90)),cot=cot(c(0,30,45,60,90)))

# Zadanie 2.2 -------------------------------------------------------------
# Za pomoc� funkcji outer() wyznacz tabliczk� mno�enia dla liczb mniejszych od 6.
outer(X = 1:5, Y = 1:5, FUN = function(x,y) paste(x, "*", y, "=", x*y))

# Zadanie 2.3 -------------------------------------------------------------
# Oblicz iloczyn element�w dowolnego wektora x za pomoc� p�tli while, repeat i for (ka�dej z osobna).

#### spos�b 1 ####
x3 <- 1:5
il <-  1
i <- 1
while (i<=x3[length(x3)]) {
  il = il *x3[i]
  i = i + 1
  
}
print(il)

repeat {
  il = il * x3[i]
  i = i+1
  if (i > x3[length(x3)]) break
}
print(il)

for (j in x3) {
  il = il*j
}
print(il)

#### spos�b 2 ####
x <- 1:5

iloczyn = 1
i = length(x)
while(i > 0){
  iloczyn = x[i] * iloczyn
  i = i - 1
}
print(iloczyn)

iloczyn = 1
i = 1
repeat{
  iloczyn = x[i] * iloczyn
  i = i + 1
  if (i > length(x))
  {break}
}
print(iloczyn)

iloczyn = 1
for (i in 1:length(x)){
  iloczyn = x[i]*iloczyn
}
print(iloczyn)

# Zadanie 2.4 -------------------------------------------------------------
# Ile liczb postaci  
# (
#   n
#   r) jest wi�kszych od miliona dla 1???r???n???100?

n = 1:100
k = 1:100

suma = 0

for (i in n){
  for (j in k){
    if (i >= j){
      if ((factorial(i)/(factorial(j)*factorial(i-j)))>1000000){
        suma = suma + 1
      }
    }
  }
}

print(suma)



# Zadanie 2.5 -------------------------------------------------------------
# Napisz funkcj�, kt�ra sprawdza czy wektor jest palindromem.

#### spos�b 1 ####
x <- c(1, 2, 3, 3, 2, 1)
y <- c(1, 2, 3, 3, 2, 2)

check = function(x) return(all(x==rev(x)))

check(x)
check(y)

#### spos�b 2 ####
is_palindrome <- function(x) {
  all(x == rev(x))
}

is_palindrome(c(1,2,2,1))

x1 <-  c(1,2,2,1)
all(x1 == rev(x1))

# Zadanie 2.6 -------------------------------------------------------------
# Napisz funkcj�, kt�rej argumentem b�dzie wektor liczbowy a wynikiem wektor zawieraj�cy trzy najmniejsze i trzy najwi�ksze liczby 
# w tym wektorze. W przypadku argumentu kr�tszego ni� trzy liczby,0
# funkcja ma zwraca� komunikat o b��dzie z komentarzem ,,za kr�tki argument��.

#### spos�b 1 ####
z <- c(2, 6, 1, 5, 7, 3, 4)
z <- c(2, 6)
print(sort(z))

zd6 <- function(x) {
  if (length(x)<6) {
    stop("vector is to short")
  }
  
  x <- sort(x)
  x_min = c(x[1:3])
  x <- rev(x)
  x_max = c(x[1:3])
  print(x_max)
  print(x_min)
  
}

zd6(z)

#### spos�b 2 ####
funkcja = function(x){
  if (length(x)<3){
    return('za krótki argument')
  }
  a = sort(x)[1:3]
  b = sort(x,decreasing=TRUE)[1:3]
  return(c(a,b))
}

x <- c(2, 6, 1, 5, 7, 3, 4)
funkcja(x)

y = c(2,2)
funkcja(y)

# Zadanie 2.7 -------------------------------------------------------------
# W teorii informacji oraz klasy???kacji danych cz�sto mo�na spotka� odleg�o�� Hamminga. 
# Jest ona okre�lona dla ci�g�w 0-1 jako liczba pozycji, 
# na kt�rej te ci�gi si� r�ni� (np. odleg�o�� Hamminga pomi�dzy 1011101 oraz 1001001 wynosi 2). 
# Napisz funkcj�, kt�ra oblicza odleg�o�� Hamminga.

funkcja = function(x,y){
  if (length(x)!=length(y)){
    return('wektory x i y sa rawnej dlugosci')
  }
  suma = 0
  for (i in 1:length(x)){
    if (x[i] != y[i]){
      suma = suma + 1
    }
  }
  return(suma)
}

x <- c(1, 0, 1, 1, 1, 0, 1)
y <- c(1, 0, 0, 1, 0, 0, 1,2)

funkcja(x,y)

# Zadanie 2.8 -------------------------------------------------------------
# W pewnych sytuacjach przydatna mo�e si� okaza� tzw. kategoryzacja zmiennych, czyli inny podzia� na kategorie ni�by wynika� z danych.
# 
#  * Wygeneruj 100 obserwacji, kt�re s� odpowiedziami na pytania ankiety, 
# ka�da odpowied� mo�e przyj�� jedn� z warto�ci: �a�, �b�, �c�, �d�, �e�.
#  * Dokonaj kategoryzacji w taki spos�b, 
# aby kategoria 1 obejmowa�a odpowiedzi �a� i �b�, 2 odpowiedzi �c� i �d� oraz 3 odpowied� �e�.
# **Wskaz�wka: Wykorzystaj funkcj� sample() oraz funkcj� recode() z pakietu car.

letters = c('A','B','C','D','E')

a = sample(letters,100,replace=TRUE)

library(car)

recode(a,"c('A')=1;c('B')=2;c('C')=3;c('D')=4;c('E')=5")

# Zadanie 2.9 -------------------------------------------------------------
# Wygeneruj 1000 rzut�w:
#   
# 1.(uczciw�) sze�cienn� kostk� do gry,
# 2.obci��on� kostk� do gry, dla kt�rej sz�stka wypada dwa razy cz�ciej ni� jedynka, 
# a szansa wypadni�cia dla ka�dej z pozosta�ych liczb oczek wynosi  1/6.
# Nast�pnie wyznacz proporcj� wyrzucenia ka�dej liczby oczek w tych wygenerowanych rzutach. 
# Czy zgadzaj� si� one z warto�ciami teoretycznymi?

#### spos�b 1 ####
s = sample(1:6,1000,replace=TRUE)

table(s)/1000

s2 = sample(c(rep(1,2),rep(c(2,3,4,5),3),rep(6,4)),1000,replace=TRUE)

table(s2)/1000

#### spoos�b 2 ####
n_rep = 1000
uczciwa_kostka <- sample(
  x = 1:6,
  size = n_rep,
  replace = TRUE
)
nieuczciwa_kostka <- sample(
  x = 1:6,
  size = n_rep,
  replace = TRUE,
  prob = c(1/9, 1/6, 1/6, 1/6, 1/6, 2/9))

table(uczciwa_kostka)/n_rep
table(nieuczciwa_kostka)/n_rep

# Zadanie 2.10 ------------------------------------------------------------
# Napisz funkcj� licz�c� �redni�, wariancj� oraz odchylenie standardowe dla nr losowo wygenerowanych liczb z rozk�adu wyk�adniczego z parametrem lambda. 
# Wywo�aj t� funkcj� dla nr = 1000 oraz lambda = 3. 
# Czy wyniki zwracane przez funkcj� s� zgodne z warto�ciami teoretycznymi?

funkcja = function(n,lambda){
  losowe = rexp(n,lambda)
  return(c(mean(losowe),var(losowe),sd(losowe)))
}

funkcja(1000,3)

# Zadanie 2.11 ------------------------------------------------------------
# Napisz kod w programie R symuluj�cy rzucanie monet� do momentu wypadni�cia pierwszego or�a. 
# Wynikiem ma by� liczba potrzebnych rzut�w do momentu pojawienia si� pierwszego or�a. 
# Wykorzystaj p�tl� repeat oraz funkcj� sample().
suma = 0
repeat{
  x = sample(c('O','R'),1)
  suma = suma + 1
  if (x == 'O')
  {return (suma)}
}
print(suma
