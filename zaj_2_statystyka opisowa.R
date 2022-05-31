
# Zadanie 2.1 -------------------------------------------------------------
# Napisz funkcjê zamieniaj¹c¹ miarê k¹ta podan¹ w stopniach na radiany. SprawdŸ dzia³anie tej funkcji dla k¹tów o mierze:  
#   0°, 30°, 45°, 60°, 90°.
# Nastêpnie przygotuj ramkê danych, w której zebrane bêd¹ informacje o wartoœciach funkcji sinus, cosinus, tangens i cotangens dla k¹tów o takich miarach.

#### sposób 1 ####
radians <- function(x) x*pi /180
rd <- radians(c(0., 30., 45., 60., 90.))
angles <- c(0., 30., 45., 60., 90.)
df = data.frame(sin = sin(rd), cos = cos(rd), tg = tan(rd), tcg = 1/tan(rd))

rownames(df) <- angles
df
#### sposób 2 ####
funkcja = function(x) return(2*pi/360*x)

funkcja(c(0,30,45,60,90))

c(0,30,45,60,90)

cot = function(x) return(1/tan(x))

df = data.frame(sin=sin(c(0,30,45,60,90)),cos=cos(c(0,30,45,60,90)),tan=tan(c(0,30,45,60,90)),cot=cot(c(0,30,45,60,90)))

# Zadanie 2.2 -------------------------------------------------------------
# Za pomoc¹ funkcji outer() wyznacz tabliczkê mno¿enia dla liczb mniejszych od 6.
outer(X = 1:5, Y = 1:5, FUN = function(x,y) paste(x, "*", y, "=", x*y))

# Zadanie 2.3 -------------------------------------------------------------
# Oblicz iloczyn elementów dowolnego wektora x za pomoc¹ pêtli while, repeat i for (ka¿dej z osobna).

#### sposób 1 ####
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

#### sposób 2 ####
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
#   r) jest wiêkszych od miliona dla 1???r???n???100?

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
# Napisz funkcjê, która sprawdza czy wektor jest palindromem.

#### sposób 1 ####
x <- c(1, 2, 3, 3, 2, 1)
y <- c(1, 2, 3, 3, 2, 2)

check = function(x) return(all(x==rev(x)))

check(x)
check(y)

#### sposób 2 ####
is_palindrome <- function(x) {
  all(x == rev(x))
}

is_palindrome(c(1,2,2,1))

x1 <-  c(1,2,2,1)
all(x1 == rev(x1))

# Zadanie 2.6 -------------------------------------------------------------
# Napisz funkcjê, której argumentem bêdzie wektor liczbowy a wynikiem wektor zawieraj¹cy trzy najmniejsze i trzy najwiêksze liczby 
# w tym wektorze. W przypadku argumentu krótszego ni¿ trzy liczby,0
# funkcja ma zwracaæ komunikat o b³êdzie z komentarzem ,,za krótki argument’’.

#### sposób 1 ####
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

#### sposób 2 ####
funkcja = function(x){
  if (length(x)<3){
    return('za krÃ³tki argument')
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
# W teorii informacji oraz klasy???kacji danych czêsto mo¿na spotkaæ odleg³oœæ Hamminga. 
# Jest ona okreœlona dla ci¹gów 0-1 jako liczba pozycji, 
# na której te ci¹gi siê ró¿ni¹ (np. odleg³oœæ Hamminga pomiêdzy 1011101 oraz 1001001 wynosi 2). 
# Napisz funkcjê, która oblicza odleg³oœæ Hamminga.

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
# W pewnych sytuacjach przydatna mo¿e siê okazaæ tzw. kategoryzacja zmiennych, czyli inny podzia³ na kategorie ni¿by wynika³ z danych.
# 
#  * Wygeneruj 100 obserwacji, które s¹ odpowiedziami na pytania ankiety, 
# ka¿da odpowiedŸ mo¿e przyj¹æ jedn¹ z wartoœci: ‘a’, ‘b’, ‘c’, ‘d’, ‘e’.
#  * Dokonaj kategoryzacji w taki sposób, 
# aby kategoria 1 obejmowa³a odpowiedzi ‘a’ i ‘b’, 2 odpowiedzi ‘c’ i ‘d’ oraz 3 odpowiedŸ ‘e’.
# **Wskazówka: Wykorzystaj funkcjê sample() oraz funkcjê recode() z pakietu car.

letters = c('A','B','C','D','E')

a = sample(letters,100,replace=TRUE)

library(car)

recode(a,"c('A')=1;c('B')=2;c('C')=3;c('D')=4;c('E')=5")

# Zadanie 2.9 -------------------------------------------------------------
# Wygeneruj 1000 rzutów:
#   
# 1.(uczciw¹) szeœcienn¹ kostk¹ do gry,
# 2.obci¹¿on¹ kostk¹ do gry, dla której szóstka wypada dwa razy czêœciej ni¿ jedynka, 
# a szansa wypadniêcia dla ka¿dej z pozosta³ych liczb oczek wynosi  1/6.
# Nastêpnie wyznacz proporcjê wyrzucenia ka¿dej liczby oczek w tych wygenerowanych rzutach. 
# Czy zgadzaj¹ siê one z wartoœciami teoretycznymi?

#### sposób 1 ####
s = sample(1:6,1000,replace=TRUE)

table(s)/1000

s2 = sample(c(rep(1,2),rep(c(2,3,4,5),3),rep(6,4)),1000,replace=TRUE)

table(s2)/1000

#### spoosób 2 ####
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
# Napisz funkcjê licz¹c¹ œredni¹, wariancjê oraz odchylenie standardowe dla nr losowo wygenerowanych liczb z rozk³adu wyk³adniczego z parametrem lambda. 
# Wywo³aj tê funkcjê dla nr = 1000 oraz lambda = 3. 
# Czy wyniki zwracane przez funkcjê s¹ zgodne z wartoœciami teoretycznymi?

funkcja = function(n,lambda){
  losowe = rexp(n,lambda)
  return(c(mean(losowe),var(losowe),sd(losowe)))
}

funkcja(1000,3)

# Zadanie 2.11 ------------------------------------------------------------
# Napisz kod w programie R symuluj¹cy rzucanie monet¹ do momentu wypadniêcia pierwszego or³a. 
# Wynikiem ma byæ liczba potrzebnych rzutów do momentu pojawienia siê pierwszego or³a. 
# Wykorzystaj pêtlê repeat oraz funkcjê sample().
suma = 0
repeat{
  x = sample(c('O','R'),1)
  suma = suma + 1
  if (x == 'O')
  {return (suma)}
}
print(suma
