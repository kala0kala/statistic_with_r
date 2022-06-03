
# zad1 --------------------------------------------------------------------

  # wczytanie i przygotowanie tabeli danych
dane1 <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt", 
           header = FALSE)
colnames(dane1) <- c("number", "context")
dane1$context <- as.factor(dane1$context)
dane1

  # œrednie liczone po grupach
#Wyznacz œrednie liczb zapamiêtanych s³ów w grupach. 
#Ponadto, przedstaw otrzymane dane za pomoc¹ wykresu ramkowego dla ka¿dej grupy z osobna.
aggregate(dane1$number, list(CONTEXT = dane1$context), FUN = mean)
  # boxplot
boxplot(number ~ context, dane1)


  # test analizy wariancji
#Wykonaj test analizy wariancji w celu sprawdzenia, 
#czy liczba zapamiêtanych s³ów zale¿y od kontekstu sprawdzania wiedzy
summary(aov(number ~ context, dane1))
#SprawdŸ za³o¿enia modelu jednoczynnikowej analizy wariancji.
  # za³o¿enie modelu jednoczynnikowej wariancji
  # normalnoœæ b³êdów losowych
shapiro.test(lm(number ~ context, data = dane1)$residuals)
  # test Bartletta
bartlett.test(number ~ context, data = dane1)
  # test Flingera-Killeena
fligner.test(number ~ context, data = dane1)
  # test Levene’a 
library(car)
leveneTest(number ~ context, data = dane1)
  # test Levene’a
leveneTest(number ~ context, data = dane1, center = "mean")

#Wykonaj testy post hoc w celu sprawdzenia, 
#które konteksty sprawdzania wiedzy ró¿ni¹ siê miêdzy sob¹.
  # testy post hoc
attach(dane1)
pairwise.t.test(number, context, data = dane1)
model_aov <- aov(number ~ context, data = dane1)
TukeyHSD(model_aov)
plot(TukeyHSD(model_aov))
library(agricolae)
HSD.test(model_aov, "context", console = TRUE)
SNK.test(model_aov, "context", console = TRUE)
LSD.test(model_aov, "context", p.adj = "holm", console = TRUE)
scheffe.test(model_aov, "context", console = TRUE)


  # testowanie hipotez szczegó³owych (kontrasty)
c1 <- c(3, -2, -2, 3, -2)
c2 <- c(0, -1, -1, 0, 2)
c3 <- c(0, -1, 1, 0, 0)
c4 <- c(-1, 0, 0, 1, 0)

cont <- cbind(c1, c2, c3, c4)
contrasts(dane1$context) <- cont

model_aov2 <- aov(number ~ context, data = dane1)
summary(model_aov2,
        split = list(context = list("c1" = 1, 
                                    "c2" = 2, 
                                    "c3" = 3, 
                                    "c4" = 4, 
                                    "c5" = 5)))


# zad2 --------------------------------------------------------------------
#Za³aduj zbiór danych do programu R. Nastêpnie usuñ zbêdn¹ kolumnê.
dane2 <- read.table("http://ls.home.amu.edu.pl/data_sets/Eysenck.txt", 
                   header = TRUE)
dane2$Instrukcja <- as.factor(dane2$Instrukcja)
dane2$Nr <- NULL
#Wyznacz œrednie wartoœci cechy zale¿nej w grupach. Ponadto, przedstaw otrzymane dane 
#za pomoc¹ wykresu ramkowego dla ka¿dej grupy z osobna.
  # œrednie liczone po grupach
aggregate(dane2$Wynik, list(Instrukcja = dane2$Instrukcja), FUN = mean)
  # boxplot
boxplot(Wynik ~ Instrukcja, dane2)
#Wykonaj test analizy wariancji w celu sprawdzenia, 
#czy typ instrukcji ma istotny wp³yw na badan¹ cechê zale¿n¹.
  # test analizy wariancji
summary(aov(Wynik ~ Instrukcja, dane2))
#SprawdŸ za³o¿enia modelu jednoczynnikowej analizy wariancji.
  # za³o¿enie modelu jednoczynnikowej wariancji
  # normalnoœæ b³êdów losowych
shapiro.test(lm(Wynik ~ Instrukcja, dane2)$residuals)
  # test Bartletta
bartlett.test(Wynik ~ Instrukcja, dane2)
  # test Flingera-Killeena
fligner.test(Wynik ~ Instrukcja, dane2)
  # test Levene’a 
library(car)
leveneTest(Wynik ~ Instrukcja, dane2)
  # test Levene’a
leveneTest(Wynik ~ Instrukcja, dane2, center = "mean")

#Wykonaj testy post hoc w celu sprawdzenia, które typy instrukcji ró¿ni¹ siê miêdzy sob¹.
  # testy post hoc
attach(dane2)
pairwise.t.test(Wynik, Instrukcja, data = dane2)
model_aov <- aov(Wynik ~ Instrukcja, data = dane2)
TukeyHSD(model_aov)
plot(TukeyHSD(model_aov))
library(agricolae)
HSD.test(model_aov, "Instrukcja", console = TRUE)
SNK.test(model_aov, "Instrukcja", console = TRUE)
LSD.test(model_aov, "Instrukcja", p.adj = "holm", console = TRUE)
scheffe.test(model_aov, "Instrukcja", console = TRUE)

#Przetestuj hipotezy szczegó³owe zwi¹zane z nastêpuj¹cymi zagadnieniami:
  # testowanie hipotez szczegó³owych (kontrasty)
c1 <- c(0, -1,  1, -1,  1)
c2 <- c(4, -1, -1, -1, -1)
c3 <- c(0, -1,  0,  1,  0)
c4 <- c(0,  0, -1,  0,  1)

cont <- cbind(c1, c2, c3, c4)
contrasts(dane2$Instrukcja) <- cont

model_aov2 <- aov(Wynik ~ Instrukcja, data = dane2)
summary(model_aov2,
        split = list(Instrukcja = list("c1" = 1, 
                                    "c2" = 2, 
                                    "c3" = 3, 
                                    "c4" = 4, 
                                    "c5" = 5)))


# zad3 --------------------------------------------------------------------
#Zaplanuj i przeprowadŸ symulacje, które bêd¹ bada³y zachowanie siê testu F jednoczynnikowej analizy wariancji dla skoñczonych prób. 
#Dok³adniej zbadaj empiryczny rozmiar i empiryczn¹ moc testu F w nastêpuj¹cych przypadkach:
  # PODPUNKT 1?
#rozk³ad normalny badanej cechy iloœciowej i takie same wariancje w grupach,
n = 1000
mu = c(10, 12, 13, 14)
sd = 1
  # create data for simulation
x <- c()
group_var <- c()
for (group in mu){
  x <- c(x, rnorm(n, group, sd))
  group_var <- c(group_var, rep(paste("group_", group, sep = ""), n))
  df <- data.frame(x, as.factor(group_var))
}
summary(df)
  # perform anova
summary(aov(x ~ group_var, data = df))


  # PODPUNKT 1?
n = 1000
mu = c(10, 10.001, 10.002, 10.003)
sd = 1
  # create data for simulation
x <- c()
group_var <- c()
for (group in mu){
  x <- c(x, rnorm(n, group, sd))
  group_var <- c(group_var, rep(paste("group_", group, sep = ""), n))
  df <- data.frame(x, as.factor(group_var))
}
summary(df)
  # perform anova
summary(aov(x ~ group_var, data = df))


  # PODPUNKT 2?
#rozk³ad normalny badanej cechy iloœciowej i ró¿ne wariancje w grupach (rozwa¿ ma³e i wiêksze ró¿nice w wariancjach),
n = 1000
mu = c(10, 12, 14)
sd = c(1, 5, 10)
  # create data for simulation
x <- c()
group_var <- c()
for (ind in 1:length(mu)){
  x <- c(x, rnorm(n, mu[ind], sd[ind]))
  group_var <- c(group_var, rep(paste("group_", ind, sep = ""), n))
  df <- data.frame(x, as.factor(group_var))
}
summary(df)
  # perform anova
summary(aov(x ~ group_var, data = df))


  # PODPUNKT 3?
#rozk³ad badanej cechy iloœciowej jest inny ni¿ normalny i wariancje s¹ takie same w grupach 
#(rozwa¿ ró¿ne rozk³ady prawdopodobieñstwa).
n = 100
dfs = c(10, 12, 14)
  # create data for simulation
x <- c()
group_var <- c()
for (ind in 1:length(mu)){
  x <- c(x, rchisq(n, dfs[ind]))
  group_var <- c(group_var, rep(paste("group_", ind, sep = ""), n))
  df <- data.frame(x, as.factor(group_var))
}
summary(df)
# perform anova
summary(aov(x ~ group_var, data = df))
