

day4 <- "e:\\Corso_R_2018\\day_4"

setwd(day4)
getwd()
library(readr)
library(magrittr)
library(xlsx)
library(tibble)
library(dplyr)
library(tidyr)
library(ISwR)


plot(rnorm(1000))


pain <- c(0,3,2,2,1)
fpain <- factor(pain,levels=0:3)
levels(fpain) <- c("none","mild","medium","severe")
fpain



# List
intake.pre <- c(5260,5470,5640,6180,6390, 6515,6805,7515,7515,8230,8770)
intake.post <- c(3910,4220,3885,5160,5645, 4680,5265,5975,6790,6900,7335)

mylist <- list(before=intake.pre,after=intake.post)
mylist

str(mylist)


# Data frame
d <- data.frame(intake.pre,intake.post)
d


# Graphics
x <- rnorm(100)
hist(x,freq=F)
curve(dnorm(x),add=T)
h <- hist(x, plot=F)
ylim <- range(0, h$density, dnorm(0))
hist(x, freq=F, ylim=ylim)
curve(dnorm(x), add=T)

secretin <- read.table("secretin.txt")
levels(secretin$time)
str(secretin)

secretin$time1 <- as.numeric(secretin$time)
secretin$person1 <- as.character(secretin$person)
str(secretin)

# Probability and distributions

sample(1:40,5)

# Simulare il lancio di una moneta
sample(c("H","T"), 10, replace=T)


# Probabilita di ottenere un dato numero in un dato ordine
1/prod(40:36)

# Probabilita di ottenere un set di numeri correttamente
prod(5:1)/prod(40:36)

# Si può anche scrivere come
1/choose(40,5)

# Simulazione con probabilita differente 
sample(c("succ", "fail"), 10, replace=T, prob=c(0.9, 0.1))


# Distribuzioni in R
x <- seq(-4,4,0.1)
plot(x,dnorm(x),type="l")

# Modo alternativo
curve(dnorm(x), from=-4, to=4)

# Diagramma a barre
x <- 0:50 # è la forma compressa di scrivere seq(0:50,1)
plot(x,dbinom(x,size=50,prob=.33),type="h")

# Immaginate di avere una distribuzione normale con media 132 e sd 13
# se un vostro dato ha valore 160 quale è la probabilità di avere valori superiori?

1-pnorm(160, mean=132, sd=13)


# 20 pazienti trattati con due trattamenti A e B e verifico quale funziona meglio
# 16 rispondono che funziona meglio A
# A è veramente meglio di B

# Se non ci sono differenze ci attendiamo che il numero di persone che preferiscono
# il trattamento A sia binomialmente distribuito con p = 0,5 e n = 20

pbinom(16, size=20, prob=.5)

# Questa è la zona di accettazione dell'ipotesi


# Si sottrae la probabilità ad 1 per ottenere il rifiuto
# Si riporta 15 e non 16 perché? vogliamo la probabilità? di 16 o meno di 16

1-pbinom(15, size=20, prob=.5)


# Qualora si fosse dovuto utilizzare un test a due code sarebbe stato
1-pbinom(15,20,.5)+pbinom(4,20,.5)

# Con 15 e 4 come limiti delle code


# Quantili
xbar <- 83
sigma <- 12
n <- 5
sem <- sigma/sqrt(n)
sem

# Intervallo di confiedenza sarà quindi
xbar + sem * qnorm(0.025)
xbar + sem * qnorm(0.975)


# Random number 
rnorm(10)
rnorm(10)
rnorm(10,mean=7,sd=5)
rbinom(10,size=20,prob=.5)


# Descriptive statistics
juul <- read.csv2("juul.csv")

attach(juul)
mean(igf1)
mean(igf1, na.rm=T)
View(juul)
sum(!is.na(igf1))
detach(juul)

juul$sex <- factor(juul$sex,labels=c("M","F"))
juul$menarche <- factor(juul$menarche,labels=c("No","Yes"))
juul$tanner <- factor(juul$tanner, labels=c("I","II","III","IV","V"))
summary(juul)

# Altra possibilità per trasformare in fattori
juul <- transform(juul,
                  sex=factor(sex,labels=c("M","F")),
                  menarche=factor(menarche,labels=c("No","Yes")),
                  tanner=factor(tanner,labels=c("I","II","III","IV","V")))
summary(juul)


# Q-Q plots
# Serve a verificare graficamente se i dati sono distribuiti normalmente (es. i residui)
qqnorm(x)


# Semplici statistiche riassuntive
descr <- read.csv2('FileTab2.csv', header = T)
str(descr)
attach(descr)
tapply(UMIDITA,RAZZA,mean)
tapply(UMIDITA,RAZZA,sd)
tapply(UMIDITA,RAZZA,length)

# Nicer
xbar <- tapply(UMIDITA,RAZZA,mean)
s <- tapply(UMIDITA,RAZZA,sd)
n <- tapply(UMIDITA,RAZZA,length)
cbind (mean=xbar, std.dev=s, n=n)
detach(descr)



# T test
daily.intake <- c(5260,5470,5640,6180,6390,6515, 6805,7515,7515,8230,8770)

mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)
t.test(daily.intake,mu=7725)

# Wilconxon test (a ranghi)

wilcox.test(daily.intake, mu=7725)

# T-test a due code
energy <- read.csv2('energy.csv', header = T)

attach(energy)

t.test(expend ~ stature)

# Assunzione di uguali varianze
t.test(expend ~ stature, var.equal=T)

# Controllate i gradi di libertà. Adesso sono 20

# Se volete controllare se le varianze sono uguali
var.test(expend ~ stature)

# Il test non è significativo quindi le varianze non sono differenti

# Anche in questo caso si può usare un test a ranghi (Wilcoxon)
wilcox.test(expend ~ stature)

detach(energy)

# T-test a coppie
intake <- read.csv2('intake1.csv', header = T)
attach(intake)
t.test(pre,post, paired=T)

# Informazione paired=T è fondamentale per definire che il confronto è fatto sullo stesso individuo/pianta/....
# provate senza il comando

# Wilcoxon test
wilcox.test(pre,post, paired=T)
detach(intake)


# REGRESSION
thuesen <- read.csv2('thuesen.csv', header = T)
attach(thuesen)
lm(short.velocity~blood.glucose)
summary(lm(short.velocity~blood.glucose))

# Riportare i punti sul grafico e plottare la retta di regressione
plot(blood.glucose,short.velocity)
abline(lm(short.velocity~blood.glucose))

# Altra forma per plottare la retta
abline(1.09781,0.02196, col="red")


# Valori adattati (fitted) e residui
lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
resid(lm.velo)

# plot dei valori adattati tenendo conto dei dati NA
plot(blood.glucose,short.velocity)
lines(blood.glucose[!is.na(short.velocity)],fitted(lm.velo))

# Altra foma di gestire gli NA
options(na.action=na.exclude)
lm.velo <- lm(short.velocity~blood.glucose)

# Plot dei residui come scarto dalla retta di regressione
segments(blood.glucose,fitted(lm.velo), blood.glucose,short.velocity)

# plot adattati vs residui e QQnorm
plot(fitted(lm.velo),resid(lm.velo))
qqnorm(resid(lm.velo))


# Predizione e intervallo di confidenza
predict(lm.velo)

# int="c" riporta intervallo di confidenza
predict(lm.velo, int="c")

# int="p" riporta intervallo di predizione
predict(lm.velo, int="p")


# Come riportare questi intervalli in forma grafica?
# Attraverso la funzione matlines

pred.frame <- data.frame(blood.glucose=4:20)
pp <- predict(lm.velo, int="p", newdata=pred.frame)
pc <- predict(lm.velo, int="c", newdata=pred.frame)
plot(blood.glucose,short.velocity, ylim=range(short.velocity, pp, na.rm=T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,2), col="black")
matlines(pred.gluc, pp, lty=c(1,3,3), col="black")


# CORRELATION

cor(blood.glucose, short.velocity)

cor(blood.glucose, short.velocity, use="complete.obs")

cor(thuesen, use="complete.obs")

# Per avere indicazioni sulla significatività della correlazione
cor.test(blood.glucose, short.velocity, use="complete.obs")

# Correlazione per ranghi (SPEARMAN)
cor.test(blood.glucose, short.velocity, method="spearman")


# Correlazione Kendall 
cor.test(blood.glucose, short.velocity, method="kendall")
detach(thuesen)



# RIPRENDIAMO LA REGRESSIONE CON IL DATASET cars DI R

# plottiamo distanza e velocità
plot(dist ~ speed, data=cars )

# Come scrivere la regressione in R

cars.lm <- lm(dist ~ speed, data=cars)
coef(cars.lm)

# Plot linee
plot(dist ~ speed, data = cars, pch = 16)
abline(coef(cars.lm))

# Se volete conoscere la distanza predetta di frenata di auto che viagiano a 6, 8 e 21 mph
predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))

# Errore standard residuo della regressione
(carsumry <- summary(cars.lm))
carsumry$sigma

# Intervallo di confidenza
confint(cars.lm)

# Intervalli di predizione e di confidenza per valori non presenti in origine
new <- data.frame(speed = c(5, 6, 21))
predict(cars.lm, newdata = new, interval = "confidence")
predict(cars.lm, newdata = new, interval = "prediction")


# Volendo avere il grafico con un solo comando
library(HH)
ci.plot(cars.lm)

# R quadro
carsumry$r.squared

# R quadro aggiustato

sqrt(carsumry$r.squared)


# Assunzione di normalità
# H0 : the residuals are normally distributed
# versus
# H1 : the residuals are not normally distributed
shapiro.test(residuals(cars.lm))


# Controllare se la varianza dei residui è costante o meno (test di Breusch-Pagan)
library(lmtest)
bptest(cars.lm)
# Assunzione di indipendenza
# Durbin-Watson test
dwtest(cars.lm,alternative="two.sided")

# Outliers
# Calcolo dei residui standardizzati
sres<- rstandard(cars.lm)

# Quali sono i residui oltre 2 d.s. 
sres[which(abs(sres) > 2)]

# Leverages
leverage <- hatvalues(cars.lm)

# Volendo avere le misure di influenza insieme
influence.measures(cars.lm)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(cars.lm)
par(mfrow = c(1, 1))
plot(cars.lm, which = 5) # std'd resids vs lev plot
identify(leverage, sres, n = 4) # identify 4 points


# Calcolo potenza test
curve(pt(x,25,ncp=3), from=0, to=6)
abline(v=qt(.975,25))
pt(qt(.975,25),25,ncp=3)

# Dimensione campione
# Potenza 90% 
# test due code sign. 1%
# Differenza di 0.5 cm
# Distribuzione con dev. st. di 2 cm

power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9)


# Per test ad una coda
power.t.test(delta=0.5, sd=2, sig.level = 0.01, power=0.9, alt="one.sided")


# REGRESSIONE MULTIPLA

cystfibr <- read.csv2('cystfibr.csv', header = T)
par(mex=0.5)
pairs(cystfibr, gap=0, cex.labels=0.9)

attach(cystfibr)

# Modello lineare con pemax come dipendente e molte indipendenti
lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)

# Riassunto informazioni
summary(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))

# Nessun valore significativo dei test t ma test F significativo


# Il test dice solo che tutte insieme NON sono significative

# Proviamo ANOVA
anova(lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc))

# I risultati cambiano perché in questo caso i test sono successivi

# Se volessimo verificare se l'inclusione della altre variabili, escluso age, abbia effetto o meno
# dobbiamo confrontare le due LM con o senza variabili tramite 'anova' 

m1<-lm(pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
m2<-lm(pemax~age)
anova(m1,m2)

# Il confronto ci dice che possiamo escluderle senza problema "ATTENZIONE MODELLI GERARCHICI"

# Per verificare quali variabili includere R usa dei modelli stepwise
# NON LI TRATTEREMO OGGI

# MODELLI LINEARI

# REGRESSIONE POLINOMIALE

summary(lm(pemax~height+I(height^2)))


# Inserire I prima della formula è per prevenire interpretazioni anomale della formula
# Qui noi vogliamo solo altezza al quadrato

# Costruiamo il grafico dei predetti
pred.frame <- data.frame(height=seq(110,180,2))
lm.pemax.hq <- lm(pemax~height+I(height^2))
predict(lm.pemax.hq,interval="pred",newdata=pred.frame)
pp <- predict(lm.pemax.hq,newdata=pred.frame,interval="pred")
pc <- predict(lm.pemax.hq,newdata=pred.frame,interval="conf")
plot(height,pemax,ylim=c(0,200))
matlines(pred.frame$height,pp,lty=c(1,2,2),col="black")
matlines(pred.frame$height,pc,lty=c(1,3,3),col="black")
detach(cystfibr)

