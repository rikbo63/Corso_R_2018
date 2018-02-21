day5 <- "e:\\Corso_R_2018\\day_5"

setwd(day5)
getwd()

path <- "e:\\Corso_R_2018\\day_5\\"
dir <- "e:\\Corso_R_2018\\day_5\\"

# REGRESSIONE LINEARE
dati<- read.table(paste(path, 'regSempl.txt', sep=""), header=T, dec=',')
plot(dati)
cov(dati)
cor(dati)
modLin <- lm(ingestione ~ peso, data=dati)
attributes(modLin)
modLin$coefficients 
modLin$residuals
modLin$fitted.values
summary(modLin)
plot(dati, ylab='yreal / yfit')
abline(modLin$coefficient[1], modLin$coefficient[2])
points(dati$peso, modLin$fitted, col=2)
par(mfrow=c(2,2))
plot(modLin)
shapiro.test(modLin$residuals)

# REGRESSIONE MULTIPLA
dati<- read.table('regMult1.txt', sep=" ", header=T)
cor(dati) 
modLinM<-lm(Ingestione~Peso+ProdLatte, data=dati)
summary(modLinM)
par(mfrow = c(2, 2))
plot(modLinM)
shapiro.test(modLinM$residuals)



# REGRESSIONE STEPWISE
# rimozione manuale delle variabili
dati<-read.table(paste(path, "misSomT.txt", sep=""), dec=".", header=T)
mlCompl<-lm(Ct ~ ., data=dati)
summary(mlCompl)
mod1<-update(mlCompl, .~.-LGLL-LGP)
summary(mod1)
drop1(mlCompl, test="F")
mlDrop<-update(mlCompl, .~.-LGLL)
summary(mlDrop)

# rimozione automatica delle variabili

modLin<-lm(Ct ~ ., data=dati)
summary(modLin)
modStep<-step(modLin, direction="both")
summary(modStep)




# ANALISI DI COVARIANZA

hellung <- read.csv2('hellung.csv', header = T)

attach(hellung)
hellung$glucose <- factor(hellung$glucose, labels=c("Yes","No"))
summary(hellung)
# Plot dei dati grezzi
plot(conc,diameter,pch=as.numeric(glucose))
# per posizionare una legenda
legend(locator(n=1),legend=c("glucose","no glucose"),pch=1:2)

# Se usiamo un asse logaritmico
plot(conc,diameter,pch=as.numeric(glucose),log="x")

# Oppure un log-log
plot(conc,diameter,pch=as.numeric(glucose),log="xy")

# Definiamo ora le due differenti rette di regressione
# I 2 differenti data.frame (ci servono solo per mettere le rette!!!!)
tethym.gluc <- hellung[which(hellung$glucose=="Yes"),]
tethym.nogluc <- hellung[which(hellung$glucose=="No"),]
lm.nogluc <- lm(log10(diameter)~ log10(conc),data=tethym.nogluc)
lm.gluc <- lm(log10(diameter)~ log10(conc),data=tethym.gluc)
abline(lm.nogluc)
abline(lm.gluc)

# Confronto delle rette di regressione
summary(lm(log10(diameter)~ log10(conc), data=tethym.gluc))

summary(lm(log10(diameter)~ log10(conc), data=tethym.nogluc))


# Se confrontiamo la differenza tra i due b (-0.05320 e -0.059677) viene 0.0065
# Errore standard di questa differenza è pari a sqroot(0.00272^2+0.004125^2) = 0.0049
# t = 0.0065/0.0049 = 1.3
# Stessa pendenza (per essere diversa al 5% doveva venire circa > 2 (1.96))

# INSERENDO TUTTO IN UN UNICO COMANDO

summary(lm(log10(diameter)~log10(conc)*glucose))


# Il risultato andrebbe letto:
# The intercept, 1.6313
# -0.0532*log10 C
# 0.0034, but only for a culture without glucose
# -0.0065*log10 C, but only for cultures without glucose

# Adattando un modello additivo avremo
summary(lm(log10(diameter)~log10(conc)+glucose))

# Estimated relation for cultures with glucose is
# log10 D = 1.6421-0.0554*log10 C
# and for cultures without glucose it is
# log10 D = (1.6421-0.0282) ???0.0554*log10 C

# Stesso b ma diversa intercetta

# L'analisi congiunta assume che le varianze siano omogenee
var.test(lm.gluc,lm.nogluc)

# OK

# Se ci fossero più di due gruppi si dovrà usare il test di Bartlett
detach(hellung)


# Esempi Corso Statistica Benedettelli

# Regressione
x <- c(5, 10, 15, 20, 25, 30, 35)
y <- c(33, 44, 62, 56, 74, 71, 80)

fit <- lm (y ~ x)
summary(fit)


# Lack of fit
w <- c(6,  6,  6,  8,  8,  8, 10, 10, 10, 12, 12, 12, 14, 14, 14)
z <- c(7, 20, 13, 35, 32, 19, 38, 58, 60, 53, 70, 58, 77, 73, 72)
fit1 <- lm(z ~ w)
summary(fit1)

fit2 <- lm(z ~ factor(w))
anova(fit1, fit2, test="F")

# Correlazione
x1 <- c(4, 8, 5, 6 , 7 ,5, 8, 7, 6)
x2 <- c(35, 65, 60, 45, 55, 44, 75, 61, 65)

cor.test(x1,x2)

# Esempio ANOVA
anova1 <- read.csv2('anova1.txt', header = T)

fit3 <-aov(X ~ NITR + VAR + NITR*VAR, data=anova1)
summary(fit3)



##############################################################################
##############################################################################
##############################################################################

############### DISEGNI SPERIMENTALI


# COMPLETELY RANDOMIZED DESIGN
crd <- read.table("crd.txt", header=TRUE)
str(crd)

av <- aov(sales ~ trt, data=crd )
summary(av) # display Type I ANOVA table
drop1(av,~.,test="F") # type III SS and F Tests 


# Esempio grasso latte bovine 
grasso_p<- c(3.25,3.90,4.50,2.60,3.10,4.30,2.55,4.55,4.60,3.90,5.20,4.50)
trattamento <- c('B','B','B','B','M','M','M','M','A','A','A','A')
esempio1<- data.frame(grasso_p,trattamento)
analisi_grasso_p <- lm(grasso_p~trattamento,data=esempio1)
anova(analisi_grasso_p)

confronto <- aov(analisi_grasso_p)
TukeyHSD(confronto)




# RANDOMIZED BLOCK DESIGN
rbd <- read.table("rbd.txt", header=TRUE)
str(rbd)
rbd$block <- as.factor(rbd$block)

av1 <- aov(sales ~ trt + block, data=rbd )
summary(av1)
drop1(av1,~.,test="F") # type III SS and F Tests 


# Esempio rbd grasso latte bovine 
grasso_p1 <- c(4.50,3.90,3.25,2.60,4.55,4.30,3.10,2.55,5.20,4.60,4.50,3.90)
trattamento1 <- c('B','B','B','B','M','M','M','M','A','A','A','A')
blocco <- c('JE','BR','GA','FR','JE','BR','GA','FR','JE','BR','GA','FR')
esempio2 <- data.frame(grasso_p1,trattamento1,blocco)
analisi_grasso_p1 <- lm(grasso_p1~trattamento1 + blocco,data=esempio2)
anova(analisi_grasso_p1)

confronto1 <- aov(analisi_grasso_p1)
TukeyHSD(confronto1)


# FACTORIAL DESIGN
fd <- read.csv("fd.csv")
str(fd)

av2 <- aov(r ~ tm1*tm2, data=fd)
summary(av2)


# Esempio FACTORIAL SUINI
peso_vivo <- c(49,39,50,55,43,38,53,48,55,41,67,58,53,42,85,73,66,68,85,92,69,62,85,99)
tipo_genetico <- c('TG1','TG1','TG1','TG1','TG1','TG1','TG1','TG1', 
                   'TG2','TG2','TG2','TG2','TG2','TG2','TG2','TG2',
                 'TG3','TG3','TG3','TG3','TG3','TG3','TG3','TG3')
sistema_allev <- c('SA1','SA1','SA2','SA2','SA3','SA3','SA4','SA4',
                 'SA1','SA1','SA2','SA2','SA3','SA3','SA4','SA4',
                 'SA1','SA1','SA2','SA2','SA3','SA3','SA4','SA4')
esempio3 <- data.frame(peso_vivo,tipo_genetico,sistema_allev)
analisi_peso_vivo <- lm(peso_vivo~tipo_genetico + sistema_allev + tipo_genetico*sistema_allev,data=esempio3)
anova(analisi_peso_vivo)
interaction.plot(esempio3$sistema_allev, esempio3$tipo_genetico, esempio3$peso_vivo, type='l', main='Grafico interazione',
                xlab='Sistema Allevamento', ylab='Peso Vivo', trace.label='Tipo Genetico')

confronto2 <- aov(analisi_peso_vivo)
TukeyHSD(confronto2)



# DISEGNO GERARCHICO

bufali <- read.csv("bufali.csv", header=T)
bufali$ordine_parto <- factor(bufali$ordine_parto)
bufali$eta <- factor(bufali$eta)
bufali
analisi <- lm(produzione_latte ~ ordine_parto + eta%in%ordine_parto, data=bufali)
anova(analisi)
confronto3 <- aov(analisi)
TukeyHSD(confronto3,"ordine_parto")


#esempio di disegno nested per gli esercizi
ariete <- factor(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4))
pecora <- factor(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12))
peso_nascita <- c(4.20, 4.75, 4.28, 5.35, 4.10, 4.25, 4.22, 4.45, 5.10, 5.20, 4.16, 4.15, 4.22, 4.28, 4.35, 4.35, 5.20, 5.20, 4.43, 4.58, 4.45, 5.40, 4.37, 4.37)

agnelli<- data.frame(ariete,pecora,peso_nascita)
analisi1 <- lm(peso_nascita~ariete+pecora%in%ariete, data=agnelli )
anova(analisi1)

#Trattando il modello come random
library(nlme)
model <- lme(peso_nascita~ariete, random=~1|pecora, data=agnelli,method="REML")
anova.lme(model)


# Modello misto,fattore random
vacche <- read.table("vacche.txt", sep='\t', dec=".", header=T)
vacche$Mese_di_lattazione <- factor(vacche$Mese_di_lattazione)
vacche$Numero_bovina <- factor(vacche$Numero_bovina)
library(lme4)
library(lmerTest)
mixed_model <- lmer(latte~Mese_di_lattazione + (1|Numero_bovina), data=vacche,REML=T)
summary(mixed_model, corr=F)
anova(mixed_model)


# SPLIT-PLOT
blocco <- factor(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3))
varieta <- factor(c(1,2,3,1,2,3,2,3,1,2,3,1,3,1,2,3,1,2))
fertilizzante <- factor(c(1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2))
produzione <- c(10.6,11.4,11.8,10.9,11.7,12.4,11.9,12.6,11.6,11.5,12.1,10.8,9.5,8.1,8.7,9.8,8.2,9.3)
prova <- data.frame(blocco,varieta,fertilizzante, produzione)
split_plot <- aov(produzione~fertilizzante + Error(blocco/fertilizzante) +varieta +fertilizzante*varieta , data=prova)
summary(split_plot)


# Disegno split-plot con vacche
parcella <- factor(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12))
blocco1 <- factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3))
pascolo <- factor(c(4,4,1,1,2,2,3,3,2,2,1,1,4,4,3,3,1,1,2,2,4,4,3,3))
integrazione <- factor(c(2,1,2,1,1,2,2,1,1,2,2,1,1,2,1,2,2,1,1,2,2,1,1,2))
latte <- c(30,29,27,25,26,28,26,24,32,37,30,31,34,37,33,32,34,31,30,31,36,38,33,32)
prova1 <- data.frame(blocco1,parcella,pascolo,integrazione, latte)
split_plot1 <- aov(latte~pascolo+Error(blocco1/pascolo)+integrazione+pascolo*integrazione, data=prova1)
summary(split_plot1)


# CHANGE-OVER DESIGN (da Kops and Lambertson)
periodo <- factor(c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2))
trattamento <- factor(c(1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1))
vacca <- factor(c(1,4,5,9,10,1,4,5,9,10,2,3,6,7,8,2,3,6,7,8))
latte1 <- c(31,34,43,28,25,27,25,38,20,19,22,40,40,33,18,21,39,41,34,20)
bovine <- data.frame(periodo,trattamento,vacca,latte1)
change_over <- lmer(latte1~trattamento+(1|vacca))
anova(change_over)
summary(change_over)


# CHANGE-OVER DESIGN CON PERIODO E SEQUENZA (da Kops and Lambertson)
periodo1 <- factor(c(1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,2,2,2,2,2))
trattamento1 <- factor(c(1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1))
sequenza <- factor(c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2))
vacca1 <- factor(c(1,4,5,9,10,1,4,5,9,10,2,3,6,7,8,2,3,6,7,8))
latte2 <- c(31,34,43,28,25,27,25,38,20,19,22,40,40,33,18,21,39,41,34,20)
bovine1 <- data.frame(periodo1,trattamento1,sequenza,vacca1,latte2)
change_over2 <- lmer(latte2~trattamento1+periodo1+sequenza+(1|vacca1:sequenza))
summary(change_over2)
anova(change_over2)



# QUADRATO LATINO (Kops and Lambertson)

periodo2 <- factor(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4))
animale <- factor(c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4))
supplemento <- factor(c(2,4,3,1,3,1,4,2,4,2,1,3,1,3,2,4))
ingestione <- c(10.0,9.0,11.1,10.8,10.2,11.3,9.5,11.4,8.5,11.2,12.8,11.0,11.1,11.4,11.7,9.9)
vitelloni <- data.frame(periodo2,animale,supplemento,ingestione)

latin_square <- aov(ingestione~periodo2+animale+supplemento,data=vitelloni)
summary(latin_square)

library(lsmeans)
lsmeans(latin_square,specs="supplemento")
confronto4 <- aov(latin_square)
TukeyHSD(confronto4,"supplemento")


# ALTRO QUADRATO LATINO

urea <- read.table('Urea_ovini.csv', header=T, sep=';', dec=',')
urea$Trattamento <- factor(urea$Trattamento)
urea$settimana <- factor(urea$settimana)
urea$Pecora <- factor(urea$Pecora)
analisi_urea <- lm(urea.N~Trattamento+settimana+Pecora, data=urea)
anova(analisi_urea)
library(lsmeans)
lsmeans(analisi_urea,specs='Trattamento')
confronto5 <- aov(analisi_urea)
TukeyHSD(confronto5,'Trattamento')



# Polli alimentati con 3 differenti diete
polli<-read.table("Polli3dieteincr.txt", header=T)
dim(polli)
attach(polli)
polli

summary(polli)

hist(peso,prob=TRUE,main="Istogramma Incremento Ponderale",xlab="Incremento ponderale (g)",ylab="Densità")

boxplot(peso,horizontal=FALSE,main="Boxplot Incremento Ponderale",ylab="Incremento ponderale(g)")

boxplot(peso[sesso=='female'],peso[sesso=='male'],horizontal=FALSE,
        main='Boxplot Incremento ponderale polli',names=c('Femmine','Maschi'),col=c('pink','lightblue'),
        ylab='Incremento ponderale(g)',ylim=c(50,80))

boxplot(peso[dieta=='casein'],peso[dieta=='linseed'],peso[dieta=='sunflower'],
        horizontal=FALSE,main='Boxplot Incremento ponderale polli',names=c('Casein','Linseed','Sunflower'),
        col=c('orange','blue','red'),ylab='Incremento ponderale(g)',ylim=c(50,80))


#Valutiamo la normalità dei dati
shapiro.test(peso[dieta == "casein"])
shapiro.test(peso[dieta == "linseed"])
shapiro.test(peso[dieta == "sunflower"])


# Oppure
n<-(table(dieta))
n
N<-sum(n)
N
treat<-levels(dieta)
treat
k<-length(treat)
par(mfrow=c(1,3))

for (i in c(1:k)){
        dati<-peso[dieta==treat[i]]
        qqnorm(dati,datax=T,main=paste('NPP',treat[i],sep=''))
        Peso.ord<-sort(dati)
        ranghi<-1:n[i]
        Femp<-(ranghi-0.5)/n[i]
        z.j<-qnorm(Femp)
        y.j<-lm(z.j ~ Peso.ord)$fitted.values
        lines(Peso.ord,y.j,col='red',lwd=2)
        }





# omogeneita della varianza può essere valutato con

bartlett.test(peso,dieta)

# Secondo alcuni autori  poiché il test di Bartlett, è sensibile alla mancanza di normalità, è meglio
# preferire) il test robusto di Levene che lavora di default sulle mediane  di default: bisognerà scaricare la library lawstat
# install.packages('lawstat')

library(lawstat)
# il test di leveneTest(dati, gruppi, location= mena or median (default)
levene.test(peso,dieta)



#Verificati gli assunti procediamo con l'analisi

Media<-mean(peso)  # media della variabile peso
Media
Media.gr<-tapply(peso,dieta,mean) # media dei livelli del fattore dieta

# varianza fra i gruppi o between 
# scarto quadratico medio annullando la variabilità all'interno del gruppo 
# e ponendo ogni valore del gruppo = alla media del gruppo per ciascuno dei tre gruppi 
# scartando questi valori dalla media generale
MS.b<-sum(n*(Media.gr-Media)^2)/(k-1) 
MS.b
# varianza entro gruppi o residua scarto quadratico medio residuo all'interno del gruppo
# within quanto scarta ogni osservazione dalla media del proprio gruppo
MS.w<-sum((n-1)*tapply(peso,dieta,var))/(N-k) 
MS.w
alpha<-0.05
# F teorico
F.alpha<-qf(1-alpha,k-1,N-k)  
F.alpha

#F empirico od osservato nei dati
F.O<-MS.b/MS.w 
F.O
# P-Value del valore di F osservato
pvalue<-1-pf(F.O,k-1,N-k) 
pvalue
# modello di Anova ad un fattore dieta
fit<-aov(peso~ dieta) 
summary(fit)


plot.design(peso ~ dieta)
model.tables(fit)

model.tables(fit,type='effects')
model.tables(fit, type = "means")
model.matrix(fit) 

fit$coefficients  
confint(fit, level=0.95)

par(mfrow = c(2,2))
plot(fit)

#Sullo stesso dataset possiamo fare l'ANOVA ad 1 via per il fatfore sesso  possiamo utilizzare tre approcci:

# viene riportato il p-value (calcolato rispetto alla variabile aleatoria t) 
# e le medie del peso nei due gruppi, Female ed Male
t.test(peso ~ sesso) 
        
# Nel secondo caso viene presentata la 'tavola' dell'ANOVA nella quale, tra le varie informazioni,
# viene  un p-value 9.96e-06  la cui lieve discrepanza  è da attribuire alla diversa variabile aleatoria, 
# la F utilizzata:        
summary( aov(peso ~ sesso) )  

# con questo comando, viene riportato lo stesso p-value dell'ANOVA, 
# ma ci vengono fornite molte altre informazioni, utili a giudicare la bontà del modello utilizzato: 
summary( lm(peso ~ sesso) ) 

detach(polli)



# simulare un dataset per un Latin Square

set.seed(12345)

# Trattamento: A B C D E
#
#                Tempo
#          1   2   3   4   5 
#  A
#  n  I    C   D   A   E   B
#  i  II   B   A   C   D   E
#  m  III  A   C   E   B   D
#  a  IV   D   E   B   C   A
#  l  V    E   B   D   A   C
#  e

costante <- rep(100, 25)


animale <- gl(5, 1, 25, labels = c(paste("Anim_", 1:5, sep = "")))
effetto_animale <- animale
levels(effetto_animale) <- rnorm(5, 0, 3)
animale
effetto_animale

tempo <- gl(5, 5, labels = c(paste("T_", 1:5, sep = "")))
effetto_tempo <- tempo
levels(effetto_tempo) <- c(1, 4, -2, 0, -3)
tempo
effetto_tempo


trattamento <- factor(c(
        "C", "D", "A", "E", "B",
        "B", "A", "C", "D", "E",
        "A", "C", "E", "B", "D",
        "D", "E", "B", "C", "A",
        "E", "B", "D", "A", "C"
))

effetto_trattamento <- trattamento
levels(effetto_trattamento) <- c(12, 3, 0, -6, -9)

trattamento
effetto_trattamento


errore <- rnorm(25, 0, 5)
errore

osservazione = costante + as.numeric(as.character(effetto_animale)) + as.numeric(as.character(effetto_tempo)) + as.numeric(as.character(effetto_trattamento)) + errore
osservazione

Dataset_LS <- data.frame(animale, tempo, trattamento, osservazione)
Dataset_LS


# tutti effetti fissi di interesse (5 tipi di seme, 5 tipi di concimi, 5 tecniche di irrigazione)

summary(Dataset_LS)

lm_out <-lm(osservazione ~ trattamento + tempo + animale, data = Dataset_LS)
summary(lm_out)
plot(lm_out)

with(Dataset_LS, tapply(osservazione, tempo, function(x) c(mean(x), sd(x))))
with(Dataset_LS, tapply(osservazione, trattamento, function(x) c(mean(x), sd(x))))
with(Dataset_LS, tapply(osservazione, animale, function(x) c(mean(x), sd(x))))

oldpar <- par()
par(mfrow = c(2, 2))
plot(osservazione ~ trattamento + tempo + animale, data = Dataset_LS)
par(oldpar)

aov_out <- aov(osservazione ~ trattamento + tempo + animale, data = Dataset_LS)
summary(aov_out)
TukeyHSD(aov_out, which = "trattamento")
TukeyHSD(aov_out, which = "tempo")
TukeyHSD(aov_out, which = "animale")

# fattori casuali

library(lme4)

modello_casuale <- lmer(osservazione ~ trattamento + tempo +(1|animale), data = Dataset_LS)
summary(modello_casuale)
ranef(modello_casuale)
fixef(modello_casuale)

# controlla pvalues in lme4

library(car)

Anova(modello_casuale)
summary(aov(osservazione ~ trattamento + tempo, data = Dataset_LS))

modello_casuale_2 <- lmer(osservazione ~ trattamento + (1|tempo) +(1|animale), data = Dataset_LS)
summary(modello_casuale_2)
ranef(modello_casuale_2)
fixef(modello_casuale_2)
Anova(modello_casuale_2)
summary(aov(osservazione ~ trattamento, data = Dataset_LS))


library(easyanova)
data()
data(data3)
data3
ea1(data = data3, design = 3)

ea1(data = Dataset_LS, design = 3) #ooops...
Dataset_LS <- Dataset_LS[ , c("trattamento", "animale", "tempo", "osservazione")]
ea1(data = Dataset_LS, design = 3)








