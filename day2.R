getwd()


day1 <- "e:\\Corso_R_2018\\day_1"

setwd(day1)
getwd()
library(readr)
library(magrittr)
library(xlsx)
library(tibble)
library(dplyr)
library(tidyr)



# oggetto standard
f1Td <- read.table('FileTab.txt', header = T, sep = "\t", dec='.')
str(f1Td)

write.xlsx(f1Td, "testfile.xlsx")


## Importare diversi tipi di file

# Read tab separated values
read.delim('FileTab.txt', header = T) %>% head()

## Read csv

# Read comma (",") separated values
read.csv('FileTab1.csv', header = T) %>% head()

## Read csv2

# Read semicolon (";") separated values
read.csv2('FileTab2.csv', header = T) %>% head()


## Lavorare con i dati

# Dopo aver importato i dati possiamo modificarli:
# - Creare nuove variabili
# - Trasformare variabili esistenti
# - Escludere variabili
# - Selezionare variabili
# - Trasporre dati (girare) linee per colonne e viceversa

# - R ha molte funzioni che possono aiutare nel farlo:
# 1. reshape2 
# 2. tidyr 
# 3. dplyr




f1RR <- read_tsv('FileTab.txt')
f1RR
f1RRF <- filter(f1RR,PESO>200)
f1RRF
f1RRF2 <- filter(f1RR,PESO>200 & RAZZA =='LIMOUSINE')
f1RRF2
f1RRF %>% filter(PESO>200) # utilizzo il comando %>% 


f1RRF <- select(f1RR,PESO,RAZZA)

f1RR %>% filter(PESO>200) %>% select(PESO,RAZZA)



# Creare nuova variabile
f1RRF <- mutate(f1RR,x=PESO/2)

f1RR %>% filter(PESO>200) %>% select(PESO,RAZZA) %>% mutate(x=PESO/2)



# Dati riassuntivi
Media <- group_by(f1RR,RAZZA)
Media <- summarise(Media,Media=mean(PESO))

f1RR %>% group_by(RAZZA) %>% summarise(Media=mean(PESO))


# Medie e deviazioni standard
f1RR$FACT <- as.factor(f1RR$GIORNO)
stat1 <- f1RR %>% group_by(RAZZA) %>% summarise(Media=mean(PESO))

stat2 <- f1RR %>% group_by(RAZZA) %>% summarise(MediaA=mean(PESO), Devst=sd(PESO))

# Arrange
f1RRs <- arrange(f1RR,RAZZA)
f1RRs

f1RRs <- arrange(f1RR,RAZZA,PESO)
f1RRs

f1RRs <- arrange(f1RR,RAZZA,-PESO)
f1RRs


###

f1T <- read.csv2('ex5file.csv')

f1Tg <- gather(f1T,Samples,Count,Sample1:Sample4)
f1Tg

f1Tg <- gather(f1T,Samples,Count,Sample1:Sample2) # solo le prime due colonne
f1Tg


f1Tg <- gather(f1T,Samples,Count,-c(Sample1,rep)) # rimuovo la prima colonna
f1Tg

f10g <- spread(f1Tg,Samples,Count)


FU <- read.csv2('filev.csv')

FU2 <- unite(FU,col='Data',year,month,day,sep='-',remove=F)
FU2

FU2a <- separate(FU2,Data,c('anno','mese','giorno'),sep='_',remove=F)

FU2a

# MERGE
m3 <- as_data_frame(merge(f1T,f10g,by.x='rep',
                          by.y='rep',
                          all.x=T))

m3.25 <- as_data_frame(merge(f1T[1:25,],f10g,by.x='rep',
                             by.y='rep',
                             all.x=T))

# Count data
my_data <- as_data_frame(mtcars) # dati di R

my_data
table(my_data$carb)
table(my_data$gear,my_data$carb)

