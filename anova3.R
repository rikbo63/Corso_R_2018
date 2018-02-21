
if(!require(car)){install.packages("car")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcomp)){install.packages("multcomp")}
library(car)
   
Input = ("
Treatment   Response
            'D1:C1'    1.0
            'D1:C1'    1.2
            'D1:C1'    1.3
            'D1:C2'    2.1
            'D1:C2'    2.2
            'D1:C2'    2.3
            'D2:C1'    1.4
            'D2:C1'    1.6
            'D2:C1'    1.7
            'D2:C2'    2.5
            'D2:C2'    2.6
            'D2:C2'    2.8
            'Control'  1.0
            'Control'  0.9
            'Control'  0.8
            ")
   
Data = read.table(textConnection(Input),header=TRUE)
   
   
### Specify the order of factor levels. Otherwise R will alphabetize them.
   
Data$Treatment = factor(Data$Treatment,
                           levels=unique(Data$Treatment))
   
Data
   
boxplot(Response ~ Treatment,
           data = Data,
           ylab="Response",
           xlab="Treatment")   

###  Define linear model
model = lm(Response ~ Treatment,
           data = Data)


Anova(model, type="II")
summary(model)

###  You need to look at order of factor levels to determine the contrasts
levels(Data$Treatment)


library(lsmeans)

leastsquare = lsmeans(model, "Treatment")

Contrasts = list(D1vsD2          = c(1,  1, -1, -1,  0),
                 C1vsC2          = c(1, -1,  1, -1,  0),
                 InteractionDC   = c(1, -1, -1,  1,  0),
                 C1vsC2forD1only = c(1, -1,  0,  0,  0),
                 C1vsC2forD2only = c(0,  0,  1, -1,  0),
                 TreatsvsControl = c(1,  1,  1,  1, -4),
                 T1vsC           = c(1,  0,  0,  0, -1),
                 T2vsC           = c(0,  1,  0,  0, -1),
                 T3vsC           = c(0,  0,  1,  0, -1),
                 T4vsC           = c(0,  0,  0,  1, -1))

### The column names match the order of levels of the treatment variable
### The coefficients of each row sum to 0

contrast(leastsquare, Contrasts, adjust="sidak")


# Example for global F-test within a group of treatments

Input = ("
Treatment          Response
         Merlot             5
         Merlot             6
         Merlot             7
         Cabernet           8
         Cabernet           9
         Cabernet          10
         Syrah             11
         Syrah             12
         Syrah             13
         
         Chardonnay         1
         Chardonnay         2
         Chardonnay         3
         Riesling           1
         Riesling           2
         Riesling           2
         Gewürtztraminer    1
         Gewürtztraminer    2
         Gewürtztraminer    4
         ")

Data = read.table(textConnection(Input),header=TRUE)


### Specify the order of factor levels. Otherwise R will alphabetize them.
Data$Treatment = factor(Data$Treatment,
                        levels=unique(Data$Treatment))

Data

boxplot(Response ~ Treatment,
        data = Data,
        ylab="Response",
        xlab="Treatment")

###  You need to look at order of factor levels to determine the contrasts

levels(Data$Treatment)

###  Define linear model
model = lm(Response ~ Treatment,
           data = Data)

library(car)
Anova(model, type="II")
summary(model)




# Tests of contrasts with lsmeans

# Question: Is there an effect within red wine ?

library(lsmeans)

leastsquare = lsmeans(model, "Treatment")

Contrasts = list(Red_line1   = c(1, -1,  0,  0,  0,  0),
                 Red_line2   = c(0,  1, -1,  0,  0,  0))

### The column names match the order of levels of the treatment variable
### The coefficients of each row sum to 0

Test = contrast(leastsquare, Contrasts)

test(Test, joint=TRUE)



# Question: Is there an effect within white wine ?

Contrasts = list(White_line1   = c(0,  0,  0,  1, -1,  0),
                 White_line2   = c(0,  0,  0,  0,  1, -1))

### The column names match the order of levels of the treatment variable
### The coefficients of each row sum to 0

Test = contrast(leastsquare, Contrasts)

test(Test, joint=TRUE)



# Question: Is there a difference between red and white wines?  And, mean separation for red wine
Contrasts = list(Red_vs_white    = c( 1,  1,  1, -1, -1, -1),
                 Merlot_vs_Cab   = c( 1, -1,  0,  0,  0,  0),
                 Cab_vs_Syrah    = c( 0,  1, -1,  0,  0,  0),
                 Syrah_vs_Merlot = c(-1,  0,  1,  0,  0,  0))

### The column names match the order of levels of the treatment variable
### The coefficients of each row sum to 0

contrast(leastsquare, Contrasts, adjust="sidak")
