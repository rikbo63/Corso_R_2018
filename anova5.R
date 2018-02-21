if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}

Input =("
Diet    Country  Weight_change
        A       USA      0.120
        A       USA      0.125
        A       USA      0.112
        A       UK       0.052
        A       UK       0.055
        A       UK       0.044
        B       USA      0.096
        B       USA      0.100
        B       USA      0.089
        B       UK       0.025
        B       UK       0.029
        B       UK       0.019
        C       USA      0.149
        C       USA      0.150
        C       USA      0.142
        C       UK       0.077
        C       UK       0.080
        C       UK       0.066
        ")

Data = read.table(textConnection(Input),header=TRUE)


### Order levels of the factor; otherwise R will alphabetize them
Data$Country = factor(Data$Country,
                      levels=unique(Data$Country))


###  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

### Remove unnecessary objects
rm(Input)

par(mfrow=c(1,1))
interaction.plot(x.factor     = Data$Country,
                 trace.factor = Data$Diet,
                 response     = Data$Weight_change,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")




# Specify the linear model and conduct an analysis of variance 
model = lm(Weight_change ~ Country + Diet + Country:Diet, data = Data)

library(car)

Anova(model, type = "II")

# Post-hoc testing with lsmeans
library(lsmeans)

lsmeans(model, pairwise ~ Country, adjust="tukey")       ### Tukey-adjusted comparisons


lsmeans(model, pairwise ~ Diet, Adjust="tukey")       ### Tukey-adjusted comparisons



# Extended example with additional country

Input =("
Diet    Country  Weight_change
        A       USA      0.120
        A       USA      0.125
        A       USA      0.112
        A       UK       0.052
        A       UK       0.055
        A       UK       0.044
        A       NZ       0.080
        A       NZ       0.090
        A       NZ       0.075
        B       USA      0.096
        B       USA      0.100
        B       USA      0.089
        B       UK       0.025
        B       UK       0.029
        B       UK       0.019
        B       NZ       0.055
        B       NZ       0.065
        B       NZ       0.050
        C       USA      0.149
        C       USA      0.150
        C       USA      0.142
        C       UK       0.077
        C       UK       0.080
        C       UK       0.066
        C       NZ       0.055
        C       NZ       0.065
        C       NZ       0.050
        C       NZ       0.054
        ")

Data = read.table(textConnection(Input),header=TRUE)


### Order levels of the factor; otherwise R will alphabetize them
Data$Country = factor(Data$Country, levels=unique(Data$Country))

###  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

### Remove unnecessary objects
rm(Input)


# Simple interaction plot

interaction.plot(x.factor     = Data$Country,
                 trace.factor = Data$Diet,
                 response     = Data$Weight_change,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")


# Specify the linear model and conduct an analysis of variance
model = lm(Weight_change ~ Country + Diet + Country:Diet, data = Data)

Anova(model, type = "II")

# Post-hoc testing with lsmeans

leastsquare = lsmeans(model,
                      pairwise ~ Country:Diet,
                      adjust="tukey")           ### Tukey-adjusted comparisons

leastsquare$contrasts

cld(leastsquare,
    alpha=0.05,
    Letters=letters,      ### Use lower-case letters for .group
    adjust="tukey")       ### Tukey-adjusted comparisons  



# Interaction plot with error bars using ggplot2

library(FSA)

Sum = Summarize(Weight_change ~ Country + Diet,
                data=Data,
                digits=3)

### Add standard error of the mean to the Sum data frame

Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se = signif(Sum$se, digits=3)
Sum

### Order levels of the factor; otherwise R will alphabetize them

Sum$Country = factor(Sum$Country,
                     levels=unique(Sum$Country))


### Produce interaction plot

library(ggplot2)

pd = position_dodge(.2)

ggplot(Sum, aes(x = Country, y = mean, color = Diet)) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width=0.2, size=0.7, position=pd) +
        geom_point(shape=15, size=4, position=pd) + theme_bw() +
        theme(axis.title = element_text(face = "bold")) +
                      scale_colour_manual(values= c("black","red","green")) +
                      ylab("Mean weight change")





