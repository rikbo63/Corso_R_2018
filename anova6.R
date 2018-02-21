if(!require(psych)){install.packages("psych")}
if(!require(ordinal)){install.packages("ordinal")}
if(!require(car)){install.packages("car")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcompView)){install.packages("multcompView")}

Input =("
 Speaker  Likert
        Pooh      3
        Pooh      5
        Pooh      4
        Pooh      4
        Pooh      4
        Pooh      4
        Pooh      4
        Pooh      4
        Pooh      5
        Pooh      5
        Piglet    2
        Piglet    4
        Piglet    2
        Piglet    2
        Piglet    1
        Piglet    2
        Piglet    3
        Piglet    2
        Piglet    2
        Piglet    3
        Tigger    4
        Tigger    4
        Tigger    4
        Tigger    4
        Tigger    5
        Tigger    3
        Tigger    5
        Tigger    4
        Tigger    4
        Tigger    3
        ")

Data = read.table(textConnection(Input),header=TRUE)


### Order levels of the factor; otherwise R will alphabetize them

Data$Speaker = factor(Data$Speaker,
                      levels=unique(Data$Speaker))


### Create a new variable which is the likert scores as an ordered factor

Data$Likert.f = factor(Data$Likert,
                       ordered = TRUE)


###  Check the data frame

library(psych)

headTail(Data)

str(Data)

summary(Data)


### Remove unnecessary objects

rm(Input)


# Define model and conduct analysis of deviance

library(ordinal)

model = clm(Likert.f ~ Speaker,
            data = Data)

library(car)
library(RVAideMemoire)

Anova(model,
      type = "II")


# Group separations by lsmeans in table format
library(lsmeans)

lsmeans(model,
        pairwise ~ Speaker,
        adjust="tukey")       ### Tukey-adjusted comparisons

# Compact letter display
library(multcompView)


leastsquare = lsmeans(model,
                      pairwise ~ Speaker,
                      adjust="tukey")      ###  Tukey-adjusted comparisons

cld(leastsquare,
    alpha=0.05,
    Letters=letters,      ### Use lower-case letters for .group
    adjust="tukey")       ### Tukey-adjusted comparisons  



# Input data and specify linear model


if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}


Location = c(rep("Olympia" , 6), rep("Ventura", 6),
             rep("Northampton", 6), rep("Burlington", 6))

Tribe  = c(rep(c("Jedi", "Sith"), 12))

Midichlorians = c(10,  4, 12,  5, 15,  4, 15,  9, 15, 11, 18, 12,
                  8, 13,  8, 15, 10, 17, 22, 22, 20, 22, 20, 25)


Data = data.frame(Tribe, Location, Midichlorians)

str(Data)


model = lm(Midichlorians ~ Tribe + Location + Tribe:Location,
           data = Data)

library(car)
Anova(model)


# One-way least square means and plot

library(multcompView)
library(lsmeans)

leastsquare = lsmeans(model,
                      pairwise ~ Location,
                      adjust="tukey")

CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")

CLD


### Order the levels for printing

CLD$Location = factor(CLD$Location,
                      levels=c("Olympia", "Ventura", "Northampton", "Burlington"))

###  Remove spaces in .group 

CLD$.group=gsub(" ", "", CLD$.group)


### Plot

library(ggplot2)
pd = position_dodge(.2)
ggplot(CLD,
       aes(x     = Location,
           y     = lsmean,
           label = .group)) +
        
        geom_point(shape  = 15,
                   size   = 4,
                   position = pd) +
        
        geom_errorbar(aes(ymin  =  lower.CL,
                          ymax  =  upper.CL),
                      width =  0.2,
                      size  =  0.7,
                      position = pd) +
        
        theme_bw() +
        theme(axis.title   = element_text(face = "bold"),
              axis.text    = element_text(face = "bold"),
              plot.caption = element_text(hjust = 0)) +
        
        ylab("Least square mean\nmidichlorian count") +
        ggtitle ("Midichlorian counts",
                 
                 subtitle = "In four U.S. cities") +
        
        labs(caption  = paste0("\nMidichlorian counts for four locations. ",
                               "Boxes indicate the LS mean. \n",
                               "Error bars indicate the 95% ",
                               "confidence interval of the LS mean. \n",
                               "Means sharing a letter are not ",
                               "significantly different (Tukey-adjusted \n",
                               "comparisons)."),
             hjust=0.5) +
        
        geom_text(nudge_x = c(0, 0, 0, 0),
                  nudge_y = c(4, 4, 4, 4),
                  color   = "black")


# Interaction plot of least square means


library(multcompView)
library(lsmeans)

leastsquare = lsmeans(model,
                      pairwise ~ Tribe:Location,
                      adjust="tukey")

CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")

CLD

### Order the levels for printing

CLD$Location = factor(CLD$Location,
                      levels=c("Olympia", "Ventura", "Northampton", "Burlington"))

CLD$Tribe = factor(CLD$Tribe,
                   levels=c("Jedi", "Sith"))

###  Remove spaces in .group 

CLD$.group=gsub(" ", "", CLD$.group)


CLD


### Plot

library(ggplot2)

pd = position_dodge(0.4)    ### How much to jitter the points on the plot

ggplot(CLD,
       aes(x     = Location,
           y     = lsmean,
           color = Tribe,
           label = .group)) +
        
        geom_point(shape  = 15,
                   size   = 4,
                   position = pd) +
        
        geom_errorbar(aes(ymin  =  lower.CL,
                          ymax  =  upper.CL),
                      width =  0.2,
                      size  =  0.7,
                      position = pd) +
        
        theme_bw() +
        theme(axis.title   = element_text(face = "bold"),
              axis.text    = element_text(face = "bold"),
              plot.caption = element_text(hjust = 0)) +
        
        ylab("Least square mean\nmidichlorian count") +
        ggtitle ("Midichlorian counts for Jedi and Sith",
                 subtitle = "In four U.S. cities") +
        
        labs(caption  = paste0("\nMidichlorian counts for two tribes across ",
                               "four locations. Boxes indicate \n",
                               "the LS mean. ",
                               "Error bars indicate the 95% confidence ",
                               "interval ",
                               "of the LS \n",
                               "mean. Means sharing a letter are ",
                               "not significantly different \n",
                               "(Tukey-adjusted comparisons)."),
             hjust=0.5) +
        
        geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
                  nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
                  color   = "black") +
        
        scale_color_manual(values = c("blue", "red"))


