irrigation<-factor(c(rep("Control",10),rep("Irrigated 10 mm",10),
                     rep("Irrigated 20 mm",10)))
biomass<-1:30

irrig <- as.data.frame(cbind(biomass,irrigation))

summary(aov(biomass~irrigation))


library(gplots)
# Plot the mean of teeth length by dose groups
plotmeans(biomass ~ irrigation, data = irrig, mean.labels = TRUE)

plot(irrigation,biomass)


# So, we would expect:
# Control differs significantly from 10 mm
# 
# 10 mm differs significantly from 20 mm
# 
# Control differs significantly from 20 mm
# 
# 20 mm differs significantly from 10 mm
# 
# 20 mm differs significantly from Control


# There are only k-1 orthogonal comparisons (where k is the number of factor levels, which is 3 in our case).

contrasts(irrigation)

summary.lm(aov(biomass~irrigation))

# What does this all mean?
# 
# The parameter labelled "Intercept" is the mean for the Control treatment (5.5)
#
# The parameter labelled "irrigationIrrigated 10 mm" is the difference between the mean of 10 mm and the Control mean (10.0)
#
# The parameter labelled "irrigationIrrigated 20 mm" is the difference between the 20 mm mean and the Control mean (20.0)
#
# Thus, our Control mean was 5.5; our 10 mm mean was (5.5+10.0)=15.5; and our 20 mm mean was (5.5+20.0)=25.5


# Look at Diagnostic Plots

# Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations.
fit <- aov(biomass~irrigation)
layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots


# Let´s now assume we would rather like to have a different kind of comparison: 
# We want to use the 20 mm mean as the "standard" against which the others should be tested.


# We first create two column vectors: One will be c(0,1,-1), and the other one will be c(-1,1,0). 
# We bind these vectors together using cbind(), and inspect the result. 
# Lets call our contrast matrix "contrastmatrix":

contrastmatrix<-cbind(c(0,1,-1),c(-1,1,0))
contrastmatrix

contrasts(irrigation)<-contrastmatrix
summary.lm(aov(biomass~irrigation))


# We see the change:
# (1) The parameter called "intercept" is now the 10 mm treatment mean (15.5).
# 
# (2) The parameter called "irrigation1" is our first comparison (between 10mm and 20 mm)


# R offers several built-in kinds of contrasts. They are specified using
contr.treatment(levels(irrigation))


# Thus, you can use the following syntax to create treatment contrasts (the ones we used in our first contrast matrix above):

contrasts(irrigation)<- contr.treatment(levels(irrigation))


