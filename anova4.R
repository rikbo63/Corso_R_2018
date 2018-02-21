# first, generate some random data
n <- 128
cond <- gl(4, n/4, labels=c("A", "B", "C","Control"))
dose <- gl(4, 4, length=n, labels=c("None", "Low", "Med", "High"), ordered=TRUE)
data <- data.frame(cond=cond, dose=dose)
data$y <- ifelse(data$cond=="Control", rnorm(n, mean=100, sd=10), 
                 ifelse(data$cond=="A", rnorm(n, mean=110, sd=10),
                        rnorm(n, mean=120, sd=10)))
data$y <- ifelse(data$dose=="Low", data$y + 10, 
                 ifelse(data$dose=="Med", data$y + 15,
                        data$y))

str(data)

par(mfrow=c(1,2)) # put the next two plots side by side
plot(y ~ cond, data=data, main="By Condition")
plot(y ~ dose, data=data, main="By Dose")
par(mfrow=c(1,1)) # return to the default of one plot at a time

model1 <- lm(y ~ cond, data=data)
summary(model1)

# It automatically uses the first level as the reference group. 
# In some cases, there's another level you would prefer to use as the reference group 
# (for example, in these data, the control condition would probably be a better choice than condition A). 
# You can change the reference group with the relevel() command:
levels(data$cond)

data$cond <- relevel(data$cond, ref="Control")
levels(data$cond) # Note that the order of the levels has changed

model1.contrref <- lm(y ~ cond, data=data)
summary(model1.contrref) # Now control is being used as the refernce group, since it is now the first level


# It's also possible to relevel just inside the model, without saving it to the dataframe. 
# For example, if I want to use condition C as the reference group for this model, 
# but I don't want to save that new ordering to the datframe

model1.condCref <- lm(y ~ relevel(cond, ref="C"), data=data)
levels(data$cond) # note that it is not changed

summary(model1.condCref)

# To get type 3 sums of squares, use the Anova() function from the car package.)
model2 <- lm(y ~ cond * dose, data=data) # factorial ANOVA

library(car)
Anova(model2, type="III") 

# if we relevel condition, the sums of squares will change
model2.relevel <- lm(y ~ relevel(cond, ref="C") * dose, data=data)
Anova(model2.relevel, type="III")

# For an ANOVA, you should set your factors to use effects coding, rather than relying on the default treatment codes. 
# You can do that with the contr.sum() function:

# this overrides the default contrasts and tells it to use the contr.sum() function to make contrasts instead
contrasts(data$cond) <- contr.sum 
contrasts(data$dose) <- contr.sum

model3 <- lm(y ~ cond * dose, data=data)

Anova(model3, type="III")


# What if you have some actual comparisons you'd like to run? 
# For example, maybe you want to run Helmert contrasts on condition, and polynomial trend contrasts on dose? 
# As long as they're orthogonal, they'll work fine in an ANOVA:
        
contrasts(data$cond) <- contr.helmert 
contrasts(data$dose) <- contr.poly

model4 <- lm(y ~ cond * dose, data=data)

Anova(model4, type="III")

# Anova() won't show you the individual contrast results, just the overall effect of each factor. 
# You can see the results of each contrast by using the summary() function on the model object:
        
summary(model4)


# If you use aov() instead of lm() to specify the original model, 
# then you'll need to add a split argument to the summary() call to see the contrast results:


model5 <- aov(y ~ cond * dose, data=data)
summary(model5) # no contrast results displayed 

summary(model5, split=list( cond=list(AvB=1, CvAB=2, CtrlvElse=3),
                            dose=list(Linear=1, Quad=2, Cubic=3))) # there are my contrasts!


# You can see the contrasts you ran in the model object saved from lm(). 
# This is especially helpful if you're using automatic contrast functions, like contr.helmert(), 
# and you want to check that the function is really running the comparisons you think it is. 
# For example, you can see here that what R uses when you call contr.helmert() 
# is actually what would be called "reverse" Helmert contrasts in other software.

attributes(model4$qr$qr)$contrasts