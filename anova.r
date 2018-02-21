library(yarrr)
yarrr::pirateplot(time ~ cleaner, 
                  data = poopdeck, 
                  theme = 2, 
                  cap.beans = TRUE,
                  main = "formula = time ~ cleaner")
head(poopdeck)

poopfile <- poopdeck

# Step 1: aov object with time as DV and cleaner as IV
cleaner.aov <- aov(formula = time ~ cleaner,
                   data = poopdeck)
# Step 2: Look at the summary of the anova object
summary(cleaner.aov)

TukeyHSD(cleaner.aov)


# Step 4: Create a regression object
cleaner.lm <- lm(formula = time ~ cleaner,
                 data = poopdeck)

# Show summary
summary(cleaner.lm)

# Step 1: Create ANOVA object with aov()
cleaner.type.aov <- aov(formula = time ~ cleaner + type,
                        data = poopdeck)

# Step 2: Get ANOVA table with summary()
summary(cleaner.type.aov)

# Step 3: Conduct post-hoc tests
TukeyHSD(cleaner.type.aov)


# Step 4: Look at regression coefficients
cleaner.type.lm <- lm(formula = time ~ cleaner + type,
                      data = poopdeck)

summary(cleaner.type.lm)


# Step 1: Create ANOVA object with interactions
cleaner.type.int.aov <- aov(formula = time ~ cleaner * type,
                            data = poopdeck)

# Step 2: Look at summary table
summary(cleaner.type.int.aov)



# Step 4: Calculate regression coefficients
cleaner.type.int.lm <- lm(formula = time ~ cleaner * type,
                          data = poopdeck)

summary(cleaner.type.int.lm)


# Step 1: Calculate regression object with lm()
time.lm <- lm(formula = time ~ type + cleaner,
              data = poopdeck)

# Type I ANOVA - aov()
time.I.aov <- aov(time.lm)

# Type II ANOVA - Anova(type = 2)
time.II.aov <- car::Anova(time.lm, type = 2)

# Type III ANOVA - Anova(type = 3)
time.III.aov <- car::Anova(time.lm, type = 3)


# Are observations in the poopdeck data balanced?
with(poopdeck,
     table(cleaner, type))


# Show me what's in my aov object
names(cleaner.type.int.aov)

# Add the fits for the interaction model to the dataframe as int.fit

poopdeck$int.fit <- cleaner.type.int.aov$fitted.values

# Add the fits for the main effects model to the dataframe as me.fit

poopdeck$me.fit <- cleaner.type.aov$fitted.values

# How far were the interaction model fits from the data on average?

mean(abs(poopdeck$int.fit - poopdeck$time))

# How far were the main effect model fits from the data on average?

mean(abs(poopdeck$me.fit - poopdeck$time))


head(pirates)

str(pirates)



##############################################################################
# Risultati esercizi
# 1
pixar.aov <- aov(formula = tattoos ~ fav.pixar, data = pirates)
summary(pixar.aov)

# Answer: No, there is no significant effect


# 2
favpirate.aov <- aov(formula = tattoos ~ favorite.pirate, data = pirates)
summary(favpirate.aov)

# Answer: No, there is no significant effect

# 3
pirpix.aov <- aov(formula = tattoos ~ favorite.pirate + fav.pixar, data = pirates)
summary(pirpix.aov)


# 4
pirpix.int.aov <- aov(formula = tattoos ~ favorite.pirate * fav.pixar, data = pirates)
summary(pirpix.int.aov)
