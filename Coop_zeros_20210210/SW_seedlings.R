
##############################################
#-----------Analyzing Data with 0s ----------#
#----------Stats Bootcamp 02.10.21 ----------#
#-------------- Jonathan Coop ---------------#
##############################################

#Before we get started you may need to install a couple packages, this can take a bit.

install.packages("glmmTMB")
install.packages("MuMIn")
install.packages("ggplot2")

#----------------THE DATA--------------------#

# First, get the dataset SW.seedlings.csv
# I made a folder on my desktop called "stats_bootcamp"
# Set working directory accordingly -- you can modify script below

setwd("C:/Users/fac_coop/Desktop/stats_bootcamp")

# Import the csv

SW_seedlings <- read.csv("SW_seedlings.csv")

# I like to see the data in a window -- spend a moment familiarizing yourself with it

View(SW_seedlings)

# The seedling counts are columns 45-49.Does it look like a lot of zeros?
# How else can we find out? 

summary(SW_seedlings[,45:49])

# What do these summaries tell us?

colSums(SW_seedlings[,45:49] == 0)

# Now, lets visualize some of the count data.

hist(SW_seedlings$PIPO, breaks = 410)#I used 410 breaks because I wanted each bin to represent just one number, and the max here is 410. 

# Try making histograms for the other species

# Do you think the count data fit a Poisson distribution? (mean  = variance)

mean(SW_seedlings$PIPO)
var(SW_seedlings$PIPO)

#----------------MAKING THE MODEL--------------------#

# Recall, we are interested in the relationship between PIPO seedling counts and distance metrics d, DWD, D2WD #

# There are many ways we could approach this modeling endeavor, but here is how I decided to do it.
# First, I expect the relationship to be monotonic, so I want to use a generalized linear model (glm)
# Second, my data is grouped by burn.I want to be able to account for differences between groups (burns) imparted by time-since-fire and other factors.
# So, I will model the effect of the variable "Burn" as a random effect on the model intercept. This means I need to use a generalized linear mixed-effects model (glmm).
#There are loads of packages for glmms, but my current go-to is glmmTMB, here are some references.
# https://www.research-collection.ethz.ch/bitstream/handle/20.500.11850/242692/1/RJ-2017-066.pdf
# https://www.biorxiv.org/content/10.1101/132753v1.abstract
# https://pbil.univ-lyon1.fr/CRAN/web/packages/glmmTMB/vignettes/glmmTMB.pdf

library("glmmTMB")
library("MuMIn")

# Let's start with a simple model: do seedlings counts decrease with distance from trees? (Ignoring random effect for now...)
# Try both a negative binomial and poisson.

PIPO.nb.glm<-glmmTMB(PIPO ~ dist, data = SW_seedlings, family = nbinom2)

PIPO.p.glm<-glmmTMB(PIPO ~ dist, data = SW_seedlings, family = poisson)

# Is there a relationship?

summary(PIPO.nb.glm)
summary(PIPO.p.glm)

# Which model is better: negative binomial or poisson? Greater log-likelihood indicates a better model (or, smaller negative value)
logLik(PIPO.nb.glm)
logLik(PIPO.p.glm)

# Oh yeah, What is the estimated value of theta, the overdispersion parameter?

# Now lets compare a fixed effects model with one that includes the random effect term. 

PIPO.nb.glmm<-glmmTMB(PIPO ~ dist + (1|Burn), data = SW_seedlings, family = nbinom2)

summary(PIPO.nb.glmm)


# What is the log-liklihood of this model, how does it compare to that of the glm?

# How does "Burn" influence seedling count -- how do the burns differ?

ranef(PIPO.nb.glmm)

# Finally: would a zero-inflated model be stronger"

PIPO.zinb.glmm<-glmmTMB(PIPO ~ dist + (1|Burn), zi = ~ dist, data = SW_seedlings, family = nbinom2)

AIC(PIPO.nb.glmm)
AIC(PIPO.zinb.glmm)

# Alright, how does distance compare to our synthetic dwd and d2wd metrics?
# There are a lot of ways we can do this, but I am OK just running some models.*Notice I added a new argument to deal with NAs

PIPO.d.nb.glmm<-glmmTMB(PIPO ~ dist + (1|Burn), data = SW_seedlings, family = nbinom2)
PIPO.dwd.nb.glmm<-glmmTMB(PIPO ~ dwd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)
PIPO.d2wd.nb.glmm<-glmmTMB(PIPO ~ d2wd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)

AIC(PIPO.d.nb.glmm)
AIC(PIPO.dwd.nb.glmm)
AIC(PIPO.d2wd.nb.glmm)

# Which is best???

# Could a better model include more than one of these terms?

PIPO.d.dwd.nb.glmm<-glmmTMB(PIPO ~ dist + dwd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)
PIPO.d.d2wd.nb.glmm<-glmmTMB(PIPO ~ dist + d2wd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)
PIPO.dwd.d2wd.nb.glmm<-glmmTMB(PIPO ~ dwd + d2wd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)
PIPO.d.dwd.d2wd.nb.glmm<-glmmTMB(PIPO ~ dist + dwd + d2wd + (1|Burn), na.action = na.omit, data = SW_seedlings, family = nbinom2)

AIC(PIPO.d.dwd.nb.glmm)
AIC(PIPO.d.d2wd.nb.glmm)
AIC(PIPO.dwd.d2wd.nb.glmm)
AIC(PIPO.d.dwd.d2wd.nb.glmm)

# Which model is strongest?

# Let's take good look at what the model tells us

summary(PIPO.d.d2wd.nb.glmm)

ranef(PIPO.d.d2wd.nb.glmm)

# If you are used to thinking about R^2 as a measure of linear model fit, you feel like something is missing here. 
# We can calculate a pseudo-r^2 with MuMIn: r.squaredGLMM. Marginal is fixed effects only, Conditional includes fixed and random effects.

r.squaredGLMM(PIPO.d.d2wd.nb.glmm)

# What did this tell us?

# Now, try modeling post-fire regeneration for a different species 
# Or adding another factor or two to the PIPO model that might influence post-fire seedling counts.

#----------------A SIMPLE GRAPHIC--------------------#

# Here is one way to illustrate the expected relationship between seedlings and D2WD

# First, exponentiate the intercept and slope based on the model above

fun.1 <- function(x) exp(0.177 + 0.163607*x)

# Then, plot it out

library(ggplot2)
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + stat_function(fun = fun.1, color = "blue", size = 2) + 
  xlim(0,20) + 
  labs (x = "D2WD", y = "Modeled PIPO Seedling Count")
  



