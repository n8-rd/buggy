
d<- read.csv(text='Herbivore,Herb-div,Sister,Sister-div
+ Tingidae,1800,Joppeicidae,1
+ Miridae,10000,Isometopidae,60
+ Trichophora,5000,Aradidae,1000
+ Elateridae,9000,Eucnemidae,1200
+ Scarabaeidae (in part),14000,Scarabaeinae + Aphondiinae,8200
+ Languriinae,410,Labarinae + Erotylidae,1750
+ Epilachninae,700,Coccinellini,250
+ Phytophaga,130000,other cucujoids,10000
+ symphyta,10000,Panorpida,80000
+ Oscinellinae + Chloropinae,2210,Siphonellopsinae,80
+ Tephritidae s.s.,4000,other tephritids,1733
+ Agromyzidae,2000,Clusiidae,200')


d ##not much of a command, but it will show us what we want to see.

wilcox.test(d$Herb.div, d$Sister.div, paired=TRUE, alternative='greater')

library(phytools)
library(diversitree)

tree1 <- pbtree(b=1, d=0, t=5)

plot(tree1)

fit.bd <-birthdeath(tree1)
fit.bd

pars <- c(0.1, 0.3, 0.03, 0.03, 0.05, 0.1)
set.seed(2)
phy <- tree.bisse(pars, max.t=40, x0=0)
states <- phy$tip.state
#head(states)
cols = c('gray50', 'green')
plot(history.from.sim.discrete(phy, 0:1), phy, col=cols)

lik <- make.bisse(phy, states) ##first we make an object that contains our tree and tip states
p <- starting.point.bisse(phy) ##given that tree, make a guess about what the parameter values might be
fit <- find.mle(lik, p) ##fit the model -- i.e., estimate parameters -- using maximum likelihood
round(coef(fit), 3) ##print the estimate model parameters, rounded to three digits

##it will take a few seconds for this to run

lik.con <- constrain(lik, lambda1 ~ lambda0, mu1 ~ mu0)
fit.con <- find.mle(lik.con, p[argnames(lik.con)])
round(rbind(full=coef(fit), equal.rates=coef(fit.con, TRUE)), 3)
anova(fit, equal.rates=fit.con)
