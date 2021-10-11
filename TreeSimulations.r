
library(phytools, ape)

print('doing Yule simulations...') 
pbs.4 <- pbtree(b=0.4, d=0, t=5, nsim=4)
pbs.9 <- pbtree(b=0.9, d=0, t=5, nsim=4)

#first look at the four trees with simulated with the birth rate at 0.4.
par(mfrow=c(2,2))
for (t in pbs.4){
    plot(t)
    }

##Now let's look at trees simulated with a birth rate of 0.9
par(mfrow=c(2,2))
for (t in pbs.9){
    plot(t)
    }

print('doing birth-death simulations...') 
bds.4 <- pbtree(b=0.4, d=0.2, t=5, nsim=4)

par(mfrow=c(2,2))
for (t in bds.4){
    plot(t)
    }
