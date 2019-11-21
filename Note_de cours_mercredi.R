

###################################################################################
############ Exemple d'algo de machine learning (Bandit) ##### 

mstar = c( 0.1, 0.3, 0.1, 0.35, 0.45  )

T = 400

nbArms = length(mstar)

pulls = rep( 0, nbArms )
wins  = rep( 0, nbArms )



#pdf( "ucb.pdf" )

for ( t in 1:T) {
  if ( t <= nbArms ) { 
    a = t 
  } else {
    mhat = wins/pulls
    bounds = sqrt( 1/2 * log(T)/ (pulls) )
    a = which.max(bounds+mhat)	
  }
  pulls[a] = pulls[a] + 1
  if (runif(1)<mstar[a]) wins[a] = wins[a] + 1 
  if ( t > nbArms ){ 
    plot( mhat , ylim=c(0,2), pch=3, xlab="Bandit",ylab="Expectation")
    points( mstar, col=2, pch=16 )
    arrows(1:nbArms, mhat+bounds, 1:nbArms, mhat-bounds , code=3, angle=90, length=0.1, col="black")
    for ( i in 1:nbArms) text( i, (mhat+bounds)[i]+0.1, pulls[i] )
  }
  
}
#dev.off()



###################################################################################


gonflable<-c(28,28,31,31,32,33,32.5,29,30.5,31,
             28.5,27.5,32,29.5,28,26,30,31,32.5,33,27.5,29,
             30,28.5,27,25,31.5,33,34.5,29)

mean(gonflable)
shapiro.test(gonflable)

#Maintenant nous pouvons calculer avec R l’intervalle de
#confiance cherché (nous rappelons que nous connaissons l'écart type sigma = sqrt(4) = 2 de la population.

mean(gonflable)-qnorm(0.975)*2/sqrt(30)

mean(gonflable)+qnorm(0.975)*2/sqrt(30)

#########################################
bille <- c(19.6,20,20.2,20.1,20 ,19.9 ,20 , 20.3,20.1,19.8)
mean(bille)
sd(bille)

mean(bille)-qt(0.975,9)*sd(bille)/sqrt(10)
mean(bille)+qt(0.975,9)*sd(bille)/sqrt(10)



###################################################################################

toxine <-c(1.2,0.8,0.6,1.1,1.2,0.9,1.5,0.9,1)
mean(toxine)
sd(toxine)
mean(toxine)-qt(0.975,8)*sd(toxine)/sqrt(9)
mean(toxine)+qt(0.975,8)*sd(toxine)/sqrt(9)

### Cas variance connue : le vrai paramètre sigma est égale à sd(toxine) (ici la variance corrigé)

mean(toxine)-(0.975)*sd(toxine)/sqrt(9)
mean(toxine)+qnorm(0.975)*sd(toxine)/sqrt(9)


################################  Jeudi     #############################################
#################### Transformation d'un liaison non linéaire en liaison linéaire 
x <- c(1:1000)
alpha =1 
beta = 2

y <- alpha * x^beta

plot(x,y)

########

x_2 = log(x)
y_2 = log(y)

plot(x_2,y_2)

########

x <- seq(1, 100, 0.1)
alpha = 10
y <- alpha * exp(beta * x)
plot(x,y)
y_3 = log(y)

plot(x,y_3)


########

