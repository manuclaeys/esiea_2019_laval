
###Load the data
d <-read.csv("/home/manue/Documents/manue/Cours/esiea_laval_2019/git/MyData.csv", sep = ",")

head(d)

library(ggplot2)

summary(d$mine)

summary(d$imdb)

plot(density(d$mine))
plot(density(d$imdb))



#Notez les diff?rences de densit? sur les notes 
#donn?es entre le visiteur et la bdd


#moyenne
mean(d$mine)
mean(d$imdb)

#?cart-type
sd(d$mine)
sd(d$imdb)

#variance
var(d$mine)
var(d$imdb)

#la moyenne est de 6,64 pour les notes du visiteur
#la moyenne est de 7.51 pour les notes de imdb
#en observant l'?cart type et la variance on constate que la 
#dispertion autour de la moyenne est plus r?duite avec les 
#utilisateurs de imdb. 
#ce s'explique par une convergence vers la loi uniforme 
#lorsqu'on observe plusieurs ?chantillons (rappelez vous la vid?o
#que je vous avais montr?e en cour)
#par contre cela ne fonctionne pas avec notre visiteur qui
#a un profil bien ? lui 
hist(d$mine)

#( Les notes 6 et 7 sont les notes les plus fr?quentes)
#et augmente l'?cart type et par cons?quence la variance

hist(d$imdb)

#La diff?rence entre la courbe de densit? et l'histogramme 
#se justifie par l'interpr?tation des notes dans un ensemble
#discret ou continue. 





###Corr?lation
#visuellement, la corr?lation se traduit par une repr?sentation
#lin?aire (ax + b = y) entre deux variables x et y. 
#Si le nuage de points se concentre autour d'une droite, 
#il y a fort ? parier qu'il existe une relation de colin?arit? 
#lin?aire entre ces deux valeurs.


pairs(d[,c(17,18,12,14)])

#La fonction cor() vous permet d'avoir le coefficent 
#de lin?arit?
help(cor)
cor(d$mine, d$imdb)
cor(d$mine, d$Year)
cor(d$mine, d$Num..Votes)
cor(d$imdb, d$Num..Votes)
cor(d$imdb, d$Year)
cor(d$imdb, d$Num..Votes)

cor(d$Year, d$Num..Votes)


#plus ce coef est, en valeur absolue, fort, plus la relation
#lin?aire entre les deux variables est importante
#Le test de Pearson vous permet de tester statistiquement
#la corr?lation, sous conditions d'une t-distribution
# cf pour plus d'info : http://www.sthda.com/french/wiki/test-de-correlation-entre-deux-variables


### Exemple en log
x <- log(d$Year)
y <- d$Num..Votes

plot(x,y)

#Linear model 1
#On entraine le mod?le sur les donn?es de la database  IMDb afin de pr?dire les notes de l'utilisateur
summary(m1<-lm(mine~imdb, data=d))

#L'ordonn?e (Intercept) indique que, en moyenne, ses notes sont plus d'un demi-point inf?rieur (-0.6). 
#Le coefficient positif de la note IMDb est positive et tr?s proche de l'axe (0.9) 
#ce qui implique que 
# IMDb note , en moyenne, d'un point sup?rieur de celui de l'axe de pr?vision
# La note personnelle d'un point demi point inf?rieur de celui de l'axe de pr?vision. 
# La Figure 2 trace la relation entre les deux variables 

#Figure 2
p2 <- ggplot(d, aes(imdb, mine))+
  geom_point(position=position_jitter(width=0.1,height=.25),shape=16, size=4,alpha=0.6,
             aes(colour =  new.genre, ))+
  stat_smooth(se = TRUE)+
  scale_x_continuous('IMDb ratings')+
  scale_y_continuous('Perso ratings')+
  theme_bw()+
  scale_colour_discrete(name="new.genre")+
  scale_size_continuous(guide=FALSE)+
  theme(legend.position=c(0.15, 0.80))+
  geom_abline(size=1, aes(intercept=-0.6387, slope=0.9686))

p2

summary(d$new.genre)

#La ligne noire est la forme de r?gression, 
#le bleu montre un lissage de loess non-param?trique qui sugg?re une certaine 
#non-lin?arit? dans la relation que nous allons explorer plus tard.

#La r?gression locale, ou LOESS, est une m?thode de r?gression non param?trique 
#fortement connexe  
#qui combine plusieurs mod?les de r?gression multiple au sein 
#d'un m?ta-mod?le qui repose sur la m?thode des k plus proches voisins. 
#? LOESS ? est un acronyme qui peut ?tre compris comme signifiant, en anglais, ? LOcal regrESSion ?.

#La r?gression locale est une alternative possible aux m?thodes habituelles de r?gression, 
#comme la r?gression par les moindres carr?s lin?aire ou non lin?aire, 
#dans les cas o? ces derni?res s'av?rent mal adapt?es. Elle combine la simplicit? de r?gression 
#lin?aire par les moindres carr?s avec la flexibilit? de la r?gression non lin?aire, 
#en effectuant une r?gression simple sur des sous-ensembles locaux de donn?es. 
#L'un des principaux avantages de cette m?thode est qu'elle rend inutile la d?finition d'une unique 
#fonction globale qui d?crirait le mod?le de r?gression, puisque la m?thode consiste ? 
#calculer autant de fonctions locales qu'il y a de segments de donn?es.


help("aes")

#Notre utilisateur ? surtout not? de la com?die, des films serieux
#et des films d'action.Certaines cat?gories ont un faible ?cart entre les 
#notes, en revanche si on prend la cat?gorie light, 
#pour certains films, notre utilisateur donne des notes plus basses que
#la moyenne des utilisateurs d'IMDb. 
#Notre mod?le est perturb? par des cat?gories que notre utilisateur
#sous-?value ou au contraire sur-?value


#R?gression lin?aire simple
help(lm)

summary(m1<-lm(mine~imdb, data=d))
plot(m1)

summary(m5<-lm(mine~imdb + Darren.Aronofsky.. + year.c + Lars.Von.Trier.. +
   comedy.. + romance.. + new.genre + votes , data=d))
plot(m5)

#Le mod?le sur la base des notes donn?es par notre utilisateur 
#et la moyenne obtenue par IMDb sur les m?mes films.



#On affiche la racine d'erreur quadratique moyenne
#L'erreur quadratique moyenne est tr?s utile pour comparer plusieurs estimateurs, 
#notamment lorsque l'un d'eux est biais?. Si les deux estimateurs ? comparer sont sans biais, 
#l'estimateur le plus efficace est simplement celui qui a la variance la plus petite. 
#On peut effectivement exprimer l'erreur quadratique moyenne en fonction du biais de l'estimateur

sqrt(mean(residuals(m1)^2)) 
#root mean squared error: 1.25
# c'est le m?me principe que pour un calcul de variance, 
#si on prend juste les diff?rences entre valeurs mesur?es et valeurs attendues, 
#on a des ?carts positifs et n?gatifs qui se compensent. 
#Pour avoir que des ?carts positifs, on peut soit prendre les valeurs absolues des ?carts, 
#soit les carr?s. 
#En prenant les carr?s, on retombe sur des formules plus simples ? mettre en oeuvre 
#(reste plus qu'? prendre la racine carr?e pour retomber sur la bonne unit?... ). 
#C'est la notion de moyenne quadratique par opposition ? celle de moyenne arithm?tique 
#(ce qui donne un ?cart-"type" plut?t qu'un ?cart moyen (nul) ou un ?cart-absolu moyen). 


sqrt(mean(residuals(m1)^2))


#Shalizi function pour une intervale de confiance
predlims <- function(preds,sigma) {
  prediction.sd <- sqrt(preds$se.fit^2+sigma^2)
  upper <- preds$fit+2*prediction.sd
  lower <- preds$fit-2*prediction.sd
  lims <- cbind(lower=lower,upper=upper)
  return(lims)
}

preds.lm <- predict(m1,se.fit=TRUE)
predlims.lm <- predlims(preds.lm,sigma=summary(m1)$sigma)
mean(d$mine <= predlims.lm[,"upper"]
     & d$mine >= predlims.lm[,"lower"]) 




plot(d$mine,preds.lm$fit,type="n", xlim=c(2,10), ylim=c(2,10),
     xlab="My actual ratings",ylab="Predicted ratings", main="")
segments(d$mine,predlims.lm[,"lower"],
         d$mine,predlims.lm[,"upper"], col="grey")
abline(a=0,b=1,lty="dashed")
points(d$mine,preds.lm$fit,pch=16,cex=0.8)





### Bien que le coefficient de partition IMDb est statistiquement tr?s significative (error 0.0884)
###Le mod?le d'ajustement est plut?t pauvre. La racine d'erreur quadratique moyenne est de 1,25 
###ce qui est important compte tenu de la variation des donn?es. 
###Mais le mauvais ajustement est le plus clairement visible si nous tra?ons les donn?es r?elles
###par rapport aux pr?visions.


#Il est possible d'observer la diff?rence avec les donn?es de IMDb pour les film not? par l'utilisateur. 
#et les notes de l'utilisateur en observant les donn?es initiales. 

#Figure 4
d1<-subset(d, d$imdb>6.49 & d$imdb<7.5)
d2<-subset(d, d$imdb>7.51 & d$imdb<8.5)

p4<-ggplot (NULL, aes(mine))+
  geom_density(data = d1, fill='blue', alpha=0.4,aes(x=mine, y = ..density..))+
  geom_density(data = d2, fill='red', alpha=0.4,aes(x=mine, y = ..density..))+
  scale_x_continuous('Notes de l utilisateur compar?es ? la base d IMDb', breaks=seq(2,10,1))+
  scale_y_continuous('Density')+
  theme_bw()+theme(legend.position="none")
p4

#Comparez les diff?rents pics de densit? pour les deux courbes? 
#La Figure 4 montre la densit? entre les notes de imdb - 6,5 ? 7,5 (bleu) et celle de l'utilisateur  7.5- 8.5 (rouge). 
#Les moyenne pour les deux s?ries diff?rent quelque peu, mais le chevauchement de la densit? est grande.
#En somme, la connaissance de la note IMDb fournit des informations, mais sur son propre 
#r?sultat mais pas sur le score de l'utilisateur. 


#Nous allons ajouter d'autres variables pour voir comment am?liorer le mod?le. 
#On voit souvent lorsqu'un film sort, que le fait d'ajouter un r?alisateur connus 
#donnent un effet de levier.
#De la m?me fa?on, certain types de film sont plus facilement populaires que d'autre. 

d_lars <- d[d$Lars.Von.Trier.. ==1 ,]
#Linear model 2
summary(m2<-lm(mine~imdb+d$comedy +d$romance+d$mystery+d$Stanley.Kubrick..+d$Lars.Von.Trier..+d$Darren.Aronofsky..+year.c, data=d))
sqrt(mean(residuals(m2)^2)) #root mean squared error: 1.14

preds.lm <- predict(m2,se.fit=TRUE)
predlims.lm <- predlims(preds.lm,sigma=summary(m2)$sigma)
mean(d$mine <= predlims.lm[,"upper"]
     & d$mine >= predlims.lm[,"lower"]) 

#Les variables avec des *** sont pertinentes pour notre mod?lisation


plot(d$mine,preds.lm$fit,type="n", xlim=c(2,10), ylim=c(2,10),
     xlab="My actual ratings",ylab="Predicted ratings", main="")
segments(d$mine,predlims.lm[,"lower"],
         d$mine,predlims.lm[,"upper"], col="grey")
abline(a=0,b=1,lty="dashed")
points(d$mine,preds.lm$fit,pch=16,cex=0.8)

#L'ajustement s'am?liore quelque peu. 
#La racine de l'erreur quadratique moyenne de ce mod?le est de 1,14 (contre 1.254 avant). 
#De plus, ? la recherche de nouveau au r?el par rapport ?valuations pr?vues, 
#l'ajustement est mieux, surtout pour les films bien not?s - pas surprenant ?tant donn? 
#que les bon r?alisateurs sont populaire.


#On s?l?ctionne les film ? partir de 1960
#La derni?re variable dans la r?gression ci-dessous est alors l'ann?e de sortie du film. 

d.60<-subset(d, Year>1960)
d.60$r<-residuals(lm(d.60$mine~d.60$imdb))


#Affichez un summary de la fonction lm sur le df d.60$r selon (~) sa variable Year
summary(lm(d.60$r~d.60$Year))


#On observe la r?gression lin?aire selon les diff?rentes ann?es
#Figure 6. 
p6 <- ggplot(d.60, aes(Year, r))+
  geom_point(position=position_jitter(width=0.1,height=.25),shape=16, size=4,alpha=0.6,
             aes(colour = new.genre, ))+
  stat_smooth()+
  scale_x_continuous('Year of release')+
  scale_y_continuous('My ratings (residuals)')+
  theme_bw()+
  scale_colour_discrete(name="Genre")+
  scale_size_continuous(guide=FALSE)+
  theme(legend.position=c(0.15, 0.15))+
  geom_abline(size=1, aes(intercept=33.33, slope=-0.016659))


p6

####Faire un modele
m2<-lm(mine~imdb+comedy.. + romance..+ mystery..+ Stanley.Kubrick..+Lars.Von.Trier..+Darren.Aronofsky..+year.c, data=d)
summary(m2)
###Pour prédire une valeur
#World War Z
a <- d[d$Title =="World War Z",]
a$Title
a$mine
a$imdb
predict(m1, a)
predict(m5, a)
predict(m2, a)

