##Mangez des pommes

#cas équilibrée
V1 <- c(93.6,95.6,96.0,93.7,96.2)
V2 <- c(95.3,96.9,95.8,97.3,97.7)
V3 <- c(94.5,97.0,97.8,97.0,98.3)
V4 <- c(98.8,98.2,97.8,97.2,97.9)
V5 <- c(94.6,97.8,98.0,95.0,98.9)

I=5
J=5


#Calcule des moyenne conditionnelle
mean_V1 <- mean(V1)
mean_V2 <- mean(V2)
mean_V3 <- mean(V3)
mean_V4 <- mean(V4)
mean_V5 <- mean(V5)

#Calcule de la moyenne globale
mean_tot <- mean(c(V1,V2,V3,V4,V5))
mean_tot

#Calcule des variances conditionnelle
var_V1 <- (sum((V1-mean_V1)^2))/J
var_V1

var_V2 <- (sum((V2-mean_V2)^2))/J
var_V2

var_V3 <- (sum((V3-mean_V3)^2))/J
var_V3

var_V4 <- (sum((V4-mean_V4)^2))/J
var_V4

var_V5 <- (sum((V5-mean_V5)^2))/J
var_V5

#nb observation
n=I*J


#variance mean
var_mean <- 1/I*( (mean_V1-mean_tot)^2  + (mean_V2-mean_tot)^2 + (mean_V3-mean_tot)^2 + (mean_V4-mean_tot)^2 + 
                    (mean_V5-mean_tot)^2  )
var_mean
#autre calcule
mean_var <- 1/I*(var_V1 + var_V2 + var_V3 + var_V4 + var_V5 )
mean_var


var_obsevation = var_mean + mean_var
var_obsevation

#variation totale SC_tot = variation due au facteur (SC_f) + Variation résiduelle

SC_f = J*( (mean_V1-mean_tot)^2  + (mean_V2-mean_tot)^2 + (mean_V3-mean_tot)^2 + (mean_V4-mean_tot)^2 + 
             (mean_V5-mean_tot)^2   )
SC_f 
#autre méthode de calcule
J*( (mean_V1)^2+(mean_V2)^2+(mean_V3)^2+(mean_V4)^2 + (mean_V5)^2  ) -n*(mean_tot^2)

SC_r = sum((V1 -mean_V1)^2) + sum((V2 -mean_V2)^2) + sum((V3 -mean_V3)^2) + sum((V4 -mean_V4)^2)+ sum((V5 -mean_V5)^2)  
SC_r
#autre méthode de calcule
(J)*sum(var_V1+ var_V2+var_V3+var_V4+ var_V5)


SC_tot = SC_f + SC_r
SC_tot


#variance due au facteur variance intergroupe s_f_2
var_f = SC_f / (I-1)
var_f
#variance corrigé des moyennes des groupes
var_cor =  var_f/J 
var_cor

#variance résiduelle (variance intragroup) ou moyenne des vairances corrigées
var_r = SC_r / (n-I)
var_r


#Si H0 vrais F_obs est une réalisation aléatoire F qui suit une loi de Fisher à I-1 degré de liberté au numérateur et (n-I) au dénominateur. 
F_obs = var_f/var_r
F_obs
#autre méthode
(SC_f/(I-1))/(SC_r/(n-I))

#On regarde dans la table de fisher à I-1 degré de liberté au numérateur et I-1 et n-I degré de liberté au numérateur
I-1
n-I
#######################
#########Table de Fisher



right_tail_p <- 0.05

# Define the vectors storing the indices corresponding to numerator (n1) and denominator (n2, row)
# degrees of freedom for F(α,n1,n2)  Note that Inf corresponds to ∞


n1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 20, 24, 30, 40, 60, 120, Inf)
n2 <- c(1:30, 40, 60, 120, Inf)

# Define precision (4-decimal point accuracy)
options(digits=4)

# Generate an empty matrix of critical f-values
f_table <- matrix(ncol=length(n1), nrow=length(n2))

# Use the The F Distribution quantile function to fill in the matrix values in a nested 2-loop
# Recall that the density (df), distribution function (pf), quantile function (qf) and random generation (rf) for the F distribution

for (i in 1:length(n2)){
  for (j in 1:length(n1)){
    f_table[i,j] <- qf(right_tail_p, n1[j], n2[i], lower.tail = FALSE)
  }
} 
rownames(f_table) <- n2
colnames(f_table) <- n1

# Print results
f_table


####################
###on trouve 2.866 ###
F_obs
### c<=F_obs on accepte H1 avec un risque de 5% 
####Conclusion###
#les moyenne des variétés de pommes ne sont pas égales






