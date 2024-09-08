#********************projet de régression(2022/2023)******************
library(data.table)
library(Hmisc)
path2data<-file.path("C:","Users","Cloud","Desktop","analyse de régression")
setwd("C:/Users/Cloud/Desktop/analyse de régression")
DM.dt<-fread(file.path(path2data,"dataset.csv"))
Dt<-DM.dt[,.(X1,X2,X3,Y)]
M=as.matrix(Dt)
#affichage du min,Q1,mediane,moyenne,Q3,max
summary(M)
#calcul de l'écart type de X1 ,X2,X3
for (j in 1:3){
  print(paste0("l'ecart type de X",j))
  print(sd(M[,j]))
}

#calcul de variance de X1 ,X2,X3
for (j in 1:3){
  print(paste0("variance de X",j))
  print(var(M[,j]))
}
#calcul de coefficient de variation de X1 ,X2,X3
for (j in 1:3){
  print(paste0("coefficient de variance X",j))
  print(sd(M[,j])/mean(M[,j]))
}
#calcul de l'écart type,variance et coefficient de variation de Y
print(paste0("écart type de Y :",sd(M[,4])))
print(paste0("variance de Y :",var(M[,4])))
print(paste0("coefficient de variation de Y :",sd(M[,4])/mean(M[,4])))

#boite à moustache
boxplot(M, col=rainbow(4))
#boite à moustache de chaque variable
par(mfrow=c(2,2))
boxplot((M[,1]), col ="green")
title("Prix")
boxplot((M[,2]), col ="red")
title("Prix moyen des concurrents")
boxplot((M[,3]), col ="blue")
title("Dépenses des publicités")
boxplot((M[,4]), col ="purple")
title("Ventes")



### scatterplot Matrix*/
pairs(~Y+X1+X2+X3+X4+X5+X6+X7+X8, data=DM.dt,
      main=" Scatterplot Matrix")
###matrice de corrélation
DM_Matrix<-as.matrix(DM.dt)
rcorr(DM_Matrix, type=c("pearson","spearman"))


par(mfrow=c(3,3))
###régression linéaire simple de Y avec X1
par(mfrow=c(1,1))
plot(DM.dt$X1,DM.dt$Y)
lm<-lm(formula = Y~X1,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X1"]],3),
            "*X1",sep = "") )
##équation de la droite de regression de Y avec X1
print(lm)
##summary de regession de Y avec X1
print(summary(lm))


##régression linéaire simple de Y avec X2
plot(DM.dt$X2,DM.dt$Y)
lm<-lm(formula = Y~X2,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X2"]],3),
            "*X2",sep = "") )
##l'équation de la droite de regression de Y avec X2
print(lm)
##summary de regession de Y avec X2
print(summary(lm))

##régression linéaire simple de Y avec X3
plot(DM.dt$X3,DM.dt$Y)
lm<-lm(formula = Y~X3,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X3"]],3),
            "*X3",sep = "") )
##l'équation de la droite de regression de Y avec X3
print(lm)
##summary de la regession de Y avec X3
print(summary(lm))

##la régression linéaire simple de Y avec X4
plot(DM.dt$X4,DM.dt$Y)
lm<-lm(formula = Y~X4,DM.dt)##droite de régression
abline(lm,col="red")
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X4"]],3),
            "*X4",sep = "") )
##l'équation de la droite de regession de Y avec X4
print(lm)
##summary de la regession de Y avec X4
print(summary(lm))

##la régression linéaire simple de Y avec X5
plot(DM.dt$X5,DM.dt$Y)
lm<-lm(formula = Y~X5,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X5"]],3),
            "*X5",sep = "") )
##l'équation de la droite de regression de Y avec X5
print(lm)
##summary de la regession de Y avec X5
print(summary(lm))

##la régression linéaire simple de Y avec X6
plot(DM.dt$X6,DM.dt$Y)
lm<-lm(formula = Y~X6,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X6"]],3),
            "*X6",sep = "") )
##l'équation de la droite de regression de Y avec X6
print(lm)
##summary de la regession de Y avec X6
print(summary(lm))

##la régression linéaire simple de Y avec X7
plot(DM.dt$X7,DM.dt$Y)
lm<-lm(formula = Y~X7,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X7"]],3),
            "*X7",sep = "") )
##l'équation de la droite de regression de Y avec X7
print(lm)
##summary de l regession de Y avec X7
print(summary(lm))

##la régression linéaire simple de Y avec X8
plot(DM.dt$X8,DM.dt$Y)
lm<-lm(formula = Y~X8,DM.dt)
abline(lm,col="red")##droite de régression
title(paste(" Y = ",
            round(lm[["coefficients"]][["(Intercept)"]],3),"+",
            round(lm[["coefficients"]][["X8"]],3),
            "*X8",sep = "") )
##l'équation de la droite de regression de Y avec X8
print(lm)
##summary de la regession de Y avec X8
print(summary(lm))


## Regression multiple de Y f(X) avec X4 et X6
lm<-lm(formula =Y ~X4+X6, data=DM.dt)
#print 
print(lm) 
#summary de la regession de Y avec X4+X6
print(summary(lm)) 
#lm attributes  
print(attributes(lm))
#### Extraction des coefficients
coef(lm)





par(mfrow=c(1,1))

#Vérification des suppositions sur les erreurs εi 
####Test de Durbin-Watson:l'indépendance des résidus
library(lmtest)#Lmtest library for statistical tests
dwtest(formula =Y~ X4+X6,data=DM.dt)#Durbin-Watson Test

###Egalité des variance : Homoscédasticité
###Goldfeld-Quandt Test
gqtest(formula = Y ~ X4+X6,data=DM.dt,fraction = 0)
###représentation graphique d' homoscedasticité
plot(lm,3)

###Normalité des résidus :
###Q-Q plot 
plot(lm,2)

library(olsrr)# olsrr library for runing regression analyses

###Vérification de la robustesse de régresion

###Detection des observations influentes 
###Résidus studentisés externes :
ols_plot_resid_stand(lm)
standard_res <- as.data.table(rstandard(lm))
###Résidus studentisés internes :
ols_plot_resid_stud(lm)
stud_resids <- as.data.table(rstudent(lm))
###la distance de Cook
ols_plot_cooksd_bar(lm)
cooks_dist <- as.data.table(cooks.distance(lm))
###DFBETAS :
ols_plot_dfbetas(lm)
dfbetas <- as.data.table(dfbetas(lm))
###dffits
ols_plot_dffits(lm)
dffits <- as.data.table(dffits(lm))
###covratio
covr <- as.data.table(covratio(lm))
###tableau synthétèse
residual <- as.data.table(resid(lm))##tableau des résidus
table <- cbind(residual, standard_res, stud_resids, cooks_dist, dffits, covr)
colnames(table) <- c("residual", "standard_res", "stud_resids", "cooks_dist", "dffits", "covr") 
dfbetas <- as.data.table(dfbetas(lm))
table <- cbind(table, dfbetas)
print(table,digits=4)

par(mfrow=c(2,2))
###les boxplot de Résidus standarisés,Covratio,Dffits et distance de Cook 
boxplot(standard_res, col ="green"    )
title("Résidus standarisés")
##################""
boxplot(covr, col ="green")
title("Covratio")
##############""
boxplot(dffits, col ="green")
title("Dffits")
###############"
boxplot(cooks_dist, col ="green")
title("DCook")

###Non colinéarité des regresseurs
#vif
ols_vif_tol(lm)
#condition index
ols_eigen_cindex(lm)

par(mfrow=c(1,1))

#### Extraction des valeurs prédites
fitted(lm)
#### représentation des erreurs en fonction des valeurs estimées
plot(lm,1)
#***********************FIN***********************************
#*****réalisé par :Samia Karroumi et Aya Mellouli*************
#*****filière :ROAD*******************************************
#*****2022/2023***********************************************
#*************************************************************