NBA = read.csv(file = "NBA.csv", sep = ";", header = TRUE)
head(NBA)

#vérification des données
NBA_verif=is.na(NBA)
if(sum(NBA_verif==FALSE)==5445)
{
  print("il n'y a pas de données manquantes")
}else{ 
  print("il manque des données")
}

head(NBA_verif)
#il n'y a pas de données manquantes


#tendances globales d'un joueur de NBA sur la saison
Moyenne_taille=mean(NBA$taille_cm)
print(Moyenne_taille)

Moyenne_poids=mean(NBA$poids_kg)
print(Moyenne_poids)

Moyenne_points=mean(NBA$moyenne_de_points)
print(Moyenne_points)

Moyenne_rebonds=mean(NBA$moyenne_de_rebonds)
print(Moyenne_rebonds)

Moyenne_assists=mean(NBA$moyenne_de_passes_d)
print(Moyenne_assists)

plot(NBA$taille_cm, NBA$poids_kg)
cor(NBA$taille_cm,NBA$poids_kg)
# on remarque une relation linéaire entre le poids et la taille

#intervalles de confiance
t.test(NBA$taille_cm, mu=198.3509)
#la p-valeur est 1
#notre valeur de taille moyenne calculée précedemment se situe bien dans l'IC à 95%
t.test(NBA$moyenne_de_points, mu=8.24)



plot(NBA$taille_cm, NBA$moyenne_de_points)
cor(NBA$taille_cm, NBA$moyenne_de_points)


plot(NBA$taille_cm, NBA$moyenne_de_rebonds)
cor(NBA$taille_cm, NBA$moyenne_de_rebonds)


plot(NBA$taille_cm, NBA$moyenne_de_passes_d)
cor(NBA$taille_cm, NBA$moyenne_de_passes_d)



lines(NBA$taille_cm, NBA$moyenne_de_rebonds)
plot(NBA$taille_cm)

shapiro.test(NBA$taille_cm)
shapiro.test(NBA$moyenne_de_points)
shapiro.test(NBA$moyenne_de_rebonds)
shapiro.test(NBA$moyenne_de_passes_d)
#très faible p-value donc les variables ,ne suivent pas une loi normale

barplot(height = table(NBA$taille_cm), horiz=TRUE, xlab = "nombre", ylab = "taille" )

sd(NBA$taille_cm) #écart type
#histogrammes
taille=NBA$taille_cm
points=NBA$moyenne_de_points
rebonds=NBA$moyenne_de_rebonds
assists=NBA$moyenne_de_passes_d


#histogrammes pour montrer la loi exponentielle sur les 3 variables de niveau

hist(taille, freq=FALSE, main="taille",ylab="")
x=taille
curve(dexp(x, 1/mean(NBA$taille_cm)),add=TRUE)

hist(points, freq=FALSE, main="points",ylab="")
x=points
curve(dexp(x, 1/mean(NBA$moyenne_de_points)),add=TRUE)

hist(rebonds, freq=FALSE, main="rebonds",ylab="")
x=rebonds
curve(dexp(x, 1/mean(NBA$moyenne_de_rebonds)),add=TRUE)

hist(assists, freq=FALSE, main="assists",ylab="")
x=assists
curve(dexp(x, 1/mean(NBA$moyenne_de_passes_d)),add=TRUE)

#méthode de la fonction de répartition

x=taille
plot(ecdf(x), main="Taille")
curve(pexp(x,1/mean(NBA$taille_cm)), add=TRUE)

x=points
plot(ecdf(x), main="Points")
curve(pexp(x,1/mean(NBA$moyenne_de_points)), add=TRUE)

x=rebonds
plot(ecdf(x), main="Rebonds")
curve(pexp(x,1/mean(NBA$moyenne_de_rebonds)), add=TRUE)

x=assists
plot(ecdf(x), main="Assists")
curve(pexp(x),1/mean(NBA$moyenne_de_passes_d), add=TRUE)


#Méthode du QQ plot
x=points
plot(qexp(ppoints(x),1/mean(NBA$moyenne_de_points)),sort(x),main="Points", xlab="")
abline(0,x,col="red")

x=rebonds
plot(qexp(ppoints(x),1/mean(NBA$moyenne_de_rebonds)),sort(x),main="Rebonds", xlab="")
abline(0,x,col="red")

x=assists
plot(qexp(ppoints(x),1/mean(NBA$moyenne_de_passes_d)),sort(x),main="Points", xlab="")
abline(0,x,col="red")




#création d'une nouvelle base de données
NBA_matchs_modif=(subset(NBA, NBA$matchs_joués>=30))

#tendances sur les bases de données modifiées pour minimum 30 matchs 
NBA_matchs_modif_points=(subset(NBA_matchs_modif, NBA_matchs_modif$moyenne_de_points>=25))
plot(NBA_matchs_modif_points$taille_cm, NBA_matchs_modif_points$moyenne_de_points)

NBA_matchs_modif_rebonds=(subset(NBA_matchs_modif, NBA_matchs_modif$moyenne_de_rebonds>=10))
plot(NBA_matchs_modif_rebonds$taille_cm, NBA_matchs_modif_rebonds$moyenne_de_rebonds)

NBA_matchs_modif_assists=(subset(NBA_matchs_modif, NBA_matchs_modif$moyenne_de_passes_d>=7))
plot(NBA_matchs_modif_assists$taille_cm, NBA_matchs_modif_assists$moyenne_de_passes_d)


#on part sur le nombre principal puis on réduit pour garder une dizaine de joueurs
#on remarque que la taille est vraiment importante pour les rebonds, mais moins pour les compétences techniques

#création de nouvelles bases de données avec toutes les 3 conditions
NBA_matchs_modif_1=(subset(NBA_matchs_modif_points, NBA_matchs_modif_points$moyenne_de_rebonds>5))
NBA_matchs_modif_2=(subset(NBA_matchs_modif_1, NBA_matchs_modif_1$moyenne_de_passes_d>3))
print(NBA_matchs_modif_2)

#graphes avec les 3 conditions
plot(NBA_matchs_modif_2$taille_cm,NBA_matchs_modif_2$moyenne_de_points)
#on remarque que les joueurs sont surtout au-dessus de 198cm
plot(NBA_matchs_modif_2$taille_cm,NBA_matchs_modif_2$moyenne_de_rebonds)
#même remarque
plot(NBA_matchs_modif_2$taille_cm,NBA_matchs_modif_2$moyenne_de_passes_d)
#même remarque

t.test(NBA$taille_cm, mu=210)