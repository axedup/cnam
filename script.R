library(rgdal)
library(sp)
library(gstat)
points <- readOGR(dsn=path.expand("F:/CNAM/Sta112/projet/Donnees"),layer="Pt_Zn", stringsAsFactors=TRUE)
 str(points)
 plot(points)
 plot(points$Zn)
commune <- readOGR(dsn=path.expand("F:/CNAM/Sta112/projet/Donnees"),layer="Commune_Zn", stringsAsFactors=TRUE)
 str(commune)
 plot(commune)
 proj4string(commune)
 proj4string(points) 
 grille<-readOGR(dsn=path.expand("F:/CNAM/Sta112/projet/Donnees"),layer="Grille_projetCNAM", stringsAsFactors=TRUE)
head(grille) 
proj4string(grille) 
 grille <- spTransform( grille, CRS("+proj=lcc +lat_1=45.898918964419 +lat_2=47.696014502038 +lat_0=46.8 +lon_0=2.337229104484 +x_0=600000 +y_0=2200000 +ellps=clrk80 +units=m +no_defs")) 
 
 #Palette de couleurs utilisÃ©es pour la reprÃ©sentation sur fond cartographique
 colorPalette <-  rainbow(100, start = 4/6, end = 1);
 
 #Echelonnement des valeurs 
 pbMin <- min(points$Zn)
 pbMax <-  max(points$Zn)
 
 step = 5;
 pbCuts <-  seq(pbMin, pbMax + (pbMax - pbMin)%% step, by = step)
 
 #ReprÃ©sentation des donnÃ©es de plomb en fonction de leur positionnement gÃ©ographique

 spplot(points, cuts = pbCuts, col.regions = colorPalette);

 
 d<-density(points$Zn)
 plot(d)
 hist(points$Zn,density = TRUE,freq=FALSE)
 hist(log(points$Zn),density=TRUE,freq=FALSE)
 
 dl<-density(log(points$Zn))
 plot(dl) 
 
 ds<-density(sqrt(points$Zn)) ### c'est le mieux 
 plot(ds) 
 
 dsi<-density(1/sqrt(points$Zn))
 plot(dsi) 
 
 points.gstat = gstat(id="Zn", formula=Zn ~ 1, data=points)
 points.emp_variog = variogram( points.gstat)
 plot( points.emp_variog$dist,points.emp_variog$gamma)
 
 points$sqzn<-sqrt(points$Zn)
   
 var_cd = variogram(sqzn~1, points)

 par(mfrow=c(1,1))
 plot(var_cd$dist, var_cd$gamma)
 plot(var_cd)
 
var_cd.fit = fit.variogram( var_cd, model = vgm(15000, "Sph", range=40000, nugget=10000)) # range c'est le plateau

plot(var_cd,var_cd.fit)
 

zn.kriged = krige(sqrt(Zn)~1, points, grille, model = var_cd.fit)
 
zn.kriged["var1.pred"]
writeOGR(obj=zn.kriged, dsn="F:/CNAM/Sta112/sortie/euh", layer="kriged", driver="ESRI Shapefile")

#Cross validation:
points_red<-points[1:1000,]

var_cd_red = variogram(Zn~1, points_red)
plot(var_cd_red )
var_cd_red.fit = fit.variogram( var_cd_red, model = vgm(15000, "Sph", range=40000, nugget=10000)) # range c'est le plateau


points_cv_red<-data.frame(points$X[1:1000],points$Y[1:1000],points$Zn[1:1000])
cv_ok_red<- krige.cv(sqrt(points.Zn.1.1000.)~1,data=points_cv_red, locations=~ points.X.1.1000.+points.Y.1.1000., 
                 model=var_cd.fit,nfold=nrow(points_cv_red)) 




points_cv_red2<-data.frame(points$X[1:2000],points$Y[1:2000],points$Zn[1:2000])
cv_ok_red2<- krige.cv(sqrt(points.Zn.1.2000.)~1,data=points_cv_red2, locations=~ points.X.1.2000.+points.Y.1.2000., 
                     model=var_cd.fit,nfold=nrow(points_cv_red2)) 


### krigeage avec moyenne locale variable

 dl<-density(commune$Zn)
 plot(dl) 

 dl<-density(log(commune$Zn))
 plot(dl) 

comm = variogram(Zn~1, commune)

 par(mfrow=c(1,1))

 plot(comm)
 
comm.fit = fit.variogram(comm, model = vgm(var(commune$Zn), "Sph", range=40000, nugget=2.0e+10)) # range c'est le plateau

plot(comm,comm.fit)
comm.kriged = krige(Zn~1, commune, points, model = comm.fit) 
comm.gr.kriged = krige(Zn~1, commune, grille, model = comm.fit) 


points$res<-points$Zn- comm.kriged["var1.pred"]$var1.pred

par(mfrow=c(1,1))
 dl<-density(points$res)
 plot(dl) 


 dl<-density(1/points$res)
 plot(dl) 
mean(points$res)

res.var = variogram(res~1, points)
plot(res.var,main="Variogramme exp�rimental des r�sidus")

 
res.var.fit = fit.variogram(res.var, model $$= vgm(var(points$res), "Sph", range=35000, nugget=2.0e+10)) # range c'est le plateau

plot(res.var,res.var.fit)

res.kriged=krige(res~1, points, grille, model = res.var.fit,beta=14770) 
 
total<-res.kriged$var1.pred+ comm.gr.kriged$var1.pred


fin<-as.data.frame(cbind(res.kriged$x,res.kriged$y,total))
colnames(fin)<-c("x","y","total")
head(fin)
fin.grid<-fin

coordinates(fin.grid)= cbind(fin.grid$x,fin.grid$y)
proj4string(fin.grid) = CRS("+proj=lcc +lat_1=45.898918964419 +lat_2=47.696014502038 +lat_0=46.8
+lon_0=2.337229104484 +x_0=600000 +y_0=2200000 +ellps=clrk80 +units=m
+no_defs")
str(fin.grid)

writeOGR(obj=fin.grid, dsn="F:/CNAM/Sta112/sortie/euhbis", layer="krigedmoyp", driver="ESRI Shapefile")

