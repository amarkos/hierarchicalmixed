

########
#	VAR0: Employment to population ratio (Ποσοστό απασχόλησης προς πληθυσμό )
#	VAR1: Agriculture % of GDP (Γεωργία% του ΑΕΠ)
#	VAR2: Industry % of GDP (Βιομηχανία% του ΑΕΠ)
#	VAR3: Services % of GDP (Υπηρεσίες% του ΑΕΠ) 
#	VAR4: Corporate Tax Rate (%) (Φόρος Εταιρικής Φορολογίας (%) ) 
#	CR : Credit ratings( Πιστοληπτική αξιολόγηση) [2 levels] 1:   A       2:   B
load("labor.Rdata")
dat_nom = dummy(dd[,6])
dat_con1 = barycentric(dd[,1],2)
dat_con2 = barycentric(dd[,2],2)
dat_con3 = barycentric(dd[,3],2)
dat_con4 = barycentric(dd[,4],2)
dat_con5 = barycentric(dd[,5],2)
dat_con = cbind(dat_con1, dat_con2, dat_con3, dat_con4, dat_con5)
dat_new = data.frame(cbind(dat_nom, dat_con))
dat.rpro = dat_new / apply(dat_new,1,sum)
dat.r = apply(dat_new,1,sum) / sum(dat_new)
dat.rclust = hierclust(dat.rpro, dat.r)
#plot(as.dendrogram(dat.rclust))
res = cutree(dat.rclust, k = 4) #k = 1 is trivial

#######

load("diamond.Rdata")
dat_con1 = barycentric(diamond[,1],3)
dat_con2 = barycentric(diamond[,2],3)
dat_con3 = barycentric(diamond[,3],3)
dat_ord4 = barycentric(as.numeric(diamond[,4]),3,con=FALSE)
dat_ord5 = barycentric(as.numeric(diamond[,5]),3,con=FALSE)
dat_cont = cbind(dat_con1, dat_con2, dat_con3,dat_ord4,dat_ord5)
dat_cat = dummy.data.frame(data.frame(diamond[,c(6)]), dummy.class="ALL")
dat_new = cbind(dat_cat,dat_cont)

dat.rpro = dat_new / apply(dat_new,1,sum)
dat.r = apply(dat_new,1,sum) / sum(dat_new)
dat.rclust = hierclust(dat.rpro, dat.r)
plot(as.dendrogram(dat.rclust))

res = cutree(dat.rclust, k = 3) #k = 1 is trivial


#Cleveland Heart data set
load("heart.Rdata")
dat_con9 =  barycentric(heart.data[,9],3)
dat_con10 = barycentric(heart.data[,10],3)
dat_con11 = barycentric(heart.data[,11],3)
dat_con12 = barycentric(heart.data[,12],3)
dat_con13 = barycentric(heart.data[,13],3)

dat_cont = cbind(dat_con9, dat_con10, dat_con11, dat_con12, dat_con13)
dat_cat = dummy.data.frame(data.frame(heart.data[,c(1:8)]), dummy.class="ALL")
dat_new = cbind(dat_cont,dat_cat)

dat.rpro = dat_new / apply(dat_new,1,sum)
dat.r = apply(dat_new,1,sum) / sum(dat_new)
dat.rclust = hierclust(dat.rpro, dat.r)
#plot(as.dendrogram(dat.rclust))

res = cutree(dat.rclust, k = 2) #k = 1 is trivial
adjustedRandIndex(res,heart.data[,14])