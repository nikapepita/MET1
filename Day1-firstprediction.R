####Task 1
hw <- read.csv("weight-height.csv")

head(hw)
summary(hw)

ggplot2::ggplot(hw, aes(x=Height,y=Weight,fill=Gender))+
  geom_point()


install.packages("measurements")
library(measurements)

hw2 <- data.frame(Gender=hw$Gender,
                  Weight=conv_unit(hw$Weight, "lbs","kg"),
                  Height=conv_unit(hw$Height, "inch","cm"))

head(hw2)
plot(hw2$Height, hw2$Weight)

#first data exploration
library(dplyr)

#provide 10 random samples
dplyr::sample_n(hw2,10)

#how are the vaules for male and female
summary(filter(hw2,Gender=="Female"))
summary(filter(hw2,Gender=="Male"))

#any obvious anomalies or indications of significant correlation between male and female values?
boxplot(filter(hw2,Gender=="Female")$Weight, filter(hw2,Gender=="Male")$Weight, notch=T)
boxplot(filter(hw2,Gender=="Female")$Height, filter(hw2,Gender=="Male")$Height, notch=T)


#spahiro.test - only 5000 samples are possible
shapiro.test(dplyr::sample_n(hw2,5000)$Height)
shapiro.test(dplyr::sample_n(hw2,5000)$Weight)   

#result - significant different from normal distribution -> no linearregreesion possible

plot(density(hw2$Weight))
plot(density(hw2$Height))

#plot on top of each other
plot(density(filter(hw2,Gender=="Female")$Weight), col="red")
lines(density(filter(hw2,Gender=="Male")$Weight),col="blue")

shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Female"),5000)$Height)
shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Male"),5000)$Height)

shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Female"),5000)$Weight)
shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Male"),5000)$Weight)

#spilt up - female 
hw2.female <- filter(hw2,Gender=="Female")
summary(hw2.female)

hw.lm <- lm(formula = Weight ~ Height, data=hw2.female)
summary(hw.lm)

hw.new <- data.frame(name=c("Annika", "Narges", "Kemeng", "Sofia", "Helena","Luisa"), 
                     Height=c(175,163,152,152,170,176))


#predict weight based on regression
hw.lm.p <- predict(object=hw.lm, newdata=hw.new)
pred.weight <- data.frame(hw.new$name, weight.pred=hw.lm.p)
pred.weight

plot(pred.weight)


#Task 2 - Eagle Distribution

library(rgdal)
library(raster)

occ <- readOGR("occurence.gpkg")

class(occ)
summary(occ)
plot(occ)


bui <- readOGR("campus_buildings.gpkg")

plot(bui)
#add true - add it to plot and style :-)
plot(occ[occ$students==1,],col="blue",pch=16,add=T)
plot(occ[occ$students==0,],col="red",pch=16,add=T)

##Rasterize it and calculate distance
#v1
r <- raster(bui, ncols=100,nrows=100)
rr.0 <- rasterize(bui,r,progress="text")
plot(rr.0)

rr.0.d <- distance(rr.0)
preds <- rr.0.d
plot(rr.0.d)

install.packages("sdm")
##easy to to - query package - sdm
library(sdm)
install.packages("e10171")

d <- sdmData(formula=students~layer, train=occ, predictors = preds)
d

m1 <- sdm(students~.,data=d,methods=c('glm','svm'))

p1 <- predict(m1, newdata=preds,filename='sdm_preds_1.grd',overwrite=T)

plot(p1)

#v2


rr <- rasterize(bui,r,progress="text", field="id")
plot(rr)

rr.1 <- rr==1
rr.1[rr.1==0] <- NA
plot(rr.1)

rr.2 <- rr==2
rr.2[rr.2==0] <- NA
plot(rr.2)

rr.3 <- rr==3
rr.3[rr.3==0] <- NA
plot(rr.3)

rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d,rr.2.d,rr.3.d)


plot(preds)

d2 <- sdmData(formula=students~layer.1+layer.2+layer.3, train=occ, predictors = preds)
d2

m2 <- sdm(students~.,data=d2,methods=c('glm','svm'))

p2 <- predict(m2, newdata=preds,filename='sdm_preds_1.grd',overwrite=T)

plot(p2)


plot(p2[[1]])
plot(bui,add=T)

##add time

occ.10 <- occ[occ$time==10,]
occ.13 <- occ[occ$time==13,]
occ.22 <- occ[occ$time==22,]

plot(occ.10)
plot(occ.13)
plot(occ.22)


d10 <- sdmData(formula=students~layer.1+layer.2+layer.3, train=occ.10, predictors = preds)
d13 <- sdmData(formula=students~layer.1+layer.2+layer.3, train=occ.13, predictors = preds)
d22 <- sdmData(formula=students~layer.1+layer.2+layer.3, train=occ.22, predictors = preds)

m10 <- sdm(students~.,data=d10,methods=c('glm','svm'))
m13 <- sdm(students~.,data=d13,methods=c('glm','svm'))
m22 <- sdm(students~.,data=d22,methods=c('glm','svm'))

p10 <- predict(m10, newdata=preds,filename='sdm_preds_10.grd',overwrite=T)
p13 <- predict(m13, newdata=preds,filename='sdm_preds_13.grd',overwrite=T)
p22  <- predict(m22, newdata=preds,filename='sdm_preds_22.grd',overwrite=T)

plot(p10[[1]])
plot(bui,add=T)

plot(p13[[1]])
plot(bui,add=T)

plot(p22[[1]])
plot(bui,add=T)

plot(p13)
plot(p22)

p.time <- stack(p10,p13,p22)

plotRGB(p.time, 1,3,5, stretch="lin")
plot(bui,add=T)


