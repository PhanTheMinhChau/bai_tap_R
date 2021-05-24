setwd("C:/Users/ADMIN/Desktop/R")

BFDeaths <- read.table("BirdFluDeaths.txt", sep = "\t", header = TRUE)
names(BFDeaths)
str(BFDeaths)

Deaths <- rowSums(BFDeaths[,2:16])
names(Deaths) <- BFDeaths[,1]
Deaths


Deaths_col <- colSums((BFDeaths[,2:16]))
names(Deaths_col) <- names(BFDeaths[,2:16])
Deaths_col


par(mfrow=c(2,2), mar= c(3,3,2,1))
pie(Deaths, main = "Total deaths per years")
pie3D(Deaths, main = "3D Pie Chart",
      explode=0.1,
      labels = names(Cases),
      labelcex=0.6,)


barplot(Deaths_col,
        main="Total death per country",
        xlim = c(0, 20),
        ylim = c(0, 120),
        col = "blue")
box()


Vegetation <- read.table("Vegetation2.txt", sep = "\t", header = TRUE)
names(Vegetation)
str(Vegetation)


Richness <- rowSums(Vegetation[,10:13])
names(Richness) <- Vegetation[,1]
Richness

Vegetation_new <- data.frame(Richness, Vegetation$Transect)
colnames(Vegetation_new)[2] <- "Transect"



Veg.M <-tapply(Vegetation_new$Richness, 
               INDEX=Vegetation_new$Transect, 
               FUN=mean)
Veg.M


Veg.sd <-tapply(Vegetation_new$Richness, 
                INDEX=Vegetation_new$Transect, 
                FUN=sd)
Veg.sd

MSD <- cbind(Veg.M, Veg.sd)
MSD
barplot(Veg.M)


bp <- barplot(Veg.M, xlab="Transect",
              ylim = c(0,550),
              ylab="Richness",
              col=rainbow(8)
)
box()


Veg.le <- tapply(Vegetation_new$Richness,
                 INDEX=Vegetation_new$Transect,
                 FUN=length)
Veg.le


Veg.se <- Veg.sd/sqrt(Veg.le)
Veg.se
# STRIP CHART


stripchart(Vegetation_new$Richness~Vegetation_new$Transect,
           vert=TRUE,
           pch=1,
           method="jitter",
           jit=0.05,
           xlab="Transect",
           ylab="Richness")

points(1:8, Veg.M, pch=16,
       cex=1.5)
arrows(1:8, Veg.M,
       1:8, Veg.M+Veg.se,
       lwd=1.5,
       angle=90,
       length = 0.1)
arrows(1:8, Veg.M,
       1:8, Veg.M-Veg.se,
       lwd=1.5,
       angle=90,
       length = 0.1)
