#all the landscape level variables agains number of families and morphospecies by house

#package loading
library(vegan)
library(RColorBrewer)
library(lme4)
library(ggplot2)
library(ade4)

#read in file
setwd('~/Dropbox/AoH/dataMain')
aoh<-read.csv("houseData.csv", header=TRUE, row.names=1)

#mantel tests for spatial autocorrelations
house.dists<-dist(cbind(aoh$lon, aoh$lat))
fam.dists<-dist(aoh$numFam)
income.dists<-dist(aoh$incomeAvg)
gc.dists<-dist(aoh$groundCoverAvg)
gd.dists<-dist(aoh$groundDivAvg)
imp.dists<-dist(aoh$Imp500m)
mantel.rtest(house.dists, imp.dists, nrepet=999)
#for "rare" see code at bottom of script
aoh1<-data.frame(ms=aoh$totalMS, numFam=aoh$numFam, 
                 #numRARE=rare, 
                 Imp100m=aoh$Imp100m,Can100m=aoh$Can100m, Imp500m=aoh$Imp500m, Can500m=aoh$Can500m, 
                incomeAvg=aoh$incomeAvg, propRenter=aoh$propRenter,
                 houseAge=aoh$houseAge, totalValue=aoh$totalValue, canopyDivAvg=aoh$canopyDivAvg,
                 groundDivAvg=aoh$groundDivAvg,groundCoverAvg=aoh$groundCoverAvg, 
                 canopyCoverAvg=aoh$canopyCoverAvg, vegAvg=aoh$vegAvg, sqFeet=aoh$sqFeet
                # , gcCat=aoh$gcCat, gcCat2=aoh$gcCat2
                # , clutter=aoh$clutter, filth=aoh$filth, plant=aoh$classPlant3, dog=aoh$classDog, cat=aoh$classCat, pesticide=aoh$classPesticide
                 )

clean.aoh<-na.omit(aoh1)
propRARE<-clean.aoh$numRARE/clean.aoh$numFam



  #scaling variables from 0 to 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
Imp100m<-range01(clean.aoh$Imp100m)
Can100m<-range01(clean.aoh$Can100m)
Imp500m<-range01(clean.aoh$Imp500m)
Can500m<-range01(clean.aoh$Can500m)

propRenter <-range01(clean.aoh$propRenter)
income<-range01(clean.aoh$incomeAvg)
houseAge<-range01(clean.aoh$houseAge)
totalValue <-range01(clean.aoh$totalValue)
canopyDiv<-range01(clean.aoh$canopyDivAvg)
groundDiv<-range01(clean.aoh$groundDivAvg)
groundCover<-range01(clean.aoh$groundCoverAvg)
canopyCover<-range01(clean.aoh$canopyCoverAvg)
veg<-range01(clean.aoh$vegAvg)
clutter<-range01(clean.aoh$clutter)
filth<-range01(clean.aoh$filth)
plant<-range01(clean.aoh$plant)
dog<-range01(clean.aoh$dog)
cat<-range01(clean.aoh$cat)
pesticide<-range01(clean.aoh$pesticide)
propRAREscaled<-range01(propRARE)
sqFeet <-range01(clean.aoh$sqFeet)
aoh.scaled<-data.frame(groundDiv, canopyDiv, groundCover, canopyCover,
                       houseAge, totalValue, sqFeet, income,
                       Imp100m, Imp500m, Can100m, Can500m)
                       
aoh.scaled<-data.frame(numFam=clean.aoh$numFam, #numRARE=clean.aoh$numRARE, 
                       #propRARE, propRAREscaled, 
                       Imp100m, Can100m, Imp500m, Can500m, 
                    income, 
                 houseAge, totalValue, canopyDiv, groundDiv,
                  groundCover, canopyCover, veg, sqFeet
                  #,gcCat=clean.aoh$gcCat, 
                  #gcCat2=clean.aoh$gcCat2
                  )
#write.csv(aoh1, file="publicData.csv")

aoh.scaled<-data.frame(numFam=clean.aoh$numFam, 
                       #propRARE, 
                       Imp500m, 
                       income,
                       groundDiv,
                       groundCover,
                       clutter, filth, plant, dog, cat, pesticide, sqFeet)

# correlation matrix (delete gcCat from above)
library(corrplot)
P <- cor(aoh.scaled, method="pearson")
K <- cor(aoh.scaled, method="kendall")
S <- cor(aoh.scaled, method="spearman")
corrplot(P, method = "number")
corrplot(S, method = "number")
corrplot(K, method = "number")

## plotting this shit
cloud(numFam~income+groundDiv, data=aoh.scaled)
plot(numFam~Imp500m, data=aoh.scaled)

## The super awesome glmulti
library(glmulti)
mALL<-glmulti(numFam~Can100m+Imp500m+houseAge+income+groundDiv+groundCover
            +canopyCover+sqFeet, 
           data=aoh.scaled, family="poisson", method="h", level=1, crit="aic")
mRARE<-glmulti(numRARE~Can100m+Imp500m+houseAge+income+groundDiv+groundCover+canopyCover+sqFeet, 
           data=aoh.scaled, family="poisson", method="h", level=1, crit="aic")
m<-glmulti(propRARE~Can100m+Imp500m+houseAge+income+groundDiv+groundCover+canopyCover+sqFeet, 
           data=aoh.scaled, family="binomial", method="h", level=1, crit="aic")
m<-glmulti((propRAREscaled*100)~Can100m+Imp500m+houseAge+income+groundDiv+groundCover+canopyCover+sqFeet, 
           data=aoh.scaled, family="poisson", method="h", level=1, crit="aic")
mInterior<-glmulti(numFam~income+Imp500m+groundDiv+groundCover+clutter+plant+cat+dog+pesticide+sqFeet, 
              data=aoh.scaled, family="poisson", method="h", level=1, crit="aic")
summary(mALL)
summary(mRARE)
summary(mInterior)
plot(mInterior, type="s")
plot(mInterior, type="w")
plot(m, type="s")

coef(mInterior, select="all", varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)
 
m<-glm(numFam~income+groundDiv+groundCover+income*groundCover+
         sqFeet, 
        data=aoh.scaled, family="poisson")
m1<-glm(numFam~income+Imp500m+groundDiv+groundCover+cat+dog+sqFeet, 
         data=aoh.scaled, family="poisson")
summary(m)
summary(m1)

m2<-glm(numFam~Imp500m+income+groundCover+groundDiv+income*groundCover+sqFeet, 
        data=aoh.scaled, family="poisson")
summary(m2)## Groundcover and income have a strong interaction!  Let's plot that.
m3<-glm(numFam~Imp500m+income+groundCover+groundDiv+cat+dog+income*groundCover+sqFeet, 
        data=aoh.scaled, family="poisson")
summary(m3)
m3<-glm(numFam~Imp500m+income+groundCover+groundDiv+cat+dog+sqFeet, 
        data=aoh.scaled, family="poisson")
summary(m3)

# PLOTTING
aoh.scaled$gcCat<-factor(aoh.scaled$gcCat, levels=c("Low", "Med", "High"))
c<-ggplot(aoh.scaled, aes(y=numFam, x=income, colour=gcCat))
c+ stat_smooth(method=lm, se=FALSE)+geom_point()+scale_x_continuous(labels=scales::dollar)+
  labs(y="Number of arthropod families", x="Average neighborhood income", colour="Ground cover")+
  theme_bw(base_size=30)
  

##*******adonis
library(vegan)
#equivalent of Dune (with morphospecies richness for 50%+ subset per family)
subset<-read.csv("adonisSubset.csv", header=TRUE)

#recalculated to be for those families found in >10% of room.  There are 28!
subsetRoom<-read.csv("familiesByRoomMatrixSubset2.csv", header=TRUE)
subsetHouse<-subsetRoom %>%  separate(event, c("event2", "houseNum", "roomCat", "roomRep")) %>%
  unite("event", event2, houseNum, sep=".") %>%
  group_by(event) %>%
  summarize_each(funs(max))
subset<-subsetHouse[-c(1:3)]
write.csv(subset, "publicData_adonis.csv")
#equipvalent of dune.env
env<-read.csv("adonisEnv.csv", header=TRUE, row.names=1)

#subset data to get rid of NAs
subset <- subset[-c(8, 20, 28), ]
env <- env[-c(8, 20, 28), ]


adonis(subset ~ groundDivAvg, data=env, permutations=999)
adonis(subset ~ canopyCoverAvg, data=env, permutations=999)
adonis(subset ~ groundCoverAvg, data=env, permutations=999)
adonis(subset ~ sizeClass, data=env, permutations=999)
adonis(subset ~ income, data=env, permutations=999)
adonis(subset ~ Imp500m, data=env, permutations=999)
adonis(subset ~ Can100m, data=env, permutations=999)

formula<-adonis(subset ~ groundDivAvg+groundCoverAvg+income+groundCoverAvg*income+size, data=env, permutations=999)
all<-adonis(subset ~ groundDivAvg+groundCoverAvg+income+Imp500m+Can100m+size, data=env, permutations=9999)
all<-adonis(subset ~ groundDivAvg+sqFeet+Can100m+houseAge+groundCoverAvg+Imp500m+income, data=env, permutations=9999)
all$aov.tab
all$coefficients
all$f.perms
all$coef.sites
all$model.matrix
all$terms

# cut into quantiles to plot
library(gtools)
fiveClass <- quantcut(env, )
table(fiveClass)
#(these quantile breakings have bee adjusted into adonisEnv in excel

adonis(subset ~ incomeClass, data=env, method="chao", permutations=999)

#pcoa
dis <- vegdist(subset)
# Calculate multivariate dispersions
mod <- betadisper(dis, env$incomeClass)
# Plot PCoA ordination with differentiated groups = visualize the
plot(mod)

# Getting the number of RARE families at each house
setwd('~/Dropbox/AoH/dataMain')
aoh<-read.csv("familyClassification.csv", header=TRUE)
aoh[is.na(aoh)]<-0
library(magrittr)
library(tidyr)
library(dplyr)
aohRARE <- aoh %>% filter (rare==1) %>% 
  select (3:52) %>%
  summarise_each (funs(sum))
aohRAREt<-t(aohRARE)
rare<-as.vector(aohRAREt)
