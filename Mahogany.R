install.packages("daewr")
library(ggplot2)
library(dplyr)
library(ggpubr)
library(agricolae)
library(daewr)

#field sheet data generator for crd
set.seed(6546)
x=as.factor(rep(c("A","B","C","D"), each=6))
mahogany=sample(x,24)
y=1:24
growth=data.frame(plot=y, specie=mahogany)
write.csv(growth, file = "growth.csv",row.names = FALSE)
head(growth)

#field sheet data generator for crd
set.seed(6546)
trt=c("A","B","C")
replications=c(4,5,4)
Rdesign=design.crd(trt,r=replications,seed=6546,serie=0)
Rdesign
book=Rdesign$book
head(book)

#creating RCBD sheet
treat=c(1,2,3,4) #treatments
rcbd1=design.rcbd(treat, 4, serie=2, seed=6546)
rcbd1=rcbd1$book
levels(rcbd1$block)=c("carnation", "daisy", "rose", "tulip")
head(rcbd1)
help("design.rcbd")

#or alternaye way for rcbd sheet generation
trt=c(1,2,3,4)
repetition=4
outdesign=design.rcbd(trt,4,serie=2,seed=6546,kinds='Super-Duper',first=TRUE,continue=TRUE,randomization=TRUE)
book=zigzag(outdesign)
print(outdesign$book)
write.csv(outdesign$book, file = "rcbd.csv",row.names = FALSE)

#EXPLORING DATA
forest=read.csv("forest.csv")
forest
species=factor(forest$Species) #convert categorical data species to factor
class(species)
summary(forest$Growth)

#using groupby function to summarise species data
forest%>%group_by(Species)%>%summarise(Growth=mean(Growth))
library(dplyr)
boxplot(Growth~Species,data = forest, xlab = "species", ylab = "growth")   

help("rnorm")
