#correspondence analysis
#clear list
rm(list=ls(all=TRUE))

##installing packages

#install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("gplots")
library("FactoMineR")
library("factoextra")

#READING DATA
s=read.csv("C:/Users/Desktop/healthcare/additional analy/QBG12_V_14.csv")
s1=read.csv("C:/Users/Desktop/healthcare/additional analy/BG11_V_BG14.csv")

rownames(s)=c("Safety","Cleaning Performance","Reliability")
rownames(s1)=colnames(s)

str(s)
typeof(s)
head(s)
dt <- as.table(as.matrix(s))

str(s1)
typeof(s1)
head(s1)
dt1 <- as.table(as.matrix(s1))

head(dt)
head(dt1)

#ballon plot
balloonplot(t(dt), main ="Preferred features V Preferred Brands", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

balloonplot(t(dt1), main ="Preferred equipments V Preferred Brands", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

#chisq <- chisq.test(s)
#chisq

#correspondence analysis                  
library(ca)
r <- ca(dt)
plot(r)

r1 <- ca(dt1)
plot(r1)



