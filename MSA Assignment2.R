#MSA 2 Assignment 1
#install.packages("readxl")
library(readxl)
dat1<- read_excel(file.choose())
View(dat1)
dim(dat1)
head(dat1)

#According to the given task, chnaging -200 to missing value
#by using NA and removing 1, 2 and 5 coulmns

dat1_updated<-dat1[,-c(1,2,5)]
dat1_updated[dat1_updated==-200]<- NA
dat1_updated
View(dat1_updated)

# Data Analysis 
corr_complete=cor(dat1_updated,use = "complete.obs")
corr_complete
corr_pair=cor(dat1_updated,use="pair")
corr_pair

#Eigen values and Eigen vectors
# Eigen values for complete obs
E_complete=eigen(corr_complete)
E_complete$values
#Eigen Vectors for complete obs
E_complete$vectors 


E_pair=eigen(corr_pair)
# Eigen values for Pair
E_pair$values  
#Eigen Vectors for pair
E_pair$vectors 

#Taking Eigen values which are greater than 1 and their corresponding eigen vectors
#For complete values
ec=E_complete$values[E_complete$values>1]
ec
E_v_complete=E_complete$vectors[,c(1,2,3)]
E_v_complete

#For pairs
ep=E_pair$values[E_pair$values>1]
ep
E_v_pair=E_pair$vectors[,c(1,2,3)]
E_v_pair

#Proportion of variance
proportion_complete<-round(ec/12,2)
proportion_complete

proportion_pair<-round(ep/12,2)
proportion_pair

#Principle Components
pca_complete= as.matrix(dat1_updated)%*%E_v_complete
pca_complete
pca_pair=as.matrix(dat1_updated)%*%E_v_pair
pca_pair
