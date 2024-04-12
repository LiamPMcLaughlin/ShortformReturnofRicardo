#########################################################
#####Liam McLaughlin - Dissertation data - Short run ####
#########################################################
####################Contents#############################
# 1~Packages required                                   #
# 4~Output Tables                                       #
# 5~Post-estimation Tests                               #
# 6~Confidence intervals                                #
#########################################################

#To run this code download data from placeholder link and put it in a folder called dissertation data
#then use a control f function to replace "C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/" with your desktop file path code up untill but not includeding the desktop part of the file path
#the same must be done for "C:\\Users\\Liam McLaughlin\\OneDrive - University of Edinburgh\\Desktop\\"
#the code should run as long as the files in the placeholder link are set up as specified in the readme document
#this has been verified to run from a usb transfer on Microsfot operating system with most uptodate 12/04 R update
#for technical assistance or for a custom code to be made for a specific computer contact liampmclaughlin@gmail.com

#########################################################
###############1 ~ Packages Required ####################
#########################################################

if(!require("plm")) install.packages("plm") 
library("plm")
if(!require("stargazer")) install.packages("stargazer") 
library("stargazer")
if(!require("MASS")) install.packages("MASS") 
library("MASS")
if(!require("dplyr")) install.packages("dplyr") 
library("dplyr")
if(!require("forcats")) install.packages("dforcats") 
library("forcats")
if(!require("sandwich")) install.packages("sandwich") 
library("sandwich")
if(!require("clubSandwich")) install.packages("clubSandwich") 
library("clubSandwich")
if(!require("lmtest")) install.packages("lmtest") 
library("lmtest")
if(!require("describedata")) install.packages("describedata") 
library("describedata")
if(!require("tidyr")) install.packages("tidyr") 
library("tidyr")
if(!require("Hmisc")) install.packages("Hmisc") 
library("Hmisc")
if(!require("tidyverse"))install.packages("tidyverse")
library("tidyverse")
if(!require("tseries")) install.packages("tseries") 
library("tseries")
if(!require("dgof")) install.packages("dgof") 
library("dgof")
if(!require("car")) install.packages("car") 
library("car")
if(!require("ggplot2")) install.packages("ggplot2") 
library("ggplot2")
if(!require("ggfortify")) install.packages("ggfortify") 
library("ggfortify")
if(!require("countrycode")) install.packages("countrycode") 
library("countrycode")
if(!require("plotrix")) install.packages("plotrix") 
library("plotrix")


###########################################################
###################4 ~ Output Tables ######################
###########################################################

#############4 ~ Output Tables Contense####################
#4a ~ Output task: Figure 1 - C&D Specification           #
#4b ~ Output task: Figure 2 - New Specification           #
#4c ~ Output task: Figure 3 - Comparative Specifiation    #
#4d ~ Output task: Figure 4 - Non-parametric test         #
#4e ~ Output task: Appendix Figure 5 - non-specialisation #
###########################################################

#####################################################
#4a ~ Output task: Figure 1 - C&D Specification     #

savedspecialisationdata2000<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/savedspecialisationdata2000.csv")
savedspecialisationdata2000 <- savedspecialisationdata2000[-c(7,37,72,76,119),]

m1<-lm(lnValue~lnPlow, savedspecialisationdata2000)
m1s<-vcovCL(m1, cluster=~CRP,type="HC1")
m1e    <- sqrt(diag(m1s))
m2<-plm(lnValue~ lnPlow, savedspecialisationdata2000, index= c("CRP"), model="within")
m2s<-vcovCR(m2, cluster= savedspecialisationdata2000$CRP,type="CR1")
m2e    <- sqrt(diag(m2s))
savedspecialisationdata2000<-drop_na(savedspecialisationdata2000)
m5<-plm(lnValue~ lnPlow, savedspecialisationdata2000, index= c("continent"), model="within")
m5s<-vcovCR(m5, cluster= savedspecialisationdata2000$CRP,type="CR1")
m5e    <- sqrt(diag(m5s))

savedspecialisationdata2010<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/savedspecialisationdata2010.csv") 
savedspecialisationdata2010 <- savedspecialisationdata2010[-c(7,37,72),]

m3<-lm(lnValue~lnPlow, savedspecialisationdata2010)
m3s<-vcovCL(m3, cluster=~CRP,type="HC1")
m3e    <- sqrt(diag(m3s))
m4<-plm(lnValue~ lnPlow, savedspecialisationdata2010, index= c("CRP"), model="within")
m4s<-vcovCR(m4, cluster=savedspecialisationdata2010$CRP,type="CR1")
m4e    <- sqrt(diag(m4s))
savedspecialisationdata2010<-drop_na(savedspecialisationdata2010)
m6<-plm(lnValue~ lnPlow, savedspecialisationdata2010, index= c("continent"), model="within")
m6s<-vcovCR(m6, cluster=savedspecialisationdata2010$CRP,type="CR1")
m6e    <- sqrt(diag(m6s))

stargazer(m1,m2,m5,m3,m4,m6,type="text",omit.stat=c("ser","f"),model.names=FALSE,column.sep.width="3000pt",se = list(m1e,m2e,m3e,m4e),dep.var.labels=c("ln(Actual Output)"),covariate.labels = c("ln(Predicted Output)"),omit="Constant",out="C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Visual Outputs/Figure 1 - C&D Specification.html", t.auto = FALSE, p.auto = FALSE, 
          add.lines=list(c("Year", "2000", "2000","2000","2010", "2010","2010"),
                         c("Fixed Effects by?", "None", "Crop","Continent","None", "Crop","Continent"),
                         c("Jacque-Bera Residual Normaility?", "Pass","-","Pass", "Pass", "-","-"),
                         c("Shapiro-Wilk Residual Normaility?", "Pass","-","Pass","Pass", "Pass","-"),
                         c("Kolmogorov–Smirnov Residual Normaility?","Pass","Pass","Pass","Pass", "Pass", "Pass")))

#used for postestimation
#jarque.bera.test(resid(m6)) 
#shapiro.test(resid(m6))  
#ks.test(resid(m6), "pnorm", mean = mean(resid(m6)), sd=sd(resid(m6)))
#autoplot(m1, which = 1:6, ncol = 3, label.size = 3)
#autoplot(m3, which = 1:6, ncol = 3, label.size = 3)
#avPlots(m1)
#avPlots(m3)
#potential removals
#savedspecialisationdata2000 <- savedspecialisationdata2000[-c(7,37,72), ]
#savedspecialisationdata2000 <- savedspecialisationdata2000[-c(37,76,100,119), ]
#savedspecialisationdata2010 <- savedspecialisationdata2010[-c(7,37,72), ]
#savedspecialisationdata2010 <- savedspecialisationdata2010[-c(37,72,60), ]

#####################################################
#4b ~ Output task: Figure 2 - New Specification     #

savernewspecF<- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/savernewspecF.csv")

m40<-lm(lnValue~lnPlow, savernewspecF)
m40s<-vcovCL(m40, cluster=~CRP,type="HC1")
m40e    <- sqrt(diag(m40s))
m41<-plm(lnValue~ lnPlow, savernewspecF, index= c("CRP"), model="within")
m41s<-vcovCR(m41, cluster= savernewspecF$CRP,type="CR1")
m41e    <- sqrt(diag(m41s))
savernewspecF<-drop_na(savernewspecF)
m44<-plm(lnValue~ lnPlow, savernewspecF, index= c("continent"), model="within")
m44s<-vcovCR(m44, cluster=savernewspecF$CRP,type="CR1")
m44e    <- sqrt(diag(m44s))

savernewspecT<- read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/savernewspecT.csv")
savernewspecT <- savernewspecT[-c(2,56,91,115, 96,72,106), ]

m42<-lm(lnValue~lnPlow, savernewspecT)
m42s<-vcovCL(m42, cluster=~CRP,type="HC1")
m42e    <- sqrt(diag(m42s))
m43<-plm(lnValue~ lnPlow, savernewspecT, index= c("CRP"), model="within")
m43s<-vcovCR(m43, cluster= savernewspecT$CRP,type="CR1")
m43e    <- sqrt(diag(m43s))
savernewspecT<-drop_na(savernewspecT)
m45<-plm(lnValue~ lnPlow, savernewspecT, index= c("continent"), model="within")
m45s<-vcovCR(m45, cluster=savernewspecT$CRP,type="CR1")
m45e    <- sqrt(diag(m45s))

stargazer(m40,m41,m44, type="text",omit.stat=c("ser","f"), column.sep.width="3000pt",se=list(m40e,m41e,m44e), model.names=FALSE,dep.var.labels=c("ln(Actual Output)"),covariate.labels = c("ln(Predicted Output)"), omit="Constant",out="C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Visual Outputs/Figure 2 - New Specification.html", t.auto = FALSE, p.auto = FALSE,
          add.lines=list(c("Year","2000","2000","2000"),
                         c("Size specification","Small","Small","Small"),
                         c("Fixed Effects by?", "None", "Crop","Continent"),
                         c("Jacque-Bera Residual Normaility?", "Pass", "-","Pass"),
                         c("Shapiro-Wilk Residual Normaility?", "Pass", "-","Pass"),
                         c("Kolmogorov–Smirnov Residual Normaility?", "Pass", "Pass","Pass")))

stargazer(m40,m41,m44,m42,m43,m45, type="text",omit.stat=c("ser","f"), column.sep.width="3000pt",se=list(m40e,m41e,m44e,m42e,m43e,m45e), model.names=FALSE,dep.var.labels=c("ln(Actual Output)"),covariate.labels = c("ln(Predicted Output)"), omit="Constant",out="C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Visual Outputs/Figure 4 - New Specification appendix.html", t.auto = FALSE, p.auto = FALSE,
          add.lines=list(c("Year","2000","2000","2000","2000","2000","2000"),
                         c("Size specification","Small","Small","Small","Large","Large","Large"),
                         c("Fixed Effects by?", "None", "Crop","Continent","None","Crop", "Continent"),
                         c("Jacque-Bera Residual Normaility?", "Pass", "-","Pass","-", "-","-"),
                         c("Shapiro-Wilk Residual Normaility?", "Pass", "-","Pass","-", "-","-"),
                         c("Kolmogorov–Smirnov Residual Normaility?", "Pass", "Pass","Pass","Pass", "-","Pass")))

#used for postestimations
#jarque.bera.test(resid(m45)) 
#shapiro.test(resid(m45))  
#ks.test(resid(m45), "pnorm", mean = mean(resid(m45)), sd=sd(resid(m45)))

#autoplot(m40, which = 1:6, ncol = 3, label.size = 3)
#autoplot(m42, which = 1:6, ncol = 3, label.size = 3)
#avPlots(m40)
#avPlots(m42)
#potential removals
#savernewspecF <- savernewspecF[-c(5,28,40,42), ]
#savernewspecF <- savernewspecF[-c(2,26,42,50), ]
#savernewspecT <- savernewspecT[-c(2,56,91,115), ]
#savernewspecT <- savernewspecT[-c(56,91,115), ]
#savernewspecT <- savernewspecT[-c(2,56,91,115,96,72,106), ]

#######################################################
#4c~ Output task: Figure 3 - Comparative Specifiation #

savedspecialisation1989<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/savedspecialisation1989.csv") 

m13<-lm(lnactual~lnpredicted, savedspecialisation1989)
m13s<-vcovCL(m13, cluster=~CRP,type="HC1")
m13e    <- sqrt(diag(m13s))
m14<-plm(lnactual~ lnpredicted, savedspecialisation1989, index= c("CRP"), model="within")
m14s<-vcovCR(m14, cluster= savedspecialisation1989$CRP,type="CR1")
m14e    <- sqrt(diag(m14s))
savedspecialisation1989<-drop_na(savedspecialisation1989)
m15<-plm(lnactual~ lnpredicted, savedspecialisation1989, index= c("continent"), model="within")
m15s<-vcovCR(m15, cluster=savedspecialisation1989$CRP,type="CR1")
m15e    <- sqrt(diag(m15s))

data<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/data.csv")

model1a<-lm(lnactual~lnpredicted, data)
cov1<-vcovCL(model1a, cluster=~Country,type="HC1")
cluster_se    <- sqrt(diag(cov1))
femodel1a<-plm(lnactual~ lnpredicted, data, index= c("Crop"), model="within")
cov2<-vcovCR(femodel1a,cluster=data$Country,type="CR1")
cluster2_se<-sqrt(diag(cov2))
femodel2a<-plm(lnactual~ lnpredicted, data, index= c("Country"), model="within")
cov3<-vcovCR(femodel2a,cluster=data$Country,type="CR1")
cluster3_se<-sqrt(diag(cov3))

stargazer(m13,m14,m15,model1a,femodel1a,femodel2a,type="text",model.names=FALSE,column.sep.width="90000pt",omit.stat=c("ser","f"),se = list(m13e,m14e,m15e,cluster_se,cluster2_se,cluster3_se),dep.var.labels=c("ln(Actual Output)"),covariate.labels = c("ln(Predicted Output)"),omit="Constant",out="C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Visual Outputs/Figure 3 - Comparative Specifiation.html", t.auto = FALSE, p.auto = FALSE,
          add.lines=list(c("Year","1981","1981","1981","1981","1981","1981"),
                         c("Specialisation by?","Country","Country","Country","Field","Field","Field"),
                         c("Data Source","GAEZV4","GAEZV4","GAEZV4","C&D(2012)","C&D(2012)","C&D(2012)"),
                         c("Fixed Effects by?", "None", "Crop","Continent","None","Crop", "Country"),
                         c("Jacque-Bera Residual Normaility?", "Pass", "-","Pass","Pass", "Pass", "Pass"),
                         c("Shapiro-Wilk Residual Normaility?", "Pass", "-","Pass","Pass", "Pass", "Pass"),
                         c("Kolmogorov–Smirnov Residual Normaility?", "Pass", "Pass","Pass","Pass", "Pass","Pass")))

#Postestimation testing
#ks.test(resid(femodel1a), "pnorm", mean = mean(resid(femodel1a)), sd=sd(resid(femodel1a)))
#jarque.bera.test(resid(m15)) #yes no yes yes yes
#shapiro.test(resid(m15))  #yes no yes yes yes
#ks.test(resid(femodel1a), "pnorm", mean = mean(resid(femodel1a)), sd=sd(resid(femodel1a))) # yes yes yes yes yes
#autoplot(m13, which = 1:6, ncol = 3, label.size = 3)
#avPlots(m13) # highest residuals and highest partial leverage
#potential removals
#savedspecialisation1989<-savedspecialisation1989[-c(17,54,77)]
#savedspecialisation1989<-savedspecialisation1989[-c(17,54,88)]

#####################################################
#4d ~ Output task: Figure 4 - Non-parametric test   #

nonpara2000<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/nonpara2000.csv")

by(nonpara2000, nonpara2000$CRP, function(nonpara2000) cor.test(nonpara2000$Value, nonpara2000$P_VS...mS, use="pair", method="kendall"))
by(nonpara2000, nonpara2000$CRP, function(nonpara2000) cor.test(nonpara2000$Value, nonpara2000$P_VS...mS, use="pair", method="spearman"))
length(unique(nonpara2000$Country))

results<-c()
for (j in unique(nonpara2000$CRP)){
  holder<-nonpara2000[nonpara2000$CRP==j,]
  holder<-holder|>drop_na()
  holder$Porder<- rank(holder$P_VS...mS)
  holder$Vorder<- rank(holder$Value)
  leadedge<-c()
  propbind<-c()
  for (i in 1:length(holder$Porder)) {
    indiv<-(holder$Porder[i]<= holder$Porder & holder$Vorder[i]<=holder$Vorder | holder$Porder[i]>= holder$Porder &  holder$Vorder[i]>=holder$Vorder)
    position<-c(1:length(holder$Porder))
    position<-position[position>i]
    indiv<-indiv[position]
    leadedge<-append(leadedge,indiv)
  }
  proportioncorrect<-sum(leadedge)/length(leadedge)
  name<-c(j)
  number<-c(length(holder$Porder))
  propbind<-data.frame(name,proportioncorrect,number)
  results<-rbind(results,propbind)
}
weighted<-results$proportioncorrect*results$number
name<-c("weighted avg proportion")
proportioncorrect<-sum(weighted)/sum(results$number)
number<-sum(results$number)
weightedbind<-data.frame(name,proportioncorrect,number)
results<-rbind(results,weightedbind)
results
sort.df <- with(results,  results[order(name) , ])
sort.df$proportioncorrect
sort.df$number

nonpara2010<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/nonpara2010.csv")

by(nonpara2010, nonpara2010$CRP, function(nonpara2010) cor.test(nonpara2010$Value, nonpara2010$P_VS...mS, use="pair", method="kendall"))
by(nonpara2010, nonpara2010$CRP, function(nonpara2010) cor.test(nonpara2010$Value, nonpara2010$P_VS...mS, use="pair", method="spearman"))

results<-c()
for (j in unique(nonpara2010$CRP)){
  holder<-nonpara2010[nonpara2010$CRP==j,]
  holder<-holder|>drop_na()
  holder$Porder<- rank(holder$P_VS...mS)
  holder$Vorder<- rank(holder$Value)
  leadedge<-c()
  propbind<-c()
  for (i in 1:length(holder$Porder)) {
    indiv<-(holder$Porder[i]<= holder$Porder & holder$Vorder[i]<=holder$Vorder | holder$Porder[i]>= holder$Porder &  holder$Vorder[i]>=holder$Vorder)
    position<-c(1:length(holder$Porder))
    position<-position[position>i]
    indiv<-indiv[position]
    leadedge<-append(leadedge,indiv)
  }
  proportioncorrect<-sum(leadedge)/length(leadedge)
  name<-c(j)
  number<-c(length(holder$Porder))
  propbind<-data.frame(name,proportioncorrect,number)
  results<-rbind(results,propbind)
}
weighted<-results$proportioncorrect*results$number
name<-c("weighted avg proportion")
proportioncorrect<-sum(weighted)/sum(results$number)
number<-sum(results$number)
weightedbind<-data.frame(name,proportioncorrect,number)
results<-rbind(results,weightedbind)
results
sort.df <- with(results,  results[order(name) , ])
sort.df$proportioncorrect
sort.df$number
sort.df

#########################################################
#4e~ Output task: Appendix Figure 5 - non specification #

data2000<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/data2000.csv")
m1a<-lm(lnValue~lnPlow, data2000)
m1ae<-vcovCL(m1a, cluster=~Country,type="HC1")
m3a<-plm(lnValue~ lnPlow, data2000, index= c("Country"), model="within")
m3ae<-vcovCR(m3a,cluster=data2000$Country,type="CR1")
m3aer    <- sqrt(diag(m3ae))
m4a<-plm(lnValue~ lnPlow, data2000, index= c("CRP"), model="within")
m4ae<-vcovCR(m4a,cluster=data2000$Country,type="CR1")
m4aer    <- sqrt(diag(m4ae))
m1aer    <- sqrt(diag(m1ae))

data2010<-read.csv("C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Finaldata/data2010.csv") 
m2a<-lm(lnValue~lnPlow, data2010)
m2ae<-vcovCL(m2a, cluster=~Country,type="HC1")
m2aer    <- sqrt(diag(m2ae))
m5a<-plm(lnValue~ lnPlow, data2010, index= c("Country"), model="within")
m5ae<-vcovCR(m5a,cluster=data2010$Country,type="CR1")
m5aer    <- sqrt(diag(m5ae))
m6a<-plm(lnValue~ lnPlow, data2010, index= c("CRP"), model="within")
m6ae<-vcovCR(m6a,cluster=data2010$Country,type="CR1")
m6aer    <- sqrt(diag(m6ae))

stargazer(m1a,m3a,m4a,m2a,m5a,m6a,type="text",model.names=FALSE,column.sep.width="90000pt",omit.stat=c("ser","f"),se = list(m1aer,m3aer,m4aer,m2aer,m5aer,m6aer),dep.var.labels=c("ln(Actual Output)"),covariate.labels = c("ln(Predicted Output)"),omit="Constant", out="C:/Users/Liam McLaughlin/OneDrive - University of Edinburgh/Desktop/Dissertation data/Visual Outputs/Appendix Figure 5.html", t.auto = FALSE, p.auto = FALSE,
          add.lines=list(c("Year","2000","2000","2000","2010","2010","2010"),
                         c("Fixed Effects by?", "None", "Country","Crop","None","Country", "Crop"),
                         c("Jacque-Bera Residual Normaility?", "-", "-","-","-", "-", "-"),
                         c("Shapiro-Wilk Residual Normaility?", "-", "-","-","-", "-", "-"),
                         c("Kolmogorov–Smirnov Residual Normaility?", "-", "-","-","-", "-","-")))
#potestimation testing
#jarque.bera.test(resid(m1a))
#shapiro.test(resid(m1a))
#ks.test(resid(m6a), "pnorm", mean = mean(resid(m6a)), sd=sd(resid(m6a)))
#autoplot(m1a, which = 1:6, ncol = 3, label.size = 3)
#autoplot(m2a, which = 1:6, ncol = 3, label.size = 3)


#############5 ~ postestimation methods####################
#run for different models remove the "#" infront and change model name
#chage the names of models and the errors as appropriate to get all statistics used

#jarque.bera.test(resid(m1a))
#shapiro.test(resid(m1a))
#ks.test(resid(m6a), "pnorm", mean = mean(resid(m6a)), sd=sd(resid(m6a)))
#autoplot(m1a, which = 1:6, ncol = 3, label.size = 3)
#avPlots(m3)

#############6 ~ confidence interval polts####################
#first part shows how the HAC standard errors were calculated using two different methods
#lm and plm have diffrent access to se and betas so this is the code to access
#change model names and standard errors as appropriate

#changed each time the model name and error name for each one
#confidence interval calculations for lm models: replace m1 and m1e as appropriate
beta<-m1$coefficients
beta<-beta[2]
beta<-as.numeric(beta)
HACerror<-m1e
HACerror<-HACerror[2]
HACerror<-as.numeric(HACerror)

Lowconf<-beta-(1.96*HACerror)
Highconf<-beta+(1.96*HACerror)
confidenceinterval<-c(Lowconf, Highconf)
confidenceinterval

#confidence interval calculations for plm models: replace m2 and m2e as appropriate
beta<-m2$coefficients
HACerror<-m2e
##############
beta<-beta[1]
beta<-as.numeric(beta)
HACerror<-HACerror[1]
HACerror<-as.numeric(HACerror)
Lowconf<-beta-(1.96*HACerror)
Highconf<-beta+(1.96*HACerror)
confidenceinterval<-c(Lowconf, Highconf)
confidenceinterval

#beta and confidence interval plots given by code below
#Outputs created were merged outside of R script to make the final used in diss main body
#modelname is RDPM2000 RDPM 2010
#modelname2 is RIPM2000
#modelname3 is RDPM1989 and RDPM C&D
#modelname4 is RDPM1989
#modelname5 is RDPMC&D
#modelname6 is all models in order RDPM2000 RDPM 2010 RIPM2000 RDPM1989 and RDPM C&D

modelname<-c(1,2,3,4,5,6,6.01)
betaest<-c(0.9088879,0.9147745,0.9343362,0.9585281,0.8956812,0.940717,0)
lowcl<-c(0.7423869,0.7241945,0.746832,0.776745,0.6311328,0.8131906,0)
highcl<-c(1.0753889,1.1053545,1.121834,1.1403105,1.1602297, 1.0682434,0)
first<-data.frame(modelname,betaest,lowcl,highcl)
plotCI(x = first$modelname,y =first$betaest,li = first$lowcl,ui = first$highcl,scol="Red",gap="TRUE",pch=21, pt.bg=par("bg"))

modelname2<-c(0,1,2,3,4,5,6)
betaest2<-c(0,0.7691391,0.9066263,0.849615,0,0,0)
lowcl2<-c(0,0.5242568,0.8029809,0.5640404,0,0,0)
highcl2<-c(0,1.0140214,1.0102716,1.1356827,0,0,0)
second<-data.frame(modelname2,betaest2,lowcl2,highcl2)
plotCI(x = second$modelname,y =second$betaest,li = second$lowcl,ui = second$highcl,scol="Green",gap="TRUE",pch=21, pt.bg=par("bg"))

modelname3<-c(1,2,3,4,5,6,6)
betaest3<-c(0.6954679,0.7096002,0.872475,0.2122954,0.24406,0.09662221,1)
lowcl3<-c(0.2984084,0.3424815,0.6283788,0.1010557,0.1036838,0.0286342,1)
highcl3<-c(1.0925275,1.0767188,1.1165713,0.323535,0.3851282,0.16460980,1)
third<-data.frame(modelname3,betaest3,lowcl3,highcl3)
plotCI(x = third$modelname,y =third$betaest,li = third$lowcl,ui = third$highcl,scol="Blue",gap="TRUE",pch=21, pt.bg=par("bg"))

modelname4<-c(1,2,3,4,5,6)
betaest4<-c(0.6954679,0.7096002,0.872475,0,0,0)
lowcl4<-c(0.2984084,0.3424815,0.6283788,0,0,0)
highcl4<-c(1.0925275,1.0767188,1.1165713,0,0,0)
fourth<-data.frame(modelname4,betaest4,lowcl4,highcl4)
plotCI(x = fourth$modelname,y =fourth$betaest,li = fourth$lowcl,ui = fourth$highcl,scol="Orange",gap="TRUE",pch=21, pt.bg=par("bg"))

modelname5<-c(1,2,3,4,5,6)
betaest5<-c(1,0,0,0.2122954,0.24406,0.09662221)
lowcl5<-c(1,0,0,0.1010557,0.1036838,0.0286342)
highcl5<-c(1,0,0,0.323535,0.3851282,0.16460980)
fith<-data.frame(modelname5,betaest5,lowcl5,highcl5)
plotCI(x = fith$modelname,y =fith$betaest,li = fith$lowcl,ui = fith$highcl,scol="Purple",gap="TRUE",pch=21, pt.bg=par("bg"))

modelname6<-c(1:15)
betaest<-c(0.9088879,0.9147745,0.9343362,0.9585281,0.8956812,0.940717,0.7691391,0.9066263,0.849615,0.6954679,0.7096002,0.872475,0.2122954,0.24406,0.09662221)
lowcl<-c(0.7423869,0.7241945,0.746832,0.776745,0.6311328,0.8131906,0.5242568,0.8029809,0.5640404,0.2984084,0.3424815,0.6283788,0.1010557,0.1036838,0.0286342)
highcl<-c(1.0753889,1.1053545,1.121834,1.1403105,1.1602297, 1.0682434,1.0140214,1.0102716,1.1356827,1.0925275,1.0767188,1.1165713,0.323535,0.3851282,0.16460980)
fullCI<-data.frame(modelname6,betaest,lowcl,highcl)
plotCI(x = fullCI$modelname,y =fullCI$betaest,li = fullCI$lowcl,ui = fullCI$highcl,gap="TRUE",pch=21, pt.bg=par("bg"))



