# *********************************
# VALIDACIÓN CRUZADA REPETIDA Y BOXPLOT para
# 
# LOGISTICA
# AVNNET
# *********************************

# saheart<-read.sas7bdat("c:/saheart.sas7bdat")
# listconti<-c("sbp", "tobacco", "ldl", "adiposity",  "obesity", 
#  "alcohol", "age", "typea")
# listclass<-c("famhist")
# vardep<-c("chd")

library(plyr)
detach(package:plyr)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(dplyr)
library(pROC)

# *********************************
# CRUZADA LOGISTICA
# *********************************


cruzadalogistica <- function(data=data,vardep=NULL,
 listconti=NULL,listclass=NULL,grupos=4,sinicio=1234,repe=5)
{
 
  if  (listclass !=c(""))
  {
   for (i in 1:dim(array(listclass))) {
    numindi<-which(names(data)==listclass[[i]])
    data[,numindi]<-as.character(data[,numindi])
    data[,numindi]<-as.factor(data[,numindi])
   }
  }   
  
  data[,vardep]<-as.factor(data[,vardep])
  
  # Creo la formula para la logistica
  
  if  (listclass!=c(""))
  {
   koko<-c(listconti,listclass)
  }  else   {
   koko<-c(listconti)
  }
 
  modelo<-paste(koko,sep="",collapse="+")
  formu<-formula(paste(vardep,"~",modelo,sep=""))
  
  formu 
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
   savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  regresion <- train(formu,data=data,
   trControl=control,method="glm",family = binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
   confu<-confusionMatrix(x,y)
   tasa<-confu[[3]][1]
   return(tasa)
  }
  
  # Aplicamos función sobre cada Repetición
  
  medias<-preditest %>%
   group_by(Rep) %>%
   summarize(tasa=1-tasafallos(pred,obs))
  
  # CalculamoS AUC  por cada Repetición de cv 
  # Definimnos función
  
  auc<-function(x,y) {
   curvaroc<-roc(response=x,predictor=y)
   auc<-curvaroc$auc
   return(auc)
  }
  
  # Aplicamos función sobre cada Repetición
  
  mediasbis<-preditest %>%
   group_by(Rep) %>%
   summarize(auc=auc(obs,Yes))
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  return(medias)
  
 }




# *********************************
# CRUZADA avNNet
# **************


cruzadaavnnetbin<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",grupos=4,sinicio=1234,repe=5,
  size=c(5),decay=c(0.01),repeticiones=5,itera=100,trace=TRUE)
 { 
  
  # Preparación del archivo
  
  # b)pasar las categóricas a dummies
  
  if  (listclass!=c(""))
  {
   databis<-data[,c(vardep,listconti,listclass)]
   databis<- dummy.data.frame(databis, listclass, sep = ".")
  }  else   {
   databis<-data[,c(vardep,listconti)]
  }
  
  # c)estandarizar las variables continuas
  
  # Calculo medias y dtipica de datos y estandarizo (solo las continuas)
  
  means <-apply(databis[,listconti],2,mean)
  sds<-sapply(databis[,listconti],sd)
  
  # Estandarizo solo las continuas y uno con las categoricas
  
  datacon<-scale(databis[,listconti], center = means, scale = sds)
  numerocont<-which(colnames(databis)%in%listconti)
  databis<-cbind(datacon,databis[,-numerocont,drop=FALSE ])
  
  databis[,vardep]<-as.factor(databis[,vardep])
  
  formu<-formula(paste(vardep,"~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
   savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
  
  avnnet<- train(formu,data=databis,
   method="avNNet",linout = FALSE,maxit=itera,repeats=repeticiones,
   trControl=control,tuneGrid=avnnetgrid,trace=trace)
  
  print(avnnet$results)
  
  preditest<-avnnet$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
   confu<-confusionMatrix(x,y)
   tasa<-confu[[3]][1]
   return(tasa)
  }
  
  # Aplicamos función sobre cada Repetición
  
  medias<-preditest %>%
   group_by(Rep) %>%
   summarize(tasa=1-tasafallos(pred,obs))
  
  # CalculamoS AUC  por cada Repetición de cv 
  # Definimnos función
  
  auc<-function(x,y) {
   curvaroc<-roc(response=x,predictor=y)
   auc<-curvaroc$auc
   return(auc)
  }
  
  # Aplicamos función sobre cada Repetición
  
  mediasbis<-preditest %>%
   group_by(Rep) %>%
   summarize(auc=auc(obs,Yes))
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  return(medias)
  
 }


## Ejemplo de utilización cruzada LOGISTICA Y AVNNET

# load ("saheartbis.Rda")
# 
# medias1<-cruzadalogistica(data=saheartbis,
#  vardep="chd",listconti=c("sbp", "tobacco", "ldl",
#   "adiposity",  "obesity", "famhist.Absent"),
#  listclass=c(""), grupos=4,sinicio=1234,repe=5)
# 
#  medias1$modelo="Logística"
# 
# 
# medias2<-cruzadaavnnetbin(data=saheartbis,
#  vardep="chd",listconti=c("sbp", "tobacco",
#   "ldl", "adiposity",  "obesity", "famhist.Absent"),
#  listclass=c(""),grupos=4,sinicio=1234,repe=5,
#   size=c(5),decay=c(0.1),repeticiones=5,itera=200)
# 
#   medias2$modelo="avnnet"
# 
# union1<-rbind(medias1,medias2)
# 
# par(cex.axis=0.5)
# boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
# boxplot(data=union1,auc~modelo,main="AUC")
# 
# 
# 
