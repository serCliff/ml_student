library(plyr)
detach(package:plyr)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(dplyr)
library(pROC)


cruzadaarbolbin<-
 function(data=data,vardep="vardep",
  listconti="listconti",listclass="listclass",
  grupos=4,sinicio=1234,repe=5,
  cp=c(0),minbucket =20)
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
  
  formu<-formula(paste("factor(",vardep,")~.",sep=""))
  
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
   savePredictions = "all",classProbs=TRUE) 
  
  # Aplico caret y construyo modelo
  
  arbolgrid <-  expand.grid(cp=cp)
  
  arbol<- train(formu,data=databis,
   method="rpart",trControl=control,
   tuneGrid=arbolgrid,minbucket=minbucket)
  
  print(arbol$results)
  
  preditest<-arbol$pred
  
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


# load ("saheartbis.Rda")
# source ("cruzadas avnnet y log binaria.R")
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
#   
#   medias3<-cruzadaarbolbin(data=saheartbis,
#  vardep="chd",listconti=c("sbp", "tobacco",
#   "ldl", "adiposity",  "obesity", "famhist.Absent"),
#  listclass=c(""),grupos=4,sinicio=1234,repe=5,
#   cp=c(0),minbucket =5)
# 
#   medias3$modelo="arbol"
# 
#   
#   union1<-rbind(medias1,medias2,medias3)
# 
# par(cex.axis=0.5)
# boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
# boxplot(data=union1,auc~modelo,main="AUC")



