
############################### Pretraitement de la base de donnees
install.packages('questionr')
library('questionr')
base1=read.table("matDonneesCorioV3_complet.txt", header = TRUE)
dim(base1)
18683    61
annee<-base1[,56]
 Indice1<-which(annee==2011)
 Indice2<-which(annee==2012)
BaseAnnee_2011<-base1[Indice1,]
dim(BaseAnnee_2011)
[1] 12671    61
BaseAnnee_2012<-base1[Indice2,]
dim(BaseAnnee_2012)
[1] 6012   61
jour<-base1[,54]
> max(jour)
[1] 31

mois<-base1[,55]
> max(mois)
[1] 12

########################################################## NOUVELLE BASE
Base_donne<-base1[,c(1,2,25,26,27,51,52,53,55)]
names(Base_donne)<-NULL
names(Base_donne)<-c( 'S_3.05m','S_9.45m','S_1137m','T_3.05m','T_9.45m','ADT','Longitude','Latitude','mois')
 dim(Base_donne)
 18683     9
 colnames(Base_donne)
  "S_3.05m"    "S_9.45m"    "S_1137m" "T_3.05m"    "T_9.45m"   
 "ADT"        "Longitude"  "Latitude"   "mois"      
 summary(Base_donne)
  write.table(Base_donne,file="Base_donne.txt",row.names=FALSE,col.names=TRUE)
########### fin ########################################## Imputation multiple avec le packages VIM
 install.packages("VIM")
  library("VIM")
png("visualNA.png", width = 1200, height = 800)
aggr(Base_donne, numbers = TRUE, prop = c(TRUE, FALSE))
dev.off()
#### imputation avec missForest pour Données hétérogènes
install.packages("missForest")
library(missForest)
Base_donne.imp_missForest <- missForest(Base_donne,verbose = TRUE)
 missForest iteration 1 in progress...done!
    estimated error(s): 0.06318681 
    difference(s): 0.002097247 
    time: 1052.405 seconds

  missForest iteration 2 in progress...done!
    estimated error(s): 0.06244109 
    difference(s): 3.791185e-05 
    time: 2273.596 seconds

  missForest iteration 3 in progress...done!
    estimated error(s): 0.06227352 
    difference(s): 4.799793e-05 
    time: 1040.465 seconds
Base_donne.imp_missForest$OOBerror  
 NRMSE 
0.06244109 
Base_donne.imp_missForest<- missForest(Base_donne,maxiter = 10,verbose = TRUE)
names(Base_donne.imp_missForest)
[1] "ximp"     "OOBerror"
Base_donne.imp_missForest$OOBerror  
      NRMSE 
   0.06292269 
write.table(Base_donne.imp_missForest$ximp,file="Base_donne_impute.txt",row.names=FALSE,col.names=TRUE)

#### imputation avec Imseq
library('rrcovNA')
imputed.Base_donne <- impSeq(Base_donne)
################################## Visualisation pour prétraitement
### scatterplot
png(file = "scatterplot_Base_donne.png")
 pairs(~S_3.05m+S_9.45m+S_1137m+T_3.05m+T_9.45m+ADT+Longitude+Latitude+mois ,data =Base_donne, main = "Scatterplot Base_donne")
dev.off()

#### correlation
install.packages('corrplot')
library(corrplot)
mcor <- cor(Base_donne.imp_missForest$ximp)
png(file = "Correlation~variables.png")
corrplot(mcor, method = 'circle')
dev.off()

################################ Regression Tree avec kfold validation
install.packages('tree')
library("tree")
# For data visualization
install.packages('rpart.plot')
library("rpart.plot")
library(rpart)
library("dplyr")
Mytree=rpart(S_1137m~.,data=Base_donne.imp_missForest$ximp,control=rpart.control(minsplit=10,cp=0.01)) 
Base_cross_validation=Base_donne.imp_missForest$ximp[1:12400,]
tab_kfold=sample_n(Base_cross_validation,12400,replace=TRUE)
head(tab_kfold)
n=nrow(tab_kfold) ; K=124 ; Etest=NULL ; nbloc=floor(n/K) ; 
indtestk=rep(0,K) ; 
for (k in 1:K){  indtestk= ((k-1)*nbloc+1):(k*nbloc)
                 indappk=-indtestk 
     mytree=rpart(S_1137m~.,data=tab_kfold[indappk,],control=rpart.control(minsplit=10,cp=0.01))            
     #Mytree=tree(S_1137m~.,data=tab_kfold[indappk,]) ;
    ptestk=predict(mytree,newdata=tab_kfold[indtestk,]);
    ptestrain=predict(mytree,newdata=tab_kfold[indappk,])
    Etest=tab_kfold[indtestk,"S_1137m"]-ptestk
    cat("L'erreur residuelle est de", Etest,"\n")
    Etestrain=tab_kfold[indappk,"S_1137m"]-ptestrain
    Etestk=sqrt(mean(tab_kfold[indtestk,"S_1137m"]-ptestk)^2)
    cat("La moyenne quadratique est de",Etestk,"\n")
    }
    Etestk
[1] 0.01249081
################# visualisation
png(file = "treevisualAll.png")
rpart.plot(Mytree)
dev.off()

mytree=rpart(S_1137m~.,data=tab_kfold[indappk,],control=rpart.control(minsplit=10,cp=0.01))
png(file = "rpartvisual.png")
rpart.plot(mytree)
dev.off()
########## boxplot
    #Etest=c(Etest,Etestk)
    png("boxplotmytree1.png", width = 1200, height =800)
    boxplot(Etest,main="Erreur residuelle",ylim=c(-0.5,0.5),col=c("blue"))
    points(mean(Etest), pch =17,cex=2, col ="red")
   dev.off()

png("plot1.png", width = 1200, height =800)
plot(tab_kfold[indtestk,"S_1137m"],tab_kfold[indtestk,"S_1137m"],xlim=c(32,39),ylim=c(32,39),main="", pch=17)
lines(ptestk, ptestk,col="#c26f10",lty=2, lwd=2)
dev.off()
################################################################## RandomForest avec kfold validation
install.packages("randomForest") 
library(randomForest)
Base_cross_validation=Base_donne.imp_missForest$ximp[1:12400,]
tab_kfold=sample_n(Base_cross_validation,12400,replace=TRUE)
head(tab_kfold)
n=nrow(tab_kfold) ; K=124 ; Etest=NULL ; nbloc=floor(n/K) ; 
indtestk=rep(0,K) ; 
for (k in 1:K){  indtestk= ((k-1)*nbloc+1):(k*nbloc)
                 indappk=-indtestk 
     RandomForest=randomForest(S_1137m~.,data=tab_kfold[indappk,])            
     print(RandomForest)
    ptestRandom=predict(RandomForest,newdata=tab_kfold[indtestk,]); 
    ptestRandomtrain=predict(RandomForest,newdata=tab_kfold[indappk,])
    EtestRandom=tab_kfold[indtestk,"S_1137m"]-ptestRandom
    cat("L'erreur residuelle est de", EtestRandom,"\n")
    EtestRandomtrain=tab_kfold[indappk,"S_1137m"]-ptestRandomtrain
    EtestkRandom=sqrt(mean(tab_kfold[indtestk,"S_1137m"]-ptestRandom)^2)
    cat("La moyenne quadratique est de",EtestkRandom,"\n")
    }
    
    EtestkRandom
 L erreur moyenne quadratique est de 0.0005781721 
  print(RandomForest)
 Call:
 randomForest(formula = S_1137m ~ ., data = tab_kfold[indappk, ]) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 2

          Mean of squared residuals: 0.00201468
                    % Var explained: 99.79   
############################################################ Visualisation des resultats 
 importance(RandomForest)
           IncNodePurity
S_3.05m      2475.52920
S_9.45m      3304.30531
T_3.05m       176.31959
T_9.45m       177.11770
ADT          1314.76055
Longitude    1558.60843
Latitude     2631.00064
mois           45.23277
 RandomForest$importance[order(RandomForest$importance[, 1], decreasing = TRUE), ]
   S_9.45m   Latitude    S_3.05m  Longitude        ADT    T_9.45m    T_3.05m 
3304.30531 2631.00064 2475.52920 1558.60843 1314.76055  177.11770  176.31959 
      mois 
  45.23277 
       png("visual_varImplotRandomForest.png", width = 1200, height =800)
       varImpPlot(RandomForest,col="red",lwd=1,pch=15, lty=18) 
        dev.off()
#######
png("visual_compareRandomForest.png", width = 1200, height =800)
plot(ptestRandom,tab_kfold[indtestk,"S_1137m"],col="blue", main = "test data & data prediction of depth salinity ~ RandomForest")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
  dev.off()
#######
png("courbe_compareRandomForest.png", width = 1200, height =800)
  x = 1:length(indtestk)
plot(x,tab_kfold[indtestk,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ RandomForest")
lines(x,ptestRandom, col = "blue", lwd=2)
legend("topright",  legend = c("Real-S_1137m", "predicted-S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
 grid()
 dev.off()
png(file = "rpartRandomForest.png")
plot(RandomForest)
dev.off()
 ####
 save(RandomForest, ptestRandom, EtestkRandom,file = "Random_env.RData") 
################################################################## Bagging avec kfold validation
install.packages("ipred")
install.packages("caret")
library(rpart) 
library(caret)   
library(ipred)     
Base_cross_validation=Base_donne.imp_missForest$ximp[1:12400,]
tab_kfold=sample_n(Base_cross_validation,12400,replace=TRUE)
head(tab_kfold)
n=nrow(tab_kfold) ; K=124 ; Etest=NULL ; nbloc=floor(n/K) ; 
indtestk=rep(0,K) ; 
for (k in 1:K){  indtestk= ((k-1)*nbloc+1):(k*nbloc)
                 indappk=-indtestk 
     Bagging=bagging(S_1137m~.,data=tab_kfold[indappk,],coob=TRUE)            
     print(Bagging)
    ptestBagging=predict(Bagging,newdata=tab_kfold[indtestk,]); 
    ptestBaggingtrain=predict(Bagging,newdata=tab_kfold[indappk,])
    EtestBagging=tab_kfold[indtestk,"S_1137m"]-ptestBagging
    cat("L'erreur residuelle est de", EtestBagging,"\n")
    EtestBaggingtrain=tab_kfold[indappk,"S_1137m"]-ptestBaggingtrain
    EtestkBagging=sqrt(mean(tab_kfold[indtestk,"S_1137m"]-ptestBagging)^2)
    cat("L'erreur moyenne quadratique est de",EtestkBagging,"\n")
    }
    
    EtestkBagging
    L erreur moyenne quadratique est de 0.004657065 
    
    Call: bagging.data.frame(formula = S_1137m ~ ., data = tab_kfold[indappk, ], coob = TRUE)

Out-of-bag estimate of root mean squared error:  0.1676    
################################################################## KNN avec kfold validation
install.packages("caret")
library(caret)   
Base_cross_validation=Base_donne.imp_missForest$ximp[1:12400,]
tab_kfold=sample_n(Base_cross_validation,12400,replace=TRUE)
#Mytree=tree(S_1137m~.,data=);
#py1=predict(Mytree,)
head(tab_kfold)
n=nrow(tab_kfold) ; K=124 ; Etest=NULL ; nbloc=floor(n/K) ; 
indtestk=rep(0,K) ; 
for (k in 1:K){  indtestk= ((k-1)*nbloc+1):(k*nbloc)
                 indappk=-indtestk 
     Model_KNN=knnreg(S_1137m~.,data=tab_kfold[indappk,])            
     print(Model_KNN)
    ptestKNN=predict(Model_KNN,newdata=tab_kfold[indtestk,]);
    ptestKNNtrain=predict(Model_KNN,newdata=tab_kfold[indappk,])
    EtestKNN=tab_kfold[indtestk,"S_1137m"]-ptestKNN
    cat("L'erreur residuelle est de", EtestKNN,"\n")
    EtestKNNtrain=tab_kfold[indappk,"S_1137m"]-ptestKNNtrain 
    ###  RMSE
    Etestk_KNN=sqrt(mean(tab_kfold[indtestk,"S_1137m"]-ptestKNN)^2)
    cat("L'erreur moyenne quadratique est de",Etestk_KNN,"\n")
    }
    
    Etestk_KNN
    L erreur moyenne quadratique est de 0.0005200475 
    ###### erreur absolue moyenne
    mae = caret::MAE(tab_kfold[indtestk,"S_1137m"],ptestKNN)
    cat("MAE:",mae)
    MAE: 0.02173576
####################################################### Visualisation des resultats
png("visual_compareKNNreg.png", width = 1200, height =800)
plot(ptestKNN,tab_kfold[indtestk,"S_1137m"],col="blue", main = "test data & data prediction of depth salinity~kNN-regression")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
  dev.off()
####################
x = 1:length(indtestk)
png("courbe_compareKNNreg.png", width = 1200, height =800)
plot(x,tab_kfold[indtestk,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ kNN-regression")
lines(x,ptestKNN, col = "blue", lwd=2)
legend("topright",  legend = c("Real~S_1137m", "predicted~S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
dev.off()
 
############################## Boxplots pour comparaison
png("boxplot_Compare.png", width = 1200, height =800)
 par(mfrow=c(1,2))
 boxplot(Etest,EtestRandom,EtestBagging,EtestKNN,names=c("test Tree","test RandomForest","test Bagging","test KNN"),col=c("blue","pink","red","yellow" ),main="comparaison des # test Erreurs Résiduelles")
 boxplot(Etestrain,EtestRandomtrain,EtestBaggingtrain,EtestKNNtrain,names=c(" train Tree","train RandomForest","train Bagging"," train KNN"),col=c("blue","pink","red","yellow" ),main="comparaison des # train Erreurs Résiduelles ")
dev.off()

##################################### Visualisation de l'estimateur le reste de la base de donnees
#### RandomForest
Base_globale_test=Base_donne.imp_missForest$ximp[12401:18683,]
Test_RandomForest=predict(RandomForest,newdata=Base_globale_test);
png("visual_TestRandomForest.png", width = 1200, height =800)
plot(Test_RandomForest,Base_globale_test[,"S_1137m"],col="blue", main = "Test data  & data prediction of depth salinity ~ RandomForest")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
  dev.off()
png("courbe_TestRandomForest.png", width = 1200, height =800)
  x = 1:length(Base_globale_test[,"S_1137m"])
plot(x,Base_globale_test[,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ RandomForest")
lines(x,Test_RandomForest, col = "blue", lwd=2)
legend("topright",  legend = c("Real-S_1137m", "predicted-S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
 grid()
 dev.off()  

####### KNN-regression

Test_KNN=predict(Model_KNN,newdata=Base_globale_test); 
 png("visual_TestKNNreg.png", width = 1200, height =800)
plot(Test_KNN,Base_globale_test[,"S_1137m"],col="blue", main = "Test data  & data prediction of depth salinity ~ KNN-regression")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
  dev.off()
  
 png("courbe_TestKNN.png", width = 1200, height =800)
  x = 1:length(Base_globale_test[,"S_1137m"])
plot(x,Base_globale_test[,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ KNN-regression")
lines(x,Test_KNN, col = "blue", lwd=2)
legend("topright",  legend = c("Real-S_1137m", "predicted-S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
 grid()
 dev.off()   
 ################
 png("visual_TestCompare.png", width = 1200, height =800)
par(mfrow=c(1,2)) 
plot(Test_RandomForest,Base_globale_test[,"S_1137m"],col="blue", main = "Test data  & data prediction of depth salinity ~ RandomForest")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
 plot(Test_KNN,Base_globale_test[,"S_1137m"],col="blue", main = "Test data  & data prediction of depth salinity ~ KNN-regression")
  abline (0 ,1,col=2) 
  grid(NA, 5, lwd = 2)
  dev.off()
 ############
 png("courbe_TestCompare.png", width = 1200, height =800)
 par(mfrow=c(1,2)) 
  x = 1:length(Base_globale_test[,"S_1137m"])
plot(x,Base_globale_test[,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ RandomForest")
lines(x,Test_RandomForest, col = "blue", lwd=2)
legend("topright",  legend = c("Real-S_1137m", "predicted-S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
 grid()
  #x = 1:length(Base_globale_test[,"S_1137m"])
plot(x,Base_globale_test[,"S_1137m"], col = "red", type = "l", lwd=2,
     main = "depth salinity of test data & data prediction ~ KNN-regression")
 lines(x,Test_KNN, col = "blue", lwd=2)
legend("topright",  legend = c("Real-S_1137m", "predicted-S_1137m"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
 grid()
 dev.off()   
############################### FIN ############################################################  
