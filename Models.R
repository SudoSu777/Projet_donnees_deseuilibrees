library(randomForest)
library(e1071)
library(dplyr)
library(glmnet)
library(gbm)
library(Metrics)


prev.lm <- function(df.X,newX){
  linear <- lm(maxO3 ~.,data=df.X)
  as.vector(predict(linear,newx = newX,type="response"))
}

-----------------------------------------------------------------------------
  

x_train=model.matrix(maxO3~.-1,data=trainingData) # predictor matrix
x_test=model.matrix(maxO3~.-1,data=testData) # predictor matrix
y_train=trainingData$maxO3
-----------------------------------------------------------------------------
  
prev.ridge <- function(df.X,df.Y,newX){
  ridge <- cv.glmnet(df.X,df.Y,alpha=0)
  as.vector(predict(ridge,newx = newX))
}

-----------------------------------------------------------------------------
prev.lasso <- function(df.X,df.Y,newX){
  lasso <- cv.glmnet(df.X,df.Y,alpha=1)
  as.vector(predict(lasso,newx = newX))
}

-----------------------------------------------------------------------------
  

prev.elasticnet <- function(df.X,df.Y,newX){
  lasso <- cv.glmnet(df.X,df.Y,family="gaussian",alpha=.5,type.measure = "mse")
  as.vector(predict(lasso,newx = newX,type="response"))
}

-----------------------------------------------------------------------------
  
  prev.svm <- function(df,newX){
    res.svm <- tune(svm, maxO3 ~.,  data = df,
                    ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
    )
    tunedModel <- res.svm$best.model
    predict(tunedModel, newX) 
    }
  
  -----------------------------------------------------------------------------
    prev.foret <- function(df,newX){
      foret <- randomForest(maxO3 ~ ., data =df, mtry = 3, 
                            importance = TRUE, na.action = na.omit) 
      as.vector(predict(foret,data=newX))
    }
    
    -----------------------------------------------------------------------------
      prev.gbm <- function(df,newX){
        ada <- gbm(maxO3~.,data=df,distribution="gaussian",interaction.depth=2,
                   bag.fraction=1,cv.folds = 5,n.trees=500)
        nb.it <- gbm.perf(ada,plot.it=FALSE)
        as.vector(predict(ada,newdata=newX,n.trees=nb.it))
      }
      
----------------------------------------------------------------------------Interaction
  

test=testData[,-1]
vec=colnames(df)[-c(1,2)]
u=c("maxO3")
v=c()
for (i in vec)
  {
  u=c(u,i)
  v=c(v,i)
  print(prev.lm(trainingData[,u],testData[,v]))
  }
  
-----------------------------------------------------------------------------
  # Création de nouvelles variables x2,x3,x4 puis refaire les étapes précédentes
  
  for (i in vec){
    df[,paste(i,"2",sep = "_")]=df[,i]^2
    df[,paste(i,"3",sep = "_")]=df[,i]^3
    df[,paste(i,"4",sep = "_")]=df[,i]^4
    
  }

-----------------------------------------------------------------------------Splines
ozone=as.data.frame(df)
ozone$date=NULL
dfSimple <- ozone
Xozone <- model.matrix(maxO3~.,data=ozone)[,-1]
dfpoly <- data.frame(maxO3=ozone$maxO3,Xozone,Xozone^2,Xozone^3)
dfinter <- data.frame(maxO3=ozone$maxO3,model.matrix(maxO3~.^2,data=ozone)[,-1])
###pour les splines
library(splines)
BB <- NULL
for(i in 1:ncol(Xozone)){
  var <- Xozone[,i]
  BX <- bs(var,knots=quantile(var,prob=c(.25,.5,.75)),degre=3,
           Boundary.knots=c(min(var),max(var)))
  colnames(BX) <- paste(colnames(Xozone)[i],"-b",1:6,sep="")
  BB <- cbind(BB,BX)
}
dfspline <- data.frame(maxO3=ozone$maxO3,BB)
  

#-------------------------------------------------------Validation croisee
Y=df$maxO3
x_train=model.matrix(maxO3~.-1,data=trainingData) # predictor matrix
x_test=model.matrix(maxO3~.-1,data=testData)
y_train=trainingData$maxO3

bloc=sample(rep(1:10,length=nrow(df)),replace=TRUE)
table(bloc)
score <- data.frame(matrix(0,nrow=nrow(df),ncol=3))
names(score) <- c("foret","boosting","linear")

for (k in 1:10){
  foret.k=NULL
  ind.test <- bloc==k
  X.app <- df[!ind.test,]
  X.test <- df[ind.test,-1]
  Y.app <- Y[!ind.test]
  Y.test <- Y[ind.test]
  #ridge.k <- prev.ridge(df.X=X.app,df.Y=Y.app,newX=X.test)
  #lasso.k <- prev.lasso(df.X=X.app,df.Y=Y.app,newX=X.test)
  foret.k=append(foret.k,rmse(X.app$maxO3,prev.foret(X.app,X.test)))
  
  #score[ind.test,] <- data.frame(foret=foret.k,boosting=gbm.k,linear=lm.k)
}
print(foret.k)


#Presentation de la problématique
#Approche et methodologie de travail

#mise en oeuvre de modeles(regression moindres carrées,gbm,randomforest,Ridge,Lasso )

#Comparaison des modèles par validation croisée sur les variables initiales

#Création d'un nouveau jeu de données avec de nouvelles variables(variables^2,variables^3 et variables^4)

#Comparaison de modeles sur le nouveau jeu de données

#Selection de variables par  méthodes forward et mise en oeuvre de modeles
#comparaison de modèles
#Nuages des moots des varaibles les plus pertinentes

#B-splines et comparaison de modeles de regressions

