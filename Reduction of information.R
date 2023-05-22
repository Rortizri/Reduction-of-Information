library(readxl)
data <- read_excel("C:/Users/Nader/Desktop/EBDS/drug_consumption/data.xlsx")

library(caret)
set.seed(42) # pour garantir que le meme echantillion est selectionné chaque fois qu'on cree une partition train et test 
train_id <- createDataPartition(data$Cannabis, p=0.80) # train=80%,test=20%
data_train=data[train_id$Resample1,]          #Base Train
data_test=data[-train_id$Resample1,] 



y = data[,11]
X = data[,-11]
library(base)
library(glmnet)
X =data.matrix(X)
y = data.matrix(y)
class(X)


#####################################################################
# Logistic Regression
data_train_logit = data_train[,-12]
data_test_logit = data_test[,-12]

logit=step(glm(Cannabis~.,data = data_train_logit,family = binomial(link="logit")),direction="both")
summary(logit)
summary(logit)$coefficient

p <- predict(logit, data_test_logit, type = "response")
consumption <- ifelse(p > 0.5, 1, 0)

p_class <- factor(consumption, levels = levels(as.factor(data_test_logit[["Cannabis"]])))
class(p_class)

confusionMatrix(p_class, as.factor(data_test_logit[["Cannabis"]]))

library(ResourceSelection)
logit$y
logit$fitted
hoslem.test(logit$y, logit$fitted)
########################################################################################
#   Principal Component Analysis
library("FactoMineR")
library("factoextra")


y_pca = data[["Cannabis"]]
#X_pca = data_train[,-11]
X_pca = data[,c("Oscore","Ascore","Cscore","SS","Age","Gender","Salary","Education_Doctorate","Education_SomeCollege","Education_Under16","Country_Canada","Country_Other","Country_UK","Ethnicity_Other","Ethnicity_White","Ethnicity_White_Asian","Ethnicity_White_Black")]
#X_pca = X_pca[,-11]
df <- X_pca %>% scale()
is.na(df)

res.pca <- PCA(df, ncp = 8, graph = TRUE)
eig.val <- get_eigenvalue(res.pca)
sum(eig.val[,1])


ind <- get_pca_ind(res.pca)
ind

class(ind$coord)

dim(ind$coord)












fviz_eig(res.pca, addlabels = TRUE)

fviz_pca_var(res.pca, col.var = "black")
library("corrplot")
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE)

fviz_cos2(res.pca, choice = "var", , axes = 1:8)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)

fviz_pca_var(res.pca, alpha.var = "cos2")
corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1:8)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 5)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = data_train$Cannabis, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")


formula = Cannabis ~ Oscore+Ascore+Cscore+SS+ Age + Gender+Salary+Education_Doctorate+Education_SomeCollege+Education_Under16+Country_Canada+Country_Other+Country_UK+Ethnicity_Other+Ethnicity_White+Ethnicity_White_Asian +Ethnicity_White_Black


#############################################################################
#reg log pca
X_pca = data_test[,c("Oscore","Ascore","Cscore","SS","Age","Gender","Salary","Education_Doctorate","Education_SomeCollege","Education_Under16","Country_Canada","Country_Other","Country_UK","Ethnicity_Other","Ethnicity_White","Ethnicity_White_Asian","Ethnicity_White_Black")]
#X_pca = X_pca[,-11]
df <- X_pca %>% scale()
res.pca2 <- PCA(df, ncp = 8, graph = TRUE)
ind2 <- get_pca_ind(res.pca2)



data_pca = cbind(ind2$coord,data_test$Cannabis)


colnames(data_pca) <- c('PCA1','PCA2','PCA3','PCA4','PCA5','PCA6','PCA7','PCA8','Cannabis')

data_pca=as.data.frame(data_pca)

regression_log_pca=step(glm(Cannabis~.,data = data_pca,family = binomial(link="logit")),direction = "both")
summary(regression_log_pca)
summary(regression_log_pca)$coefficient

p2 <- predict(regression_log_pca, data_pca, type = "response")
consumption2 <- ifelse(p2 > 0.5, 1, 0)

p_class2 <- factor(consumption2, levels = levels(as.factor(data_pca[["Cannabis"]])))
class(p_class2)

confusionMatrix(p_class2, as.factor(data_pca[["Cannabis"]]))








##############################################################################

#######################################################################################
y1 = data_train[,11]
X1 = data_train[,-11]

y2 = data_test[,11]
X2 = data_test[,-11]

X1 =data.matrix(X1)
y1 = data.matrix(y1)

X2 =data.matrix(X2)
y2 = data.matrix(y2)
lasso_mod = cv.glmnet(X1, y1, family = "binomial", alpha = 1)
lasso_mod = glmnet(X1, y1, family = "binomial", alpha = 1)
plot(lasso_mod,label = TRUE)
lasso_mod$lambda.min
lasso_mod$lambda.1se
coef(lasso_mod, s=lasso_mod$lambda.min)

plot(lasso_mod, xvar = "dev", label = TRUE)
pred_lasso = predict(lasso_mod, newx = X2, type = "class", s = lasso_mod$lambda.min)
p3 = as.numeric(pred_lasso[,'s1'])
confusion.glmnet(
  pred_lasso,
  newx = X2,
  newy=y2,
  family = "binomial")


rsq = 1 - lasso_mod$cvm/var(y1)
max(rsq)

plot(lasso_mod$lambda,rsq)
#########################################################################

# Ridge regression

ridge_mod = cv.glmnet(X1, y1, alpha = 0, family = "binomial")
plot(ridge_mod,label = TRUE)
ridge_mod$lambda.min
coef(ridge_mod, s = ridge_mod$lambda.min)

rsq2 = 1 - ridge_mod$cvm/var(y1)
max(rsq2)

plot(ridge_mod$lambda,rsq2)



dim(coef(ridge_mod))
   # Draw plot of coefficients
ridge_mod$lambda.min #Display 50th lambda value

pred_ridge = predict(ridge_mod, newx = X2, type = "class", s = ridge_mod$lambda.min)
pred_ridge

confusion.glmnet(
  pred_ridge,
  newx = X2,
  newy=y2,
  family = "binomial")

##########################################################################
#Roc curve
library(pROC)
rocs <- list()
rocs[["Logistic regression"]] <- roc(data_test$Cannabis, p)
rocs[["Logistic regression with PCA"]] <- roc(data_test$Cannabis, p2)
rocs[["Lasso logistic regression"]] <- roc(data_test$Cannabis, as.numeric(pred_lasso[,'s1']))
rocs[["Ridge logistic regression"]] <- roc(data_test$Cannabis, as.numeric(pred_ridge[,'s1']))
ggroc(rocs)

rocs[["Logistic regression"]]$auc
rocs[["Logistic regression with PCA"]]$auc
rocs[["Lasso logistic regression"]]$auc
rocs[["Ridge logistic regression"]]$auc
