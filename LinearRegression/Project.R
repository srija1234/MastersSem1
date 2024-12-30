#FINAL PROJECT

library("ggplot2")
library("GGally")
Cp <- read.csv("F:/Uni classes/Linear regression/Project/Carprice.csv", header=TRUE)
ggpairs(Cp, columns=c('wheelbase','carlength', 'carwidth','carheight','curbweight','enginesize','boreratio',
                        'stroke','compressionratio','horsepower','peakrpm','citympg','highwaympg','price'))
cormatrixf <- cor(Cp[c('wheelbase','carlength', 'carwidth','carheight','curbweight','enginesize','boreratio',
                      'stroke','compressionratio','horsepower','peakrpm','citympg','highwaympg','price')])
cormatrixf


#Assigning variables
Y <- Cp$price
X1 <- Cp$wheelbase
X2 <- Cp$carlength
X3 <- Cp$carwidth
X4 <- Cp$curbweight
X5 <- Cp$enginesize
X6 <- Cp$boreratio
X7 <- Cp$horsepower
X8 <- Cp$citympg
X9 <- Cp$highwaympg
X10 <-Cp$fueltype
X11 <- Cp$aspiration
X12 <- Cp$doornumber
Cp$carbody <- as.factor(Cp$carbody )
X13 <- Cp$carbody
Cp$drivewheel <- as.factor(Cp$drivewheel)
X14 <- Cp$drivewheel
X15 <- Cp$enginelocation
Cp$enginetype <- as.factor(Cp$enginetype)
X16 <- Cp$enginetype
Cp$cylindernumber <- as.factor(Cp$cylindernumber)
X17 <- Cp$cylindernumber
Cp$fuelsystem <- as.factor(Cp$fuelsystem)
X18 <- Cp$fuelsystem

#check correlation on category variables
library("chisquare")
c_t <- table(Cp$fueltype, Cp$fuelsystem)
chisq.test(c_t)

c_t1 <- table(Cp$drivewheel, Cp$enginelocation)
chisq.test(c_t1)

c_t2 <- table(Cp$aspiration, Cp$enginetype)
chisq.test(c_t2)

c_t3 <- table(Cp$carbody, Cp$price)
chisq.test(c_t3)

c_t4 <- table(Cp$fuelsystem, Cp$price)
chisq.test(c_t4)

c_t5 <- table(Cp$fuelsystem, Cp$enginetype)
chisq.test(c_t5)

#polynomial reg assumption??
plot(price ~ highwaympg, data=Cp)
plot(price ~ citympg, data=Cp)

#anova check
catmodel <- lm(Y ~ X10+ X11+X12+X13+X14+X15+X16+X17+X18)
summary(catmodel) #84,82
anova(catmodel)
ggpairs(Cp, columns=c('fueltype','aspiration', 'doornumber','carbody','drivewheel','enginelocation','enginetype',
                      'cylindernumber', 'fuelsystem','price'))

install.packages("DAAG")
library("DAAG")
vif(catmodel)

#interaction terms
X1X2 <- X1* X2
X5X7 <- X5 * X7
X8X9 <- X8 * X9
X10X8 <- X10 * X8 
X10X9 <- X10 * X9
X11X7 <- X11* X7
X15X5 <- X15 * X5

X1X6 <- X1*X6
X1X8 <- X1*X8
#error
X1314 <- X13* X14 
X185 <- X5 *X18
X1013 <- X10 * X13

n<-length(Y)
n

# REG COEFFICINETS ref
# the first column of design matrix X
X0 <- rep(1,n)

# Form the design matrix X (n rows, 4 Cols), ADDED X AS PER MODEL     
X <- as.matrix(cbind(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X15,X16,X17,X18)) 
p <- ncol(X)

# Form the Y-vector (n rows, 1 col)
Y <- as.matrix(Y,ncol=1) 
Y
# Notes: t(X) = transpose of X, %*% = matrix multiplication, solve(A) = A^(-1) 
XtX <- t(X) %*% X 
XtX
XtY <- t(X) %*% Y
XtY

# Obtain (X'X)^(-1) matrix (4 rows, 4 cols)
XXI <- solve(XtX) 

# Obtain least square estimators of beta-vector: b-vector (4 rows, 1 col)
b <- XXI %*% XtY

# Obtain the vector of fitted values (n rows, 1 col)
Y_hat <- X %*% b 
Y_hat
# Obtain the vector of residuals (n rows, 1 col)
e <- Y - Y_hat 
print(cbind(Y_hat,e))

# Obtain the Hat matrix
H <- X %*% XXI %*% t(X) 

# Obtain the (1/n)J matrix (n rows, n cols) 
J_n <- matrix(rep(1/n,n^2),ncol=n) 

# Obtain the identity matrix (n rows, n cols) 
I_n <- diag(n) 

# Compute Analysis of Variance Table
#compute variations
SST <- t(Y) %*% (I_n - J_n) %*% Y
SSE <- t(Y) %*% (I_n - H) %*% Y
SSR <- t(Y) %*% (H - J_n) %*% Y

#Degree of freedoms
df_SSE<- n-p
df_SSR<- p-1
# Obtain Mean Squares
MSE <- SSE/df_SSE 
MSR <- SSR/df_SSR 

Fstar<-MSR/MSE
R2<-1-SSE/SST
R2 #0.86
R2a<-1-(n-1)*(SSE/SST)/(n-p)
R2a #84.9
r<-sqrt(R2)
r #0.928
pvalueF<- pf(65.218,df1=df_SSR,df2=df_SSE,lower=FALSE) #pvalue-> pf
qf(0.95,df1=df_SSR,df2=df_SSE) # ftest-> qf #1.678

#ANOVA table
Reg_ana<-cbind(SSR,df_SSR,MSR,Fstar,pvalueF)
colnames(Reg_ana)<- cbind("SSR","df_R","MSR","F*","p-value")
rownames(Reg_ana)<- cbind("Regression Analysis")
print(Reg_ana)
Error<-cbind(SSE,df_SSE,MSE)
colnames(Error)<- cbind("SSE","df_E","MSE")
rownames(Error)<- cbind("Error")
print(Error)
Total<-cbind(SST,df_SSR+df_SSE)
colnames(Total)<- cbind("SST","df")
rownames(Total)<- cbind("Total")
print(Total)
qf(1-0.05,p-1,n-p) #hypothesis

#INTERACTION model
model_f <- lm(Y ~ X1+X2+X5+X6+X7+X8+X9+X10+X11+X15+X1X2+X5X7+I(X8^2)+I(X9^2)+X8X9+X10X8+X10X9+X11X7+X15X5+X1X6+X1X8)
summary(model_f)
anova(model_f)
#ADDITIVE model
model_r <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18)
summary(model_r)
anova(model_r)

# apply F test to see if we can add interaction terms
anova(model_r,model_f)
qf(1-0.05,(p-1),(n-p)) #more than sig
qf(1-0.15,(p-1),(n-p))

#general model-----
model1 <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18)
summary(model1) #R2-92.4, 90.9
anova(model1)

model2 <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+I(X8^2)+I(X9^2)+X10+X11+X12+X13+X14+X15+X16+X17+X18+
                 X1X2+X5X7+X8X9+X10X8+X10X9+X11X7+X15X5+X1X6+X1X8)
summary(model2) #R2-94.9, 93.4
anova(model2)

#model removing one multicollinearity variables (highwaympg,carlength, carheight )
modelc <- lm(Y ~ X1+X4+X5+X6+X7+X8+I(X8^2)+X10+X11+X12+X13+X14+X15+X16+X17+X18+
               X5X7+X10X8+X11X7+X15X5+X1X6+X1X8)
summary(modelc) #R2- 94, 93  - not much difference from general
anova(modelc)

#Model selection----
install.packages("leaps") 
library(leaps)
Model1l <- regsubsets(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18,nbest=2,data=Cp)
Leaps1l<- summary(Model1l)
with(Leaps1l,round(cbind(which,rsq,adjr2,cp,bic),3)) #warning-linear depend

Model12 <- regsubsets(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+I(X8^2)+I(X9^2)+X10+X11+X12+X13+X14+X15+X16+X17+X18+
                        X1X2+X5X7+X8X9+X10X8+X10X9+X11X7+X15X5+X1X6+X1X8,nbest=2,data=Cp)
Leaps12<- summary(Model12)
with(Leaps12,round(cbind(which,rsq,adjr2,cp,bic),3)) #2 best models


#AUTOMATIC REG PROCEDURE
Regfull1 <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18,data=Cp)
summary(Regfull1)
anova(Regfull1)
drop1(Regfull1)

backward_reg1 <- step(Regfull1,direction="backward",k=log(n)) #AIC=3222.73
summary(backward_reg1) #R2=90.6, 89.8
Regnull1 <- lm(Y ~ 1,data=Cp)
forward_reg1 <- step(Regnull1 ,direction="forward",scope=list(upper=Regfull1,lower=Regnull1)) #AIC=3155.47
summary(forward_reg1) #R2=92.2, 90.9

#automatic reg procedure WITH INTERACTION TERMS
Regfull2 <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+I(X8^2)+I(X9^2)+X10+X11+X12+X13+X14+X15+X16+X17+X18+
                 X1X2+X5X7+X8X9+X10X8+X10X9+X11X7+X15X5+X1X6+X1X8,data=Cp)
summary(Regfull2)
anova(Regfull2)
drop1(Regfull2)

backward_reg2 <- step(Regfull2,direction="backward",k=log(n)) #AIC=3192.81
summary(backward_reg2) #R2=93, 92
Regnull2 <- lm(Y ~ 1,data=Cp)
forward_reg2 <- step(Regnull2 ,direction="forward",scope=list(upper=Regfull2,lower=Regnull2)) #AIC=3096.07
summary(forward_reg2) #R2=94, 93
anova(forward_reg2)

# FULL MODEL(for forward_reg2 )
Fullmodel <- lm(Y~ X1+X3+X4+X5+X6+X7+X8+X9+X10+X13+X14+X15+X16+X17+X18+ X1X6+X1X8+X5X7+I(X8^2)+X10X9)
summary(Fullmodel) #R2=94.6, 93.3
anova(Fullmodel)
vif(Fullmodel)


#MODEL VALIDATION
options(max.print=1000000)
set.seed(2023) 
attach(Cp)
Indices <- sample(1:length(price),151,replace=FALSE)  #75% for 201obs
Cp.train <- Cp[Indices,]
Cp.Validation <- Cp[-Indices,]
Cp.train 

Train.Model <- lm(price~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype
                  + carbody+ drivewheel + enginelocation+ enginetype+
                    (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype),  data=Cp.train)

Validation.Model <- lm(price~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype
                       + carbody+ drivewheel + enginelocation+ enginetype+
                         (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype), data=Cp.Validation)

summary(Train.Model) #R2=92.9  91.5
summary(Validation.Model) #R2=94.9 ,90.8

####Mean Squared Prediction Error 
Y_hat_Validation<-predict(Train.Model,Cp.Validation)
length(Cp.train$wheelbase)
MSPR<-sum((Cp.Validation$price-Y_hat_Validation)^2)/length(Cp.Validation$wheelbase)
#Mean of train data set
MSE_train<-sum((Cp.train$price-predict(Train.Model,Cp.train))^2)/(length(Cp.train$wheelbase)-3)
MSPR 
MSE_train 
sqrt(MSPR/MSE)  #1.508


#OUTLIER DIAGNOSTICS
plot(enginetype, xlab="Tye of engine", ylab="price", title="EngineType Vs CarPrice")
boxplot(Cp$fuelsystem, main = "fuel system", horizontal = TRUE)
boxplot(Cp$enginetype, main = "Type of engine", horizontal = TRUE)

#continue from reg coeff
MSE[1.1] 
diag(H)
#Semistudentized residuals 
e_star<-e/sqrt(MSE[1,1])
#studentized residuals 
r<-e/sqrt(MSE[1,1]*(1-diag(H)))
#Deleted Residuals
d<-e/(1-diag(H))
#Deleted Studentized Residuals
t<-e*sqrt((n-p-1)/(SSE[1,1]*(1-diag(H))-e^2))
t
alpha<-0.05
qt(1-alpha/(2*n),n-p-1) #3.73

#table of all
table<-cbind(X1,X3,X4,X5,X6,X7,X8,X9,X10,X13,X14,X15,X16,X17,X18, X1X6,X1X8,X5X7,I(X8^2),X10X9,
             Y,Y_hat,e,e_star,r,d,abs(t)-qt(1-alpha/(2*n),n-p-1))
colnames(table)<- cbind("X1","X3","X4","X5","X6","X7","X8", "X9","X10","X13","X14","X15","X16","X17","X18",
                         "X1*X6","X1*X8","X5*X7","X8*X8","X10*X9","Y","Y_hat","e","Semistudentized e_star","Studentized r","Deleted Residuals d","|t|-t(1-alpha/2n;n-p-1)")
print(table,quote=FALSE) #no outlying Y observation

#Hat matrix and leverage- for outlying X observation
Leverage_test<-diag(H)-(2*p/n)*X0 
LT<-cbind(X1,X3,X4,X5,X6,X7,X8,X9,X10,X13,X14,X15,X16,X17,X18, X1X6,X1X8,X5X7,I(X8^2),X10X9,Y,diag(H),Leverage_test)
colnames(LT)<- cbind("X1","X3","X4","X5","X6","X7","X8", "X9","X10","X13","X14","X15","X16","X17","X18",
                     "X1*X6","X1*X8","X5*X7","X8*X8","X10*X9","Y","h_ii","h_ii-2p/n")
print(LT) 
#hii>(2*p)/n  #0.179 - many obs

# influential cases DFFITS & Cook's distance
DFFITS_i<-t*sqrt(diag(H)/(1-diag(H)))
D_i<-e^2*diag(H)/(p*MSE[1,1]*(1-diag(H))^2)
Influe_table<-cbind(Y_hat,e,DFFITS_i,D_i,D_i-qf(0.5,p,n-p))
colnames(Influe_table)<-cbind("Y_hat","e_i","(DFFITS)_i","D_i","D_i-F(0.5;p,n-p)")
print(Influe_table)
hist(D_i)
qf(0.5,p,n-p)
#Di>f()  = 0.9667

#influential cases DFBETAS and Cook's distance(same as ful mo)
Modelic <- lm(Y~ X1+X3+X4+X5+X6+X7+X8+X9+X10+X13+X14+X15+X16+X17+X18+ X1X6+X1X8+X5X7+I(X8^2)+X10X9)
summary(Modelic) #R2=94.6, 93.3

plot(Modelic)
dfbetas(Modelic)

#plot 
install.packages("olsrr")
library("olsrr")
ols_plot_cooksd_bar(Modelic)
ols_plot_dfbetas(Modelic)

#infleuntial points
influence.measures(Modelic)
influential <- which(cooks.distance(Modelic) > 0.02) #(2/sqrt(n))
Cp[influential, ]
#20 observations

#boxcox transformation test
model.inv <- lm((Y)^-1 ~ X1+ X5 + X9+ X10+ X17 + X16 + X5X7 + X4 + X13 + X18 + X14 + X7 + X3 + 
                  X1X8 + X10X9 + X15 + X1X6 + X8X9 + X6 + X10X8 + X8, data = Cp ) #works with inv
summary(model.inv)

#Y~ X1+X3+X4+X5+X6+X7+X8+X9+X10+X13+X14+X15+X16+X17+X18+X1X6+X1X8+X5X7+I(X8^2)+X10X9
#REMOVING OUTLIERS
nrow(Cp)
Cp_c <- Cp[-influential, ]
nrow(Cp_c) #181
Yc <- Cp_c$price
X1c <- Cp_c$wheelbase
X3c <- Cp_c$carwidth
X4c <- Cp_c$curbweight
X5c <- Cp_c$enginesize
X6c <- Cp_c$boreratio
X7c <- Cp_c$horsepower
X8c <- Cp_c$citympg
X9c <- Cp_c$highwaympg
X10c <-Cp_c$fueltype

Cp_c$carbody <- as.factor(Cp_c$carbody )
X13c <- Cp_c$carbody
Cp_c$drivewheel <- as.factor(Cp_c$drivewheel)
X14c <- Cp_c$drivewheel
X15c <- Cp_c$enginelocation
Cp_c$enginetype <- as.factor(Cp_c$enginetype)
X16c <- Cp_c$enginetype
Cp_c$cylindernumber <- as.factor(Cp_c$cylindernumber)
X17c <- Cp_c$cylindernumber
Cp_c$fuelsystem <- as.factor(Cp_c$fuelsystem)
X18c <- Cp_c$fuelsystem

#interaction terms
X5X7c <- X5c * X7c
X1X8c <- X1c*X8c
X1X6c <- X1c*X6c
X8X8c <- X8c*X8c
X10X9c <- X10c * X9c
X8X9c <- X8c * X9c
X10X8c <- X10c * X8c 

# Fit the regression model again with the cleaned dataset
model_c<- lm(Yc~ X1c+X3c+X4c+X5c+X6c+X7c+X8c+X9c+X10c+X13c+X14c+X15c+X16c+X17c+X18c+ 
               X1X6c+X1X8c+X5X7c+X8X8c+X10X9c , data = Cp_c)
summary(model_c) #96.3, 95.3
plot(model_c)
anova(model_c)


#BREUSCH PAGAN TEST to check for heteroscedasticity
library(zoo)
library(lmtest)
bptest(model.inv) #old dataset p>0.05
bptest(model_c) 
bptest(model_c, studentize=FALSE)  #new dataset p<0.05
#P less than alpha
chi2_val <- qchisq(0.95, 1)
chi2_val

#1. handle heteroscedasticity-log transform
model_clog <- lm((log(Yc))~ X1c+ X3c +X4c+ X5c+ X6c+ X7c +X8c +X9c +X10c +X13c +X14c +X15c +X16c +X17c +X18c+ 
                   X1X6c +X1X8c +X5X7c +X8X8c +X10X9c , data = Cp_c)
bptest(model_clog, studentize = FALSE) 
#p=0.2 > than 0.05 so homo
summary(model_clog) #R2=95, 93.9
plot(model_clog)

#2. boxcox transformation
library(MASS)
boxcox(model_c)
bc <- boxcox(model_c, lambda = seq(-2, 2, by = 0.1))
lambda <- bc$x[which(bc$y==max(bc$y))]
lambda
# lambda- 0.06
model.invc <- lm((Yc)^-1 ~ X1c+ X3c +X4c+ X5c+ X6c+ X7c +X8c +X9c +X10c +X13c +X14c +X15c +X16c +X17c +X18c+ 
                   X1X6c +X1X8c +X5X7c +X8X8c +X10X9c  , data = Cp_c)
plot(model.invc)
bptest(model.invc)
summary(model.invc) #R2=92.5, 90.6
#p=0.32 > 0.05

#OR
new_model <- lm(((Y^lambda-1)/lambda) ~ X1c+ X3c +X4c+ X5c+ X6c+ X7c +X8c +X9c +X10c +X13c +X14c +X15c +X16c +X17c +X18c+ 
                  X1X6c +X1X8c +X5X7c +X8X8c +X10X9c) #still same
plot(new_model)


#MULTICOLLINEARITY check vif
vif(Fullmodel) #not recommended
fmv <- lm(Y~ X1+ X5 + X9+ X10+ X17 + X16 + X4 + X13 + X18 + X14 + X7 + X3 + X15 +  X6 + X8)
vif(fmv)

fmv1 <- lm(Y~ X1+X3+X4+X5+X6+X7+X8+X9)
vif(fmv1)

cormatrix <- cor(Cp[c("wheelbase","carwidth","curbweight","enginesize","boreratio","horsepower","citympg","highwaympg")])
cormatrix

fmv2 <- lm(Y~ X4+X5+X6+X8+X9)
vif(fmv2)

fmv2e <- lm(price ~ wheelbase+ carwidth +curbweight + enginesize+boreratio+horsepower+ citympg+highwaympg, data=Cp_c)
vif(fmv2e)
#effects verified in other file

# removed X8X9c interaction term---
#Ridge regression on cleaned dataset with transformation
install.packages("glmnet")
library(glmnet)
x <- model.matrix((log(Yc))~ X1c+ X3c +X4c+ X5c+ X6c+ X7c +X8c +X9c +X10c +X13c +X14c +X15c +X16c +X17c +X18c+ 
                    X1X6c +X1X8c +X5X7c +X8X8c +X10X9c , data = Cp_c)[, -1]
y <- log(Cp_c$price)
# Standardizing predictors
X_scaled <- scale(x)

# Fit Ridge regression model(ridge+transform)
ridge_model <- glmnet(X_scaled , y, alpha = 0)  # alpha = 0 for Ridge
plot(ridge_model, main="Ridegmodel")
# Cross-validation for optimal lambda
cv_ridge <- cv.glmnet(X_scaled , y, alpha = 0)
best_lambda <- cv_ridge$lambda.min
print(best_lambda) 
# lambda= 0.06
plot(cv_ridge)
# Fit Ridge model with optimal lambda
ridge_final <- glmnet(X_scaled, y, alpha = 0, lambda = best_lambda)
coef(ridge_final)

# Predict on the same dataset 
predicts <- predict(ridge_final, newx = X_scaled)
sst <- sum((y - mean(y))^2)
sse <- sum((y - predicts)^2)
r_squared <- 1 - (sse / sst) #0.938
sst
sse
r_squared

#Ridge regression  split with train & test
set.seed(123) 
train_indices <- sample(1:nrow(Cp_c), size = 0.75 * nrow(Cp_c))
train_data <- Cp_c[train_indices, ]
test_data <- Cp_c[-train_indices, ]
x_train <- model.matrix(price~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype + carbody+ drivewheel + enginelocation+ enginetype+
                          (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype)+ cylindernumber+ fuelsystem ,data = train_data)
y_train <- train_data$price
# Fit ridge regression (alpha = 0 for ridge)
ridge_modelt <- glmnet(x_train, y_train, alpha = 0)
cv_ridget <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambdat <- cv_ridget$lambda.min  # Optimal lambda value
#best lambdat= 614.74
plot(cv_ridget)

# Final Ridge Model with Optimal Lambda
Ridge_final<- glmnet(x_train, y_train, alpha = 0, lambda = best_lambdat)
coef(Ridge_final)
predictions_train <- predict(Ridge_final, s = best_lambdat, newx = x_train)
#metrics
mse_train <- mean((y_train - predictions_train)^2)
mae_train <- mean(abs(y_train - predictions_train))
sst_train <- sum((y_train - mean(y_train))^2)
sse_train <- sum((y_train - predictions_train)^2)
R_squaredtrain <- 1 - (sse_train/ sst_train)

cat("Total sum of squares (train):", sst_train, "\n")
cat("Error sum of squares (train):", sse_train, "\n")
cat("Mean Absolute Error (train):", mae_train, "\n")
cat("R-squared (train):", R_squaredtrain, "\n") 

# Calculate RMSE
rmse_train <- sqrt(mean((y_train - predictions_train)^2))
cat("RMSE (train):", rmse_train, "\n")


#model on test_data 
x_test <- model.matrix(price~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype + carbody+ drivewheel + enginelocation+ enginetype+
                         (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype)+ cylindernumber+ fuelsystem, data = test_data)
y_test <- test_data$price
predictions_test <- predict(Ridge_final, s = best_lambdat, newx = x_test)
#metrics
mse_test <- mean((y_test - predictions_test)^2)
mae_test <- mean(abs(y_test - predictions_test))
sst_test <- sum((y_test - mean(y_test))^2)
sse_test <- sum((y_test - predictions_test)^2)
R_squaredtest <- 1 - (sse_test / sst_test )

cat("Total sum of squares (test):", sst_test, "\n")
cat("Error sum of squares (test):", sse_test, "\n")
cat("Mean Absolute Error (test):", mae_test, "\n")
cat("R-squared (test):", R_squaredtest, "\n") 
# Calculate RMSE
rmse_test <- sqrt(mean((y_test - predictions_test)^2))
cat("RMSE (test):", rmse_test, "\n") 

#plot residual vs predict
plot(y_test, predictions_test, main = "Actual vs Predicted",
     xlab = "Actual Values", ylab = "Predicted Values")
abline(0, 1, col = "red")  # Ideal prediction line

residuals <- y_test - predictions
qqnorm(residuals)
qqline(residuals)


# MODEL VAL TRain & test split(validation on cleaned)
options(max.print=1000000)
set.seed(2023) 
nrow(Cp_c)
attach(Cp_c)
Indices <- sample(1:length(price),135,replace=FALSE)  #75% for 179obs
Cp.trainc <- Cp_c[Indices,]
Cp.Validationc <- Cp_c[-Indices,]
Cp.trainc

#X1c+ X5c + X9c+ X10c+ X17c + X16c + X5X7c + X4c + X13c + X18c + X14c + X7c + X3c + X1X8c + X10X9c + X15c + X1X6c + X6c + X8c+ X10X8c
Train.Modelc <- lm(log(price)~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype + carbody+ drivewheel + enginelocation+ enginetype+
                     (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype)+ cylindernumber+ fuelsystem, data=Cp.trainc)

Validation.Modelc <- lm(log(price)~ wheelbase + carwidth+ curbweight+ enginesize+boreratio+horsepower+citympg+highwaympg+fueltype + carbody+ drivewheel + enginelocation+ enginetype+
                          (wheelbase*boreratio)+(wheelbase*citympg)+(enginesize*horsepower)+(citympg*citympg)+(highwaympg*fueltype)+ cylindernumber+ fuelsystem, data=Cp.Validationc)
summary(Train.Modelc) #R2=96.3, 94.9
summary(Validation.Modelc) #R2=97.5, 93.4

####Mean Squared Prediction Error 
Y_hat_Validationc<-predict(Train.Modelc,Cp.Validationc)
length(Cp.trainc$wheelbase)
MSPR<-sum((Cp.Validationc$price-Y_hat_Validationc)^2)/length(Cp.Validationc$wheelbase)
#Mean of train data set
MSE<-sum((Cp.trainc$price-predict(Train.Modelc,Cp.trainc))^2)/(length(Cp.trainc$wheelbase)-3)
MSPR 
MSE 
sqrt(MSPR/MSE) 

n_c <- length(Yc)

#Automatic procedure on cleand dataset
Regfullc <- lm((log(Yc))~ X1c+ X3c +X4c+ X5c+ X6c+ X7c +X8c +X9c +X10c +X13c +X14c +X15c +X16c +X17c +X18c+ 
                 X1X6c +X1X8c +X5X7c +X8X8c +X10X9c ,data=Cp_c)
summary(Regfullc)
anova(Regfullc)
drop1(Regfullc)

backward_regc <- step(Regfullc,direction="backward",k=log(n_c)) #AIC=-679.77
summary(backward_regc) #R2=94, 93
Regnullc <- lm(Yc ~ 1,data=Cp_c)
forward_regc <- step(Regnullc ,direction="forward",scope=list(upper=Regfullc,lower=Regnullc)) #AIC=2684.26
summary(forward_regc) #R2=96,95


#Model
#final model with cleaned & backward
model_finalcf<- lm((log(Yc))~ X4c+ X5c+ X7c +X13c +X15c +X16c +X17c +X1X6c +X10X9c , data = Cp_c)
summary(model_finalcf) #R2=94, 93.5
anova(model_finalcf)

#LASSO regualrization 
lasso_model <- cv.glmnet(x_train, y_train, alpha=1)
lambda_l <- lasso_model$lambda.min #148.7
final_lasso <- glmnet(x_train, y_train, alpha=1, lambda = lambda_l)
coef_l <- coef(final_lasso, s=lambda_l)
print(coef_l)
plot(lasso_model)

predictions_testl <- predict(final_lasso, s = lambda_l, newx = x_test)
final <- cbind(y_test, predictions_testl)
head(final)
mse_testl <- mean((y_test - predictions_testl)^2)
mae_testl <- mean(abs(y_test - predictions_testl))
sst_testl <- sum((y_test - mean(y_test))^2)
sse_testl <- sum((y_test - predictions_testl)^2)
R_squaredtestl <- 1 - (sse_testl / sst_testl )
cat("R-squared (test) with Lasso:", R_squaredtestl, "\n") 




