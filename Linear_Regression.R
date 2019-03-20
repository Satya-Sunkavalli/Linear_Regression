head(blogData_train)
head(blogData_test.2012.02.14.00_00)

#Creating basic features attributes i.e for Experiment 1 
tdata_exp1 <- blogData_train[,c(51:60,281)]
head(tdata_exp1)
vdata_exp1 <- blogData_test.2012.02.14.00_00[,c(51:60,281)]
head(vdata_exp1)

#Multiple Regression model
results_exp1 <- lm(V281~ .,  data  =tdata_exp1)
sm_exp1 <- summary(results_exp1)
results_exp1$coefficients
options(warn = -1)

#Prediction
pred_exp1 <- predict(results_exp1,vdata_exp1)
mse_exp1 <- mean((vdata_exp1$V281 - predict(results_exp1, vdata_exp1)) ^ 2)
print(mse_exp1)
head(pred_exp1)
head(vdata_exp1)

#Creating basic features attributes i.e for Experiment 1 
tdata_exp2 <- blogData_train[,c(63:262,281)]
vdata_exp2 <- blogData_test.2012.02.14.00_00[,c(63:262,281)]
head(tdata_exp2)
head(vdata_exp2)
#Multiple Regression model
results_exp2 <- lm(V281~ .,  data  =tdata_exp2)
sm_exp2 <- summary(results_exp2)
results_exp2$coefficients
options(warn = -1)

#Prediction
pred_exp2 <- predict(results_exp2,vdata_exp2)
mse_exp2 <- mean((vdata_exp2$V281 - predict(results_exp2, vdata_exp2)) ^ 2)
print(mse_exp2)
head(pred_exp2)
head(vdata_exp2)

