library(dplyr)
library(ggpubr)
library(caTools)
df <- read.csv("D:/Data Analytics/Sem 3/DANA-4820/Project/heart_failure_clinical_records_dataset.csv")
View(df)

str(df)
summary(df)

#2
#Thirteen (13) clinical features:
  
#1. age: age of the patient (years)
#2.  anaemia: decrease of red blood cells or hemoglobin (boolean)
#3. high blood pressure: if the patient has hypertension (boolean)
#4.creatinine phosphokinase (CPK): level of the CPK enzyme in the blood (mcg/L)
#5. diabetes: if the patient has diabetes (boolean)
#6. ejection fraction: percentage of blood leaving the heart at each contraction (percentage)
#7. platelets: platelets in the blood (kiloplatelets/mL)
#8. sex: woman or man (binary)
#9. serum creatinine: level of serum creatinine in the blood (mg/dL)
#10. serum sodium: level of serum sodium in the blood (mEq/L)
#11. smoking: if the patient smokes or not (boolean)
#12. time: follow-up period (days)
#13. [target] death event: if the patient deceased during the follow-up period (boolean)

head(df)
colnames(df)
str(df)
colSums(is.na(df))

#3
df$anaemia<-as.factor(df$anaemia)
df$diabetes<-as.factor(df$diabetes)
df$high_blood_pressure<-as.factor(df$high_blood_pressure)
df$sex<-as.factor(df$sex)
df$smoking<-as.factor(df$smoking)
df$DEATH_EVENT<-as.factor(df$DEATH_EVENT)
#Round off AGE
df$age<-round(df$age)

#4
df%>%
group_by(sex) %>%
  get_summary_stats(age, type = "mean_sd")


# Save the data in two different vector
#T-Test
#1. Age mean comparison in Two groups

no_death_age <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(age)

death_age <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(age)

# Compute t-test
var.test(no_death_age, death_age)
#variances are not equal
t.test(no_death_age, death_age,var.equal = F)

#2. phosphokinase mean comparison in Two groups

no_death_phosphokinase <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(creatinine_phosphokinase)

death_phosphokinase <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(creatinine_phosphokinase)

# Compute t-test
var.test(no_death_phosphokinase, death_phosphokinase)
#variances are not equal
t.test(no_death_phosphokinase, death_phosphokinase,var.equal = F)

#3. ejection_fraction mean comparison in Two groups

no_death_ejection_fraction <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(ejection_fraction)

death_ejection_fraction <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(ejection_fraction)

# Compute t-test
var.test(no_death_ejection_fraction, death_ejection_fraction)
#variances are not equal
t.test(no_death_ejection_fraction, death_ejection_fraction,var.equal = F)

#4. platelets mean comparison in Two groups

no_death_platelets <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(platelets)

death_platelets <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(platelets)

# Compute t-test
var.test(no_death_platelets, death_platelets)
#variances are not equal
t.test(no_death_platelets, death_platelets,var.equal = F)

#5. serum_creatinine mean comparison in Two groups

no_death_serum_creatinine <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(serum_creatinine)

death_serum_creatinine <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(serum_creatinine)

# Compute t-test
var.test(no_death_serum_creatinine, death_serum_creatinine)
#variances are not equal
t.test(no_death_serum_creatinine, death_serum_creatinine,var.equal = F)

#6. serum_sodium mean comparison in Two groups

no_death_serum_sodium <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(serum_sodium)

death_serum_sodium <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(serum_sodium)

# Compute t-test
var.test(no_death_serum_sodium, death_serum_sodium)
#variances are not equal
t.test(no_death_serum_sodium, death_serum_sodium,var.equal = F)

#7. time mean comparison in Two groups

no_death_time <- df %>%
  filter(DEATH_EVENT == 0) %>%
  pull(time)

death_time <- df %>%
  filter(DEATH_EVENT == 1) %>%
  pull(time)

# Compute t-test
var.test(no_death_time, death_time)
#variances are not equal
t.test(no_death_time, death_time,var.equal = F)

#5
#Chi-Square Test
chi_diabetes_death <- chisq.test(table(df$DEATH_EVENT, df$diabetes))
chi_diabetes_death

chi_anemia_death <- chisq.test(table(df$DEATH_EVENT, df$anaemia))
chi_anemia_death

chi_smoking_death <- chisq.test(table(df$DEATH_EVENT, df$smoking))
chi_smoking_death

chi_bloodP_death <- chisq.test(table(df$DEATH_EVENT, df$high_blood_pressure))
chi_bloodP_death

chi_sex_death <- chisq.test(table(df$DEATH_EVENT, df$sex))
chi_sex_death



#6
with(df,{interaction.plot(x.factor=age,
                 trace.factor = sex,
                 response = ejection_fraction,
                 fun=mean,trace.label = "GENDER",col = c("red","black"))})

with(df,{interaction.plot(x.factor=age,
                          trace.factor = sex,
                          response = serum_creatinine ,
                          fun=mean,trace.label = "GENDER",col = c("red","black"))})

with(df,{interaction.plot(x.factor=age,
                          trace.factor = sex,
                          response = creatinine_phosphokinase ,
                          fun=mean,trace.label = "GENDER",col = c("red","black"))})

with(df,{interaction.plot(x.factor=age,
                          trace.factor = sex,
                          response = time ,
                          fun=mean,trace.label = "GENDER",col = c("red","black"))})



#with(df,{plsmo(age, DEATH_EVENT, datadensity = T, group = sex, 
#      col=c('black', 'red'), xlab = 'AGE', ylab ='DEATH EVENT',
#     ylim = c(0,1))})

#7 
set.seed(100)
sample_df<-sample.split(df,SplitRatio = 0.70)
train<-subset(df,sample_df==T)
test<-subset(df,sample_df==F)
nrow(train)
nrow(test)

#8
library(MASS)
fit<-glm(DEATH_EVENT~.,family = binomial,data=train)
summary(fit)
stepAIC(fit)

#Reduced Model-No interaction term
fit2<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+sex+time+creatinine_phosphokinase,family = binomial,data=train)
summary(fit2)
#Full Model-With Interaction term
fit3<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+sex+creatinine_phosphokinase+time+age*ejection_fraction+
            age*serum_creatinine+age*serum_sodium+age*time,family = binomial,data=train)
summary(fit3)

anova(fit2,fit3,test="LRT")

fit4<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+sex+creatinine_phosphokinase+time+age*ejection_fraction,family = binomial,data=train)
summary(fit4)

anova(fit2,fit4,test='LRT')

fit5<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+creatinine_phosphokinase+time+age*serum_creatinine,family = binomial,data=train)
summary(fit5)

anova(fit2,fit5,test='LRT')

fit6<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+creatinine_phosphokinase+time+age*creatinine_phosphokinase,family = binomial,data=train)
summary(fit6)

anova(fit6,fit2,test='LRT')

fit7<-glm(DEATH_EVENT~age+ejection_fraction+serum_creatinine+creatinine_phosphokinase+time+
            age*time,family = binomial,data=train)
summary(fit7)
anova(fit2,fit7,test='LRT')

library(lmtest)
waldtest(fit2,test = "Chisq")

# confusion matrix
table_mat <- table(df$DEATH_EVENT, predict > 0.5)
table_mat
library(ROCR)
predict2 <- predict(fit2, test, type = 'response')
predict2
pred<- prediction(predict2, test$DEATH_EVENT)
roc<- performance(pred, "tpr", "fpr")
plot(roc)


abline(a=0, b=1)

plot(roc, Main="ROC Curve", ylab="Sensitivity", xlab="1-Specificity")

abline(a=0, b=1)


#Area Under Curve (AUC)
auc<- performance (pred, "auc")
auc
unlist(slot(auc,"y.values"))
# correct classification=.75
sum(diag(table_mat))/sum(table_mat)
1-sum(diag(table_mat))/sum(table_mat)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
library(pROC)
#logistic regression model

#multi-collinearity

formula<-df$DEATH_EVENT~.
model<-fit2
summary(model)
vif<-vif(model)
vif
#All variables are significant with no multi-collinearity according to the vif values.

