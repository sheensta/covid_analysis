# load the packages
library(dplyr)
library(ggplot2)
library(readr)
library('plot.matrix')
library(caret)
library(car)
library(pscl)
library(ROCR)
library(randomForest)
library(caTools)

# load in the csv file
covid <- read_csv("covid.csv")

# filter out non covid positive cases
covid <- covid %>%
  filter(covid_res == 1)

# check for duplicates
covid_distinct <- covid %>% distinct()

# 0x23 means the dataframes are the same
setdiff(covid,covid_distinct)

# check for missing values
#library(Amelia)
#missmap(covid)
attach(covid)

# summary of data
summary(covid)

# Age outliers will be imputed to mean + 3SD
mean_age <- mean(covid$age)
sd_age <- sd(covid$age)
imputed_age <- round(mean_age + 3*sd_age)
covid$age[covid$age > imputed_age] = imputed_age

# convert sex to 1 and 0 (0 is male, 1 is female)
covid$sex[covid$sex == 2] <- 0

# complete case analysis
# filter out incomplete cases
covid_complete_case <- covid %>%
  filter((pneumonia == 1 | pneumonia == 2) & (pregnancy == 1 | pregnancy == 2 | pregnancy == 97) &
           (diabetes == 1 | diabetes == 2) & (copd == 1 | copd == 2) & (asthma == 1 | asthma ==2) &
           (inmsupr == 1 | inmsupr == 2) & (hypertension == 1 | hypertension == 2) &
           (other_disease == 1 | other_disease == 2) & (cardiovascular == 1 | cardiovascular == 2) &
           (obesity == 1 | obesity == 2) & (renal_chronic == 1 | renal_chronic == 2) & (tobacco == 1 | tobacco == 2))

# add column for death factor
attach(covid_complete_case)
covid_complete_case <- covid_complete_case %>%
  mutate(deathyn = ifelse(date_died == '9999-99-99', '0','1'))

# convert sex and pre-existing conditions to 0 and 1
covid$sex[covid$sex == 2] <- 0
covid_complete_case$intubed <- ifelse(covid_complete_case$intubed == 1,1,0)
covid_complete_case$pneumonia[covid_complete_case$pneumonia == 2] <- 0
covid_complete_case$pregnancy[covid_complete_case$pregnancy == 2] <- 0
covid_complete_case$diabetes[covid_complete_case$diabetes == 2] <- 0
covid_complete_case$copd[covid_complete_case$copd == 2] <- 0
covid_complete_case$asthma[covid_complete_case$asthma == 2] <- 0
covid_complete_case$inmsupr[covid_complete_case$inmsupr == 2] <- 0
covid_complete_case$hypertension[covid_complete_case$hypertension == 2] <- 0
covid_complete_case$other_disease[covid_complete_case$other_disease == 2] <- 0
covid_complete_case$cardiovascular[covid_complete_case$cardiovascular == 2] <- 0
covid_complete_case$obesity[covid_complete_case$obesity == 2] <- 0
covid_complete_case$renal_chronic[covid_complete_case$renal_chronic == 2] <- 0
covid_complete_case$tobacco[covid_complete_case$tobacco == 2] <- 0

# graph the data after cleaning
p1 <- ggplot(covid_complete_case,aes(x=sex, fill=deathyn)) + geom_bar(position='fill')
p2 <- ggplot(covid_complete_case,aes(x=pneumonia, fill=deathyn)) + geom_bar(position='fill')
p3 <- ggplot(covid_complete_case,aes(x=diabetes, fill=deathyn)) + geom_bar(position='fill')
p4 <- ggplot(covid_complete_case,aes(x=copd, fill=deathyn)) + geom_bar(position='fill')
p5 <- ggplot(covid_complete_case,aes(x=asthma, fill=deathyn)) + geom_bar(position='fill')
p6 <- ggplot(covid_complete_case,aes(x=inmsupr, fill=deathyn)) + geom_bar(position='fill')
p7 <- ggplot(covid_complete_case,aes(x=hypertension, fill=deathyn)) + geom_bar(position='fill')
p8 <- ggplot(covid_complete_case,aes(x=other_disease, fill=deathyn)) + geom_bar(position='fill')
p9 <- ggplot(covid_complete_case,aes(x=cardiovascular, fill=deathyn)) + geom_bar(position='fill')
p10 <- ggplot(covid_complete_case,aes(x=obesity, fill=deathyn)) + geom_bar(position='fill')
p11 <- ggplot(covid_complete_case,aes(x=renal_chronic, fill=deathyn)) + geom_bar(position='fill')
p12 <- ggplot(covid_complete_case,aes(x=tobacco, fill=deathyn)) + geom_bar(position='fill')
p13 <- ggplot(covid_complete_case,aes(x=intubed, fill=deathyn)) + geom_bar(position='fill')
p14 <- ggplot(covid_complete_case,aes(x=contact_other_covid, fill=deathyn)) + geom_bar(position='fill')
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13, p14)
factor_columns <- c('sex','pneumonia', 'pregnancy', 'diabetes', 'copd', 'asthma', 'inmsupr', 'hypertension', 'other_disease', 'cardiovascular', 'obesity', 'renal_chronic', 'tobacco', 'deathyn')
covid_complete_case[factor_columns] <- lapply(covid_complete_case[factor_columns], factor)

# chi squared test for each pair of pre-existing conditions
conditions <- list(pneumonia, diabetes, copd, asthma, inmsupr, hypertension, other_disease, cardiovascular, obesity, renal_chronic, tobacco)
conditions_as_strings <- list('pneumonia', 'diabetes', 'copd', 'asthma', 'inmsupr', 'hypertension', 'other_disease', 'cardiovascular', 'obesity', 'renal_chronic', 'tobacco')
countx <- 0
conditions_matrix <- matrix(NA,11,11,byrow=TRUE)
rownames(conditions_matrix) <- conditions_as_strings
colnames(conditions_matrix) <- conditions_as_strings
for (conditionx in conditions) {
  countx <- countx + 1
  county <- 1
  for (conditiony in conditions) {
    t <- table(conditionx, conditiony)
    chi <- chisq.test(t)
    chi_statistic <- (chi$statistic/length(conditionx))^0.5 %>% round(4)
    if (chi_statistic < 0.5) {
      conditions_matrix[countx,county] <- chi_statistic
      county <- county +1
    } else {
      conditions_matrix[countx,county] <- "NA"
      county <- county + 1
    }
  }
}
# plot the matrix
plot(conditions_matrix, digits=4, axis.col=list(side=3, cex.axis=0.7), axis.row=list(cex.axis=0.7))
#correlation between tobacco and most other conditions

###LOGISTIC REGRESSION MODEL
set.seed(123)
# select the columns needed for modeling
covid_complete_case_modeling <- covid_complete_case %>% select(2,8:20,24)
# convert the categorical variables to factors
factor_columns <- c('sex','pneumonia', 'pregnancy', 'diabetes', 'copd', 'asthma', 'inmsupr', 'hypertension', 'other_disease', 'cardiovascular', 'obesity', 'renal_chronic', 'tobacco', 'deathyn')
covid_complete_case_modeling[factor_columns] <- lapply(covid_complete_case[factor_columns], factor)

# create the training and testing data
sample_size <- round(0.75*nrow(covid_complete_case_modeling))
train_index <- sample(seq_len(nrow(covid_complete_case_modeling)), size = sample_size)
train_data <- covid_complete_case_modeling[train_index, ]
test_data <- covid_complete_case_modeling[-train_index, ]

# fit the model
logistic_regression_model_1 <- glm(deathyn ~.,family=binomial(link='logit'),data=train_data)

# summary() will provide the details of the model
summary(logistic_regression_model_1)
# shows that pregnancy and cardiovascular are insignificant (p.values > 0.05)
# anova() analyzes the table of deviance
anova(logistic_regression_model_1, test="Chisq")

# use the McFadden R^2 to asses the model fit
pR2(logistic_regression_model_1)

# Assess the model's ability to predict death
fitted.results <- predict(logistic_regression_model_1, new_data = subset(test_data,select=c(1:14)))
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClassificError <- mean(fitted.results != test_data$deathyn)
print(paste('Accuracy', 1-misClassificError))

# check ROC curve and area under the curve
p_1 <- predict(logistic_regression_model_1, newdata = subset(test_data, select=c(1:14)),type='response')
pr_1 <- prediction(p_1, test_data$deathyn)
performance_1 <- performance(pr_1, measure = 'tpr', x.measure = 'fpr')
sensitivity_1 <- performance(pr_1, 'sens', 'spec')
plot(performance_1)
plot(sensitivity_1)

auc_1 <- performance(pr_1, measure = 'auc')
auc_1 <- auc_1@y.values[[1]]
auc_1

# check correlation of variables
vif(logistic_regression_model_1)

###LOGISITC REGRESSION MODEL USING KFOLD WITH K = 10###
set.seed(123)
train_control <- trainControl(method = 'cv', number = 10)
logistic_regression_model_k10 <- train(deathyn ~., data = covid_complete_case_modeling, trControl = train_control, method = 'glm')
saveRDS(logistic_regression_model_k10,"regression_model.rds")

# check ROC curve and area under the curve
p_2 <- predict(logistic_regression_model_k10, newdata = subset(test_data, select=c(1:14)),type='prob')[,2]
pr_2 <- prediction(p_2, test_data$deathyn)
performance_2 <- performance(pr_2, measure = 'tpr', x.measure = 'fpr')
sensitivity_2 <- performance(pr_2, 'sens', 'spec')
plot(performance_2)
plot(sensitivity_2)

auc_2 <- performance(pr_2, measure = 'auc')
auc_2 <- auc_2@y.values[[1]]
auc_2

summary(model_4)
confusionMatrix(model_4)

# histogram of risk factor

covid_complete_case_graph <- covid_complete_case %>% 
  mutate(risk_exp=(-5.808-0.453*covid_complete_case$sex+2.255*covid_complete_case$pneumonia+
                     0.052*covid_complete_case$age+0.329*covid_complete_case$diabetes+
                     0.159*covid_complete_case$copd-0.124*covid_complete_case$asthma+
                     0.308*covid_complete_case$inmsupr+0.158*covid_complete_case$hypertension+
                     0.483*covid_complete_case$other_disease-0.093*covid_complete_case$cardiovascular+
                     0.746*covid_complete_case$renal_chronic-0.076*covid_complete_case$tobacco))
covid_complete_case_graph <- covid_complete_case_graph %>%
  mutate(risk_factor=(exp(covid_complete_case_graph$risk_exp)/(1+exp(covid_complete_case_graph$risk_exp)))*100)


risk_hist <- covid_complete_case_graph %>%
  ggplot(aes(x=covid_complete_case_graph$risk_factor)) + geom_histogram(binwidth = 1)

check_percentile <- function(prediction) {
  for (num in seq(0,1,0.01)){
    if (unname(quantile(covid_complete_case_graph$risk_factor,num)) > prediction){
      return(names(quantile(covid_complete_case_graph$risk_factor,num)))
    }
  }
}

###Decision Tree Model
#training the decision tree as classifier
tree_model <- train(deathyn ~.,data = covid_complete_case_modeling, method="rpart",
                    trControl = trainControl("cv",number=10), tuneLength=10)

#show model characteristics
plot(tree_model)
tree_model$finalModel

# prediction
tree.death.predicted <-predict(tree_model, test_data)
# confusion matrix
round(prop.table(confusionMatrix(tree.death.predicted, test_data$deathyn)$table),4)*100
#save the model
saveRDS(tree_model, "decision_tree.rds")


###Random Forest Model
trcontrol_rose <- trainControl(method = 'cv', number = 10, sampling = 'rose')
random_forest_model <- train(deathyn~., data = covid_complete_case_modeling, method = 'rf', trControl = trcontrol_rose, na.action = na.omit)

plot(random_forest_model)
random_forest_model$finalModel
#confusion matrix
random_forest_model$finalModel$confusion

saveRDS(rf_model_cv_rose, 'random_forest_model.rds')

###
gbm_model <- train(deathyn ~., data = covid_complete_case_modeling, method = "gbm", trControl = train_control, verbose = FALSE)

