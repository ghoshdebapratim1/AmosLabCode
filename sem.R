
### Reading the Data
df=read.csv('cleaned_uic.csv')

### Installing the relevant packages
install.packages('lavaan')
library(lavaan)

### 
names(df)

data=df[,c("Q4_1" ,"Q4_2","Q4_3","Q4_4","Q4_5", "Q4_6","Q4_7","Q4_8","Q4_9")]

lavCor(data, ordered =c("Q4_1" ,"Q4_2","Q4_3","Q4_4","Q4_5", "Q4_6","Q4_7","Q4_8","Q4_9"))


Model <- 'f1 =~ Q4_1 + Q4_2 + Q4_3+ Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9'

fit <- cfa(Model, data, std.lv = TRUE,
           ordered = c("Q4_1" ,"Q4_2","Q4_3","Q4_4","Q4_5", "Q4_6","Q4_7","Q4_8","Q4_9"))
parameterEstimates(fit)



Model <- ' career_decision_making =~ Q4_1 + Q4_2 + Q4_3+ Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9
           career_exploratory_plans =~ Q5_1 + Q5_2 + Q5_3 +Q5_4 +Q5_5 
           career_envir_exploration=~Q6_1+Q6_2+Q6_3+Q6_4+Q6_5+Q6_6
           career_self_exploration=~Q7_1+Q7_2+Q7_3+Q7_4+Q7_5
           career_thoughts_inventory=~Q8_1+Q8_2+Q8_3+Q8_4+Q8_5+Q8_6+Q8_7+Q8_8+Q8_9+Q8_10+Q8_11+Q8_12+Q8_13+Q8_14+Q8_15+Q8_16+Q8_17+Q8_18+Q8_19+Q8_20+Q8_21+Q8_22+Q8_23+Q8_24+Q8_25+Q8_26+Q8_27+Q8_28+Q8_29+Q8_30+Q8_31+Q8_32+Q8_33
'

data=df[,c('Q4_1',
           'Q4_2', 'Q4_3', 'Q4_4', 'Q4_5', 'Q4_6', 'Q4_7', 'Q4_8', 'Q4_9', 'Q5_1',
           'Q5_2', 'Q5_3', 'Q5_4', 'Q5_5', 'Q6_1', 'Q6_2', 'Q6_3', 'Q6_4', 'Q6_5',
           'Q6_6', 'Q7_1', 'Q7_2', 'Q7_3', 'Q7_4', 'Q7_5', 'Q8_1', 'Q8_2', 'Q8_3',
           'Q8_4', 'Q8_5', 'Q8_6', 'Q8_7', 'Q8_8', 'Q8_9', 'Q8_10', 'Q8_11',
           'Q8_12', 'Q8_13', 'Q8_14', 'Q8_15', 'Q8_16', 'Q8_17', 'Q8_18', 'Q8_19',
           'Q8_20', 'Q8_21', 'Q8_22', 'Q8_23', 'Q8_24', 'Q8_25', 'Q8_26', 'Q8_27',
           'Q8_28', 'Q8_29', 'Q8_30', 'Q8_31', 'Q8_32', 'Q8_33')]


fit<-cfa(Model,data, std.lv = TRUE,ordered =c('Q4_1',
                                              'Q4_2', 'Q4_3', 'Q4_4', 'Q4_5', 'Q4_6', 'Q4_7', 'Q4_8', 'Q4_9', 'Q5_1',
                                              'Q5_2', 'Q5_3', 'Q5_4', 'Q5_5', 'Q6_1', 'Q6_2', 'Q6_3', 'Q6_4', 'Q6_5',
                                              'Q6_6', 'Q7_1', 'Q7_2', 'Q7_3', 'Q7_4', 'Q7_5', 'Q8_1', 'Q8_2', 'Q8_3',
                                              'Q8_4', 'Q8_5', 'Q8_6', 'Q8_7', 'Q8_8', 'Q8_9', 'Q8_10', 'Q8_11',
                                              'Q8_12', 'Q8_13', 'Q8_14', 'Q8_15', 'Q8_16', 'Q8_17', 'Q8_18', 'Q8_19',
                                              'Q8_20', 'Q8_21', 'Q8_22', 'Q8_23', 'Q8_24', 'Q8_25', 'Q8_26', 'Q8_27',
                                              'Q8_28', 'Q8_29', 'Q8_30', 'Q8_31', 'Q8_32', 'Q8_33'))

install.packages('semPlot')

## To Plot the Relationships 
library(semPlot)

semPaths(fit,style='lisrel')

summary(fit)


2*fitMeasures(fit)['fmin']

fitMeasures(fit)['rmsea'] # Root mean square of approximation  which is a measure of absolute fit 

# <0.05 - Very good fit 
# 0.05 - 0.08 - good fit 
# 0.08 - 0.1 - mediocre fit 
# >0.1 - Poor fit 
## Ours is a good fit rmsea =0.07485529

## Goodness of fit 
fitMeasures(fit)['rmsea.pvalue']

residuals(fit)$cov

calEffSizes(model, n, Cov, Mean)
modindices(fit,sort=T)


### Next Model 

Model2 <- ' career_decision_making =~ Q4_1 + Q4_2 + Q4_3+ Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9
           career_exploratory_plans =~ Q5_1 + Q5_2 + Q5_3 +Q5_4 +Q5_5 + Q6_1+Q6_2+Q6_3+Q6_4+Q6_5+Q6_6
           career_envir_exploration=~Q6_1+Q6_2+Q6_3+Q6_4+Q6_5+Q6_6 + Q5_1 + Q5_2 + Q5_3 +Q5_4 +Q5_5
           career_self_exploration=~Q7_1+Q7_2+Q7_3+Q7_4+Q7_5
           career_thoughts_inventory=~Q8_1+Q8_2+Q8_3+Q8_4+Q8_5+Q8_6+Q8_7+Q8_8+Q8_9+Q8_10+Q8_11+Q8_12+Q8_13+Q8_14+Q8_15+Q8_16+Q8_17+Q8_18+Q8_19+Q8_20+Q8_21+Q8_22+Q8_23+Q8_24+Q8_25+Q8_26+Q8_27+Q8_28+Q8_29+Q8_30+Q8_31+Q8_32+Q8_33
'
fit2<-cfa(Model2,data, std.lv = TRUE,ordered =c('Q4_1',
                                              'Q4_2', 'Q4_3', 'Q4_4', 'Q4_5', 'Q4_6', 'Q4_7', 'Q4_8', 'Q4_9', 'Q5_1',
                                              'Q5_2', 'Q5_3', 'Q5_4', 'Q5_5', 'Q6_1', 'Q6_2', 'Q6_3', 'Q6_4', 'Q6_5',
                                              'Q6_6', 'Q7_1', 'Q7_2', 'Q7_3', 'Q7_4', 'Q7_5', 'Q8_1', 'Q8_2', 'Q8_3',
                                              'Q8_4', 'Q8_5', 'Q8_6', 'Q8_7', 'Q8_8', 'Q8_9', 'Q8_10', 'Q8_11',
                                              'Q8_12', 'Q8_13', 'Q8_14', 'Q8_15', 'Q8_16', 'Q8_17', 'Q8_18', 'Q8_19',
                                              'Q8_20', 'Q8_21', 'Q8_22', 'Q8_23', 'Q8_24', 'Q8_25', 'Q8_26', 'Q8_27',
                                              'Q8_28', 'Q8_29', 'Q8_30', 'Q8_31', 'Q8_32', 'Q8_33'))


fitMeasures(fit2)['rmsea']


### Model 3 

Model3 <- ' 
## Factor Loading 
career_decision_making =~ 1*Q4_1 + Q4_2 + Q4_3+ Q4_4+Q4_5+Q4_6+Q4_7+Q4_8+Q4_9
           career_exploratory_plans =~ 1*Q5_1 + Q5_2 + Q5_3 +Q5_4 +Q5_5 
           career_envir_exploration=~1*Q6_1+Q6_2+Q6_3+Q6_4+Q6_5+Q6_6 
           career_self_exploration=~1*Q7_1+Q7_2+Q7_3+Q7_4+Q7_5
           career_thoughts_inventory=~1*Q8_1+Q8_2+Q8_3+Q8_4+Q8_5+Q8_6+Q8_7+Q8_8+Q8_9+Q8_10+Q8_11+Q8_12+Q8_13+Q8_14+Q8_15+Q8_16+Q8_17+Q8_18+Q8_19+Q8_20+Q8_21+Q8_22+Q8_23+Q8_24+Q8_25+Q8_26+Q8_27+Q8_28+Q8_29+Q8_30+Q8_31+Q8_32+Q8_33
           
          '
fit3<-cfa(Model3,data)
fitMeasures(fit3)['rmsea']

library(dplyr)

modificationindices(fit3) %>% arrange(-mi) %>% head(10)

