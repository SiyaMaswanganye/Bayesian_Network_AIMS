
# General Linear Models
MyData$Outcomes2 <- as.factor(MyData$Outcomes2)

#To make sure that the variables used have no empty values 
clean_data<- MyData[complete.cases(MyData[c("Sex", "Age range", "UpdatedSelfDefEth", "Officer-defined ethnicity", "Purpose")]), ]

set.seed(123)  # for reproducibility
sample_data <- clean_data[sample(nrow(clean_data), 100000), ]
#Arrest vs Others

##Parallel Lines Analysis

ArrestVsOthersGLM01 <- glm(ArrestvsOthers ~ Sex + `Age range` + UpdatedSelfDefEth + `Officer-defined ethnicity` + Purpose, 
                           data = sample_data, 
                           family = binomial)
summary(ArrestVsOthersGLM01)
ArrestVsOthersGLM02 <- glm(ArrestvsOthers ~ Sex + `Age range` + UpdatedSelfDefEth + `Officer-defined ethnicity`, 
                           data = sample_data, 
                           family = binomial)
summary(ArrestVsOthersGLM02)
#Model Evaluation 
ArrestANOVAResults <- anova(ArrestVsOthersGLM01,ArrestVsOthersGLM02, test = 'Chisq')
ArrestANOVAResults

##Seperate lines 

ArrestVsOthersGLM03 <- glm(ArrestvsOthers ~ Sex * `Age range` * UpdatedSelfDefEth * `Officer-defined ethnicity` * Purpose, 
                           data = sample_data, 
                           family = binomial)
summary(ArrestVsOthersGLM03)

ArrestVsOthersGLM04 <- glm(ArrestvsOthers ~ Sex * `Age range` * UpdatedSelfDefEth * `Officer-defined ethnicity`, 
                           data = sample_data, 
                           family = binomial)
summary(ArrestVsOthersGLM04)

#Model Evaluation 
NFAANOVAResults <- anova(ArrestVsOthersGLM01, ArrestVsOthersGLM04, test = 'Chisq')

NFAANOVAResults

# NFA vs Others

##Parallel lines

NFAvsRestGLM01 <- glm(NFAvsOther ~ Sex + `Age range` + UpdatedSelfDefEth + `Officer-defined ethnicity` + Purpose, 
                    data = sample_data, 
                    family = binomial)

summary(NFAvsRestGLM01)

NFAvsRestGLM02 <- glm(NFAvsOther ~ Sex + `Age range` + UpdatedSelfDefEth + `Officer-defined ethnicity` , 
                      data = sample_data, 
                      family = binomial)
summary(NFAvsRestGLM02)

#Model evaluation 
NFA_ANOVAResults <- anova(NFAvsRestGLM01,NFAvsRestGLM02, test = 'Chisq')
NFA_ANOVAResults

#Seperate lines 

NFAvsRestGLM03 <- glm(NFAvsOther ~ Sex * `Age range` * UpdatedSelfDefEth * `Officer-defined ethnicity` * Purpose, 
                      data = sample_data, 
                      family = binomial)

summary(NFAvsRestGLM03)

NFAvsRestGLM04 <- glm(NFAvsOther ~ Sex * `Age range` * UpdatedSelfDefEth * `Officer-defined ethnicity` , 
                      data = sample_data, 
                      family = binomial)
summary(NFAvsRestGLM04)

#Model evaluation 
NFA_ANOVAResults <- anova(NFAvsRestGLM03,NFAvsRestGLM04, test = 'Chisq')
NFA_ANOVAResults


#Comparison between Parallel lines and Seperate lines

ComparisonArrest01 <- anova(ArrestVsOthersGLM02, ArrestVsOthersGLM03)
ComparisonArrest01
ComparisonNFA01 <- anova(NFAvsRestGLM01, NFAvsRestGLM03)
ComparisonNFA01
