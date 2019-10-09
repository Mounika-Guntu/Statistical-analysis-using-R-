my_data = read.csv(file.choose(), sep = ",",header = TRUE)
# Cronbach's aplha forsurvey Instrument consistency
install.packages("pysch")
library("pysch")
alpha(my_data,check.keys= TRUE)
psych::alpha(my_data,check.keys = TRUE)$total$std.alpha
install.packages("dplyr")
library("dplyr")
cognitive <- select(filter(my_data),c("Q1_1","Q1_2","Q1_3","Q1_4","Q1_5","Q1_6","Q1_7","Q1_8","Q1_9","Q1_10","Q1_11","Q1_12","Q1_13"))
alpha(cognitive,check.keys = TRUE)
psych::alpha(cognitive,check.keys = TRUE)$total$std.alpha
behavioral <- select(filter(my_data),c("Q1_14","Q1_15","Q1_16","Q1_17","Q1_18","Q1_19","Q1_20","Q1_21","Q1_22","Q1_23","Q1_24"))
alpha(behavioral,check.keys = TRUE)
psych::alpha(behavioral,check.keys = TRUE)$total$std.alpha
emotional <- select(filter(my_data),c("Q1_25","Q1_26","Q1_27","Q1_28","Q1_29"))
alpha(emotional,check.keys = TRUE)
psych::alpha(emotional,check.keys = TRUE)$total$std.alpha
#Split-Half Correlation And Reliability with Spearman-Brown reliability using splithalf.r function 
install.packages("multicon")
library("multicon")
splithalf.r(my_data, sims = 1000, graph = TRUE)
splithalf.r(cognitive, sims = 1000, graph = TRUE)
splithalf.r(behavioral, sims=1000, graph = TRUE)
splithalf.r(emotional, sims = 1000, graph = TRUE)




#Fleiss' Kappa Coefficient for reliability of agreement:
#method1:
#install.packages("devtools")
library(devtools)
#install_github("raredd/ragree")
library("ragree")
fleiss.kappa.raw(cognitive,conflev = 0.95, N = Inf,print = TRUE)
fleiss.kappa.raw(behavioral, conflev = 0.95, N = Inf,print = TRUE)
fleiss.kappa.raw(emotional, conflev = 0.95, N = Inf,print = TRUE)
#method2:
#install.packages("irr")
library("irr")
X <- kappam.fleiss(cognitive, exact = FALSE, detail = FALSE)
print(X)
format.pval(X$p.value)
Y <- kappam.fleiss(behavioral, exact = FALSE, detail = FALSE)
print(Y)
format.pval(Y$p.value)
Z <- kappam.fleiss(emotional, exact = FALSE, detail = FALSE)
print(Z)
format.pval(Z$p.value)


#Kendall’s Tau-b for the sub-construct association:

install.packages("Kendall")
library("Kendall")
data1 <- read.csv(file.choose(), stringsAsFactors=FALSE)
cognitive_ques <- c("Q1_1","Q1_2","Q1_3","Q1_4","Q1_5","Q1_6","Q1_7","Q1_8","Q1_9","Q1_10","Q1_11","Q1_12","Q1_13")
behave_ques <- c("Q1_14","Q1_15","Q1_16","Q1_17","Q1_18","Q1_19","Q1_20","Q1_21","Q1_22","Q1_23","Q1_24")
emotion_ques <- c("Q1_25","Q1_26","Q1_27","Q1_28","Q1_29")

#cognitive vs behavioral: 
for(i in cognitive_ques) {
  for(j in behave_ques) {
    print(i)
    print(j)
    summary(Kendall(data1[,i],data1[,j]))
  }
}

#cognitive vs emotional
for(i in cognitive_ques) {
  for(j in emotion_ques) {
    print(i)
    print(j)
    summary(Kendall(data1[,i],data1[,j]))
  }
}

#behavioral vs emotional
for(i in behave_ques) {
  for(j in emotion_ques) {
    print(i)
    print(j)
    summary(Kendall(data1[,i],data1[,j]))
  }
}





