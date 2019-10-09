NSSE_2018 <- read.csv(file.choose(), header = TRUE,na.strings=" ")
which(is.na(NSSE_2018))
modefunc <- function(x){
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if(sum(tabresult == max(tabresult))>1) themode <- NA
  return(themode)
}
apply(NSSE_2018, 2, modefunc)
# Assigning highest possible to missing values
NSSE_2018$Q1_1[which(is.na(NSSE_2018$Q1_1))] <- 2
NSSE_2018$Q1_2[which(is.na(NSSE_2018$Q1_2))] <- 3
NSSE_2018$Q1_3[which(is.na(NSSE_2018$Q1_3))] <- 3
NSSE_2018$Q1_4[which(is.na(NSSE_2018$Q1_4))] <- 3
NSSE_2018$Q1_5[which(is.na(NSSE_2018$Q1_5))] <- 1
NSSE_2018$Q1_6[which(is.na(NSSE_2018$Q1_6))] <- 1
NSSE_2018$Q1_7[which(is.na(NSSE_2018$Q1_7))] <- 2
NSSE_2018$Q1_8[which(is.na(NSSE_2018$Q1_8))] <- 3
NSSE_2018$Q1_9[which(is.na(NSSE_2018$Q1_9))] <- 3
NSSE_2018$Q1_10[which(is.na(NSSE_2018$Q1_10))] <- 3
NSSE_2018$Q1_11[which(is.na(NSSE_2018$Q1_11))] <- 2
NSSE_2018$Q1_12[which(is.na(NSSE_2018$Q1_12))] <- 2
NSSE_2018$Q1_13[which(is.na(NSSE_2018$Q1_13))] <- 3
NSSE_2018$Q1_14[which(is.na(NSSE_2018$Q1_14))] <- 3
NSSE_2018$Q1_15[which(is.na(NSSE_2018$Q1_15))] <- 2
NSSE_2018$Q1_16[which(is.na(NSSE_2018$Q1_16))] <- 3
NSSE_2018$Q1_17[which(is.na(NSSE_2018$Q1_17))] <- 1
NSSE_2018$Q1_18[which(is.na(NSSE_2018$Q1_18))] <- 3
NSSE_2018$Q1_19[which(is.na(NSSE_2018$Q1_19))] <- 3
NSSE_2018$Q1_20[which(is.na(NSSE_2018$Q1_20))] <- 3
NSSE_2018$Q1_21[which(is.na(NSSE_2018$Q1_21))] <- 3
NSSE_2018$Q1_22[which(is.na(NSSE_2018$Q1_22))] <- 3
NSSE_2018$Q1_23[which(is.na(NSSE_2018$Q1_23))] <- 5
NSSE_2018$Q1_24[which(is.na(NSSE_2018$Q1_24))] <- 3
NSSE_2018$Q1_25[which(is.na(NSSE_2018$Q1_25))] <- 3
NSSE_2018$Q1_26[which(is.na(NSSE_2018$Q1_26))] <- 1
NSSE_2018$Q1_27[which(is.na(NSSE_2018$Q1_27))] <- 2
NSSE_2018$Q1_28[which(is.na(NSSE_2018$Q1_28))] <- 3
NSSE_2018$Q1_29[which(is.na(NSSE_2018$Q1_29))] <- 2
NSSE_2018$Q1_30[which(is.na(NSSE_2018$Q1_30))] <- 3
NSSE_2018$Q1_31[which(is.na(NSSE_2018$Q1_31))] <- 2
NSSE_2018$Q1_32[which(is.na(NSSE_2018$Q1_32))] <- 3
NSSE_2018$Q1_33[which(is.na(NSSE_2018$Q1_33))] <- 2
NSSE_2018$Q1_34[which(is.na(NSSE_2018$Q1_34))] <- 1
NSSE_2018$Q1_35[which(is.na(NSSE_2018$Q1_35))] <- 2
NSSE_2018$Q1_36[which(is.na(NSSE_2018$Q1_36))] <- 2
NSSE_2018$Q1_37[which(is.na(NSSE_2018$Q1_37))] <- 4
NSSE_2018$Q1_38[which(is.na(NSSE_2018$Q1_38))] <- 4
NSSE_2018$Q1_39[which(is.na(NSSE_2018$Q1_39))] <- 4
NSSE_2018$Q1_40[which(is.na(NSSE_2018$Q1_40))] <- 4
NSSE_2018$Q1_41[which(is.na(NSSE_2018$Q1_41))] <- 2
NSSE_2018$Q1_42[which(is.na(NSSE_2018$Q1_42))] <- 3
NSSE_2018$Q1_43[which(is.na(NSSE_2018$Q1_43))] <- 2
NSSE_2018$Q1_44[which(is.na(NSSE_2018$Q1_44))] <- 2
NSSE_2018$Q1_45[which(is.na(NSSE_2018$Q1_45))] <- 2
NSSE_2018$Q1_46[which(is.na(NSSE_2018$Q1_46))] <- 2
NSSE_2018$Q1_47[which(is.na(NSSE_2018$Q1_47))] <- 3
NSSE_2018$Q1_48[which(is.na(NSSE_2018$Q1_48))] <- 3
NSSE_2018$Q1_49[which(is.na(NSSE_2018$Q1_49))] <- 6
NSSE_2018$Q1_50[which(is.na(NSSE_2018$Q1_50))] <- 7
NSSE_2018$Q1_51[which(is.na(NSSE_2018$Q1_51))] <- 6
NSSE_2018$Q1_52[which(is.na(NSSE_2018$Q1_52))] <- 9
NSSE_2018$Q1_53[which(is.na(NSSE_2018$Q1_53))] <- 5
which(is.na(NSSE_2018))
#creating a new csv file with cleaned data
write.csv(NSSE_2018,'NSSE2018(today1).csv')
NSSE2018 <- read.csv(file.choose(), header = TRUE , sep = ',')
which(is.na(NSSE2018))
#install.packages('plyr')
library(plyr)
NSSE2018$Q1_4 <- as.character(NSSE2018$Q1_4)
NSSE2018$Q1_4 <- revalue(NSSE2018$Q1_4, c("3" = "2","4"="2","5"= "3", "6" = "3", "7" = "4"))
NSSE2018$Q1_5 <- as.character(NSSE2018$Q1_5)
NSSE2018$Q1_5 <- revalue(NSSE2018$Q1_5, c("3" = "2","4"="2","5"= "3", "6" = "3", "7" = "4"))
NSSE2018$Q1_6 <- as.character(NSSE2018$Q1_6)
NSSE2018$Q1_6 <- revalue(NSSE2018$Q1_6, c("3" = "2","4"="2","5"= "3", "6" = "3", "7" = "4"))
NSSE2018$Q1_14 <- as.character(NSSE2018$Q1_14)
NSSE2018$Q1_14 <- revalue(NSSE2018$Q1_14, c("5" = "4"))
NSSE2018$Q1_15 <- as.character(NSSE2018$Q1_15)
NSSE2018$Q1_15 <- revalue(NSSE2018$Q1_15, c("3" = "2","4"="2","5"= "3", "6" = "3", "7" = "3", "8" = "4"))
NSSE2018$Q1_16 <- as.character(NSSE2018$Q1_16)
NSSE2018$Q1_16 <- revalue(NSSE2018$Q1_16, c("3" = "2","4"="2","5"= "3", "6" = "3", "7" = "3", "8" = "4"))
NSSE2018$Q1_22 <- as.character(NSSE2018$Q1_22)
NSSE2018$Q1_22 <- revalue(NSSE2018$Q1_22, c("3" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
NSSE2018$Q1_48 <- as.character(NSSE2018$Q1_48)
NSSE2018$Q1_48 <- revalue(NSSE2018$Q1_48, c("9" = "1", "1" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
NSSE2018$Q1_49 <- as.character(NSSE2018$Q1_49)
NSSE2018$Q1_49 <- revalue(NSSE2018$Q1_49, c("9" = "1", "1" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
NSSE2018$Q1_50 <- as.character(NSSE2018$Q1_50)
NSSE2018$Q1_50 <- revalue(NSSE2018$Q1_50, c("9" = "1", "1" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
NSSE2018$Q1_51 <- as.character(NSSE2018$Q1_51)
NSSE2018$Q1_51 <- revalue(NSSE2018$Q1_51, c("9" = "1", "1" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
NSSE2018$Q1_52 <- as.character(NSSE2018$Q1_52)
NSSE2018$Q1_52 <- revalue(NSSE2018$Q1_52, c("9" = "1", "1" = "2","4"="3","5"= "3", "6" = "4", "7" = "4"))
write.csv(NSSE2018,'NSSE2018(today3).csv')
