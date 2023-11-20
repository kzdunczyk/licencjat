library(StepReg)
library(xtable)
set.seed(2137)

hmeq <- read.csv("hmeq.csv", header = TRUE,  na.strings=c("","NA"))
LTV <- round(hmeq$LOAN/hmeq$VALUE, digits = 2)
hmeq <- cbind(hmeq, LTV)
hmeq <- cbind(hmeq$LTV,hmeq$BAD)
hmeq <- na.omit(hmeq)
sumy <- aggregate(V2 ~ V1, data = hmeq, FUN = sum)
licznik <- table(hmeq[,1])
print(licznik)
sumy <- cbind(sumy,licznik)
Proc <- sumy$V2/sumy$Freq
sumy <- cbind(sumy,Proc)
sumy <- head(sumy, n = 98)
sumy <- subset(sumy, Proc != 0)
model <- lm(Proc ~ V1, data = sumy)


summary(model)


plot(sumy$V1, sumy$Proc, main = "Regresja liniowa", xlab = "LTV", ylab = "Proc. nie. kredyt")

# Dodanie linii regresji
abline(model, col = "red")

colnames(sumy) <- c("LTV", "ILo")

df_subset <- sumy[, c(1, ncol(sumy))]
colnames(df_subset) <- c("LTV", "Prawd.")


tab2 <- xtable( head(df_subset, n=10), row.names = TRUE, caption = "Tablica pomyłek")
print(tab2, type = "latex", table.placement = "H", comment=FALSE) 



hmeq2 <- read.csv("hmeq.csv", header = TRUE,  na.strings=c("","NA"))
hmeq2 <- cbind(hmeq2, LTV)
hmeq2 <- subset(hmeq2, select = c('BAD', 'LTV','DEBTINC'))
hmeq2 <- na.omit(hmeq2)
sumy <- aggregate(LTV ~ DEBTINC, data = hmeq2, FUN = mean)
licznik <- table(hmeq2[,1])
print(licznik)
sumy <- cbind(sumy,licznik)
Proc <- sumy$V2/sumy$Freq
sumy <- cbind(sumy,Proc)
sumy <- head(sumy, n = 98)
sumy <- subset(sumy, Proc != 0)
model2 <- lm(Proc ~ LTV + DEBTINC, data = sumy)
