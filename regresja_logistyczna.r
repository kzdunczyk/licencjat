set.seed(123)
library(lmtest)
dane <- read.csv("hmeq.csv", header = TRUE,  na.strings=c("","NA"))
dane <- na.omit(dane)
dane <- dane[,-(5:6)]
dane <- data.frame(apply(dane, 2, function(x){ if(is.factor(x)) as.numeric(as.character(x)) else x} ))



dane_stan_1 <- dane[which(dane$BAD ==1),]
dane_stan_2 <- dane[which(dane$BAD == 0),]
ind_stan_1 <- sample(1:dim(dane_stan_1)[1], size = 0.7*dim(dane_stan_1)[1], replace = FALSE)
ind_stan_2 <- sample(1:dim(dane_stan_2)[1], size = 0.7*dim(dane_stan_2)[1], replace = FALSE)
trening_1 <- dane_stan_1[ind_stan_1,]
trening_0 <- dane_stan_2[ind_stan_2,]
trening_0 <- trening_0[1:(nrow(trening_0)/10), ]
test_1 <- dane_stan_1[-ind_stan_1,]
test_0 <- dane_stan_2[-ind_stan_2,]
trening <- rbind(trening_0, trening_1)
test <- rbind(test_0, test_1)
# B
model <- glm(BAD~.,trening, family=binomial)
lrtest(model1)
# odrzucamy H0
model2 <- glm(BAD~ LOAN + MORTDUE + VALUE ,trening, family=binomial)


lrtest(model2)



beta <- summary(model)$coefficients[,1]
SE <- summary(model)$coefficients[,2]
confint(model,level=0.95)
summary(model)$coefficients[,4]

pi_est <- predict.glm(model,newdata = test[,-1], type='response')



pi_0 <- 0.7
y_est <- ifelse(pi_est > pi_0, 1, 0)
stany <- data.frame(stan_est=y_est, stan=test$BAD)
TP <- length(which(stany$stan_est==1 & stany$stan==1))
FP <- length(which(stany$stan_est==1 & stany$stan==0))
FN <- length(which(stany$stan_est==0 & stany$stan==1))
TN <- length(which(stany$stan_est==0 & stany$stan==0))

czulosc <- TP/ (TP + FN)
specyficznosc <- TN / (TN + FP)
prawd_pop_kl <- (TP + TN) / length(stany$stan_est)

wyniki <- t(matrix(c(TN,FN,FP,TP),ncol =2))
tab1 <- xtable( wyniki, digits = 3, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
colnames(tab1) <- paste0(c("Wartości prognozowane","Wartości rzeczywiste"))
print(tab1, type = "latex", table.placement = "H", comment=FALSE) 




# F
library(pROC)

roc_obj <- roc(test$BAD, pi_est)
plot(roc_obj,  main =  "Krzywa ROC", xlab = "Odsetek fałszywie pozytywnych", ylab = "Odsetek prawdziwe pozytwnych") 
auc(roc_obj)

