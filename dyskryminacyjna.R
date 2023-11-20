library(MASS)
library(xtable)
set.seed(123)


hmeq <- read.csv("hmeq.csv", header = TRUE,  na.strings=c("","NA"))
hmeq <- na.omit(hmeq)
hmeq$REASON <- ifelse(hmeq$REASON == "HomeImp", 1, 0)
hmeq$Other <- ifelse(hmeq$JOB == "Other",1,0)
hmeq$Office<- ifelse(hmeq$JOB == "Office",1,0)
hmeq$Mgr  <- ifelse(hmeq$JOB == "Mgr",1,0)
hmeq$ProfExe<- ifelse(hmeq$JOB == "ProfExe",1,0)
hmeq$Sales<- ifelse(hmeq$JOB == "Sales",1,0)
hmeq$Self<- ifelse(hmeq$JOB == "Self",1,0)
hmeq <- hmeq[,-6]
hmeq <- data.frame(apply(hmeq, 2, function(x){ if(is.factor(x)) as.numeric(as.character(x)) else x} ))




#PodziaÄ¹â€š na zbiory testowy i treningowy 


dane_stan_1 <- hmeq[which(hmeq$BAD ==1),]
dane_stan_2 <- hmeq[which(hmeq$BAD == 0),]
ind_stan_1 <- sample(1:dim(dane_stan_1)[1], size = 0.7*dim(dane_stan_1)[1], replace = FALSE)
ind_stan_2 <- sample(1:dim(dane_stan_2)[1], size = 0.7*dim(dane_stan_2)[1], replace = FALSE)
trening_1 <- dane_stan_1[ind_stan_1,]
trening_0 <- dane_stan_2[ind_stan_2,]
trening_0 <- trening_0[1:(nrow(trening_0)/10), ]
test_1 <- dane_stan_1[-ind_stan_1,]
test_0 <- dane_stan_2[-ind_stan_2,]
train <- rbind(trening_0, trening_1)
test <- rbind(test_0, test_1)



lda_model <- lda(BAD~ LOAN + DEBTINC +YOJ +CLAGE
                 ,train)

trening_01 <- trening_1[,c( "LOAN", "DEBTINC", "YOJ", "CLAGE")]
cov(trening_01)

tab2 <- xtable( cov(trening_01), digits = 6, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")

print(tab2, type = "latex", table.placement = "H", comment=FALSE) 

print(lda_model)

predictions <- predict(lda_model, newdata = test[,-1])
print(predictions$class)


confusion_matrix <- table(predictions$class, test$BAD)
print(confusion_matrix)

qda_model <- qda(BAD~ LOAN + DEBTINC +YOJ +CLAGE
                 ,train)

predictions <- predict(qda_model, newdata = test[,-1])
print(predictions$class)


confusion_matrix <- table(predictions$class, test$BAD)
print(confusion_matrix)



bayes_model <- naiveBayes(train, train[,1])

# Klasyfikacja na podstawie modelu Bayesa
predicted <- predict(bayes_model, test)

# Wyœwietlenie wyników klasyfikacji
print(predicted)

confusion_matrix <- table(predicted, test[,1])
print(confusion_matrix)

wyniki <- confusion_matrix
tab1 <- xtable( wyniki, digits = 3, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
print(tab1, type = "latex", table.placement = "H", comment=FALSE) 


wyniki2 <- matrix(c("0.504717 %" ,"0.495283 %"), nrow = 1)

tab2 <- xtable( wyniki2, digits = 6, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
colnames(tab2) <- paste0(c("0","1"))
print(tab2, type = "latex", table.placement = "H", comment=FALSE) 


wyniki2 <- matrix(c(0, 20612.15, 33.86666, 8.523364, 183.4889,
                    1, 17497.62, 40.52507, 7.619048, 152.105), nrow = 2)
tab3 <- xtable( wyniki2, digits = 6, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
colnames(tab3) <- paste0(c("LOAN","  DEBTINC",  "YOJ", "CLAGE"))
print(tab3, type = "latex", table.placement = "H", comment=FALSE) 


wyniki3 <- matrix(c(-3.814601e-05,6.716433e-02,-4.782517e-03, -5.123835e-03), nrow = 1)
tab4 <- xtable( wyniki3, digits = 6, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
colnames(tab4) <- paste0(c("LOAN","  DEBTINC",  "YOJ", "CLAGE"))
print(tab4, type = "latex", table.placement = "H", comment=FALSE) 


# Instalacja i za³adowanie pakietu
install.packages("e1071")
library(e1071)

# Przygotowanie danych
set.seed(123)
# Przyk³adowe dane
# Utworzenie macierzy zmiennych objaœniaj¹cych
predictors <- matrix(rnorm(200), ncol = 2)
# Utworzenie wektora zmiennych objaœnianych
response <- factor(rep(c("A", "B"), each = 100))

# Podzia³ danych na zbiór treningowy i testowy
train_index <- sample(1:200, 150)
train_data <- predictors[train_index, ]
train_response <- response[train_index]
test_data <- predictors[-train_index, ]

# Wytrenowanie modelu Bayesa
bayes_model <- naiveBayes(train_data, train_response)

# Klasyfikacja na podstawie modelu Bayesa
predicted <- predict(bayes_model, test_data)

# Wyœwietlenie wyników klasyfikacji
print(predicted)
