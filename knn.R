# Instalacja i wczytanie pakietu 'class'
library(class)
library(caret)
library(ipred)
library(xtable)
set.seed(123)  

#Wczytanie danych
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
hmeq <- data.frame(lapply(hmeq, as.integer))



#PodziaÅ‚ na zbiory testowy i treningowy 


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



#Utworzenie modelu 
model.knn.2 <- ipredknn( BAD ~ DEBTINC  , data=train, k=17)

# sprawdzamy jakoÅ›Ä‡ modelu
# uwaga: czasami funkcje "predict" dziaÄ¹Â‚ajÃ„Â… niestandardowo
etykietki.prog <- predict(model.knn.2, test, type="class")

# macierz pomyÄ¹Â‚ek (ang. confusion matrix)
(wynik.tablica <- table(etykietki.prog,test$BAD))

n.test <- dim(test)[1]
(n.test - sum(diag(wynik.tablica))) / n.test

tab2 <- xtable( wynik.tablica, row.names = TRUE, caption = "Tablica pomyÅ‚ek")
print(tab2, type = "latex", table.placement = "H", comment=FALSE) 


ggplot(test, aes(x = DEBTINC, y = BAD, color = factor(etykietki.prog))) +
  geom_point() +
  labs(title = "Model k-NN",
       x = "Zmienna DEBTINC",
       y = "Zmienna BAD",
       color = "Prognoza") +
  theme_minimal()
 
hmeq$BAD <- as.integer(hmeq$BAD)
hmeq$DEBTINC <- as.integer(hmeq$DEBTINC)



error_rates <- numeric(length = 20)


# Obliczenie b³êdów klasyfikacji dla ró¿nych wartoœci k
for (k in 1:20) {
  model.knn.2 <- ipredknn( BAD ~ DEBTINC  , data=train, k=k)
  etykietki.prog <- predict(model.knn.2, test, type="class")
  wynik.tablica <- table(etykietki.prog,test$BAD)
  n.test <- dim(test)[1]
  error_rates[k] <- (n.test - sum(diag(wynik.tablica))) / n.test
}

plot(1:20, error_rates, lwd=2,type = "b", xlab = "Liczba s¹siadów (k)", ylab = "B³¹d klasyfikacji",xlim=c(1, 20),ylim=c(0.25, 0.34),main="Wp³yw liczby s¹siadów na b³¹d klasyfikacji modelu")
