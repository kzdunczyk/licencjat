#Drzewa kasyfikacyjne
library(rpart)
library(rpart.plot)
library(MASS)
library(xtable)

# Ustawienie ziarna dla powtarzalności
set.seed(2137)  

#Wczytanie danych
hmeq <- read.csv("hmeq.csv", header = TRUE,  na.strings=c("","NA"))
hmeq <- na.omit(hmeq)
hmeq$BAD <- ifelse(hmeq$BAD == 1, 'BAD', 'GOOD')

#Podział na zbiory testowy i treningowy 
indices <- sample(1:nrow(hmeq), size = 0.7*nrow(hmeq)) 
train <- hmeq[indices, ]  
test <- hmeq[-indices, ]  


#Utworzeni modelu
model <- BAD ~ .

#Kreowanie dużego drzewa 
hmeq.tree.complex <- rpart(model, data=train, cp=.01, minsplit=5, maxdepth=20)
hmeq.tree.complex.pruned <- prune(hmeq.tree.complex, cp = 0.030151)
plotcp(hmeq.tree.complex)
printcp(hmeq.tree.complex)
rpart.plot(hmeq.tree.complex,
             main="Oryginalne drzewo")
rpart.plot(hmeq.tree.complex.pruned,
             main="Przyciete drzewo")




wyniki <- t(matrix(c(1, 0.216080,      0,   1.00000, 1.00000, 0.067826,
                     2, 0.055276,      1,   0.78392, 0.81910, 0.061895,
                     3, 0.030151,      2,   0.72864, 0.78392, 0.060648,
                     4, 0.016511,      3,   0.69849, 0.76382, 0.059920,
                     5, 0.015075,     10,   0.58291, 0.77387, 0.060286,
                     6, 0.013400,     12,   0.55276, 0.75377, 0.059552,
                     7, 0.011725,    15,  0.51256, 0.72864, 0.058617,
                     8, 0.010050,     21,   0.44221, 0.68844, 0.057080,
                     9, 0.010000,     26,   0.39196, 0.68844, 0.057080),ncol =9))
tab1 <- xtable( wyniki, digits = 6, row.names = TRUE, caption = "Wstawiamy podpis pod tabel", label = "tab:tabela1")
colnames(tab1) <- paste0(c("indeks", "CP", "nsplit" ,"rel error",  "xerror",     "xstd"))
print(tab1, type = "latex", table.placement = "H", comment=FALSE) 


pred.labels <- predict(hmeq.tree.complex, newdata = test,
                       type = "class")
table(pred.labels, test$BAD) -> matrix2 

tab2 <- xtable( matrix2, row.names = TRUE, caption = "Tablica pomyłek")
print(tab2, type = "latex", table.placement = "H", comment=FALSE) 
  
