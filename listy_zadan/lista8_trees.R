library(rpart)
library(datasets)
library(data.table)
library(party)
library(caret)
library(maptree)
library(rattle)


########################################
#ZAD 1. Na wbudowanych danych „iris” za pomocą „rpart” zbuduj drzewo klasyfikacyjne szacujące przynależność do gatunku wykorzystując jako predyktory: 
#  a.	dane o Sepalach
#b.	wszystkie dane
#Porównaj procent właściwie zakwalifikowanych przypadków. Które drzewo lepiej klasyfikuje?
#Sprawdź wpływ parametrów minsplit i cp  na głębokość drzewa.

#ad 1#######################################

summary(iris)

tree1<-rpart(Species~Sepal.Length+Sepal.Width,iris)

plot(tree1)
text(tree1,use.n = TRUE)


tree1$frame

summary(tree1)


##wyciagniecie przypisania na piechotę
#ls(tree1)
table(tree1$where)
table(tree1$y)
tree1$y
(result1<-table(tree1$where,tree1$y))
table(tree1$y,iris$Species)
(n<-length(tree1$y))

printcp(tree1)
#1 - setosa - 3,7
#2 - versicolor - 4,8
#3 - virginica - 9

dobre<-result1[1,1] + result1[3,1] + result1[2,2]+result1[4,2] + result1[5,3]
true1<-dobre/n*100
true1


predict(tree1)
(result1a<-predict(tree1,type="class"))

iris2<-iris
iris2$predict1<-result1a
(good1<-sum(iris2$predict1==iris2$Species)/n*100)
true1

##inne przykłady jak ładniej narysować
library(maptree)
draw.tree(tree1,cex=0.7,nodeinfo=TRUE)

library(rattle)
drawTreeNodes(tree3, col = NULL, nodeinfo = FALSE,decimals = 2,  print.levels = TRUE, new = TRUE) 

#ad b #######################################
tree2<-rpart(Species~.,iris)
plot(tree2)
text(tree2,use.n = TRUE)


(result1b<-predict(tree2,type="class"))

iris2$predict2<-result1b
(good2<-sum(iris2$predict2==iris2$Species)/n*100)


#ad b parametry#############################

tree3<-rpart(Species~.,iris,control=rpart.control(minsplit = 5, cp = 0.01))
plot(tree3)
text(tree3,use.n = TRUE)
printcp(tree3)

tree4<-rpart(Species~.,iris,control=rpart.control(minsplit = 10, cp = 0.02))
plot(tree4)
text(tree4,use.n = TRUE)
printcp(tree4)

tree4<-rpart(Species~.,iris,control=rpart.control(minsplit = 1, cp = 0.001))
plot(tree4)
text(tree4,use.n = TRUE)


iris2<-iris
(result13<-predict(tree3,type="class"))
table(tree3$where,tree3$y)
iris2$predict3<-result13
n<-150
(good3<-sum(iris2$predict3==iris2$Species)/n*100)

(result14<-predict(tree4,type="class"))

iris2$predict4<-result14
(good4<-sum(iris2$predict4==iris2$Species)/n*100)


########################################
#ZAD 2. Teraz zbuduj drzewo z pomocą „ctree”, w dalszym ciągu wykorzystaj dane „iris” oraz wszystkie predyktory.
########################################


tree5<-ctree(Species~.,iris)
plot(tree5)
plot(tree6)

Predict(tree5)

(result25<-Predict(tree5))

iris2$predict5<-result25
(good5<-sum(iris2$predict5==iris2$Species)/n*100)

tree6<-ctree(Species~.,iris,controls = ctree_control(maxdepth = 2))

tree6<-ctree(Species~.,iris,controls = ctree_control(teststat = c("quad", "max"), testtype = c("MonteCarlo"),mincriterion = 0.8, minsplit = 5,maxdepth = 0))
plot(tree6)

(result26<-Predict(tree6))


iris2$predict6<-result26
(good6<-sum(iris2$predict6==iris2$Species)/n*100)

########################################
#ZAD 3. Uzupełnij braki danych w ramce „cases” (możesz wspomóc się przesłanym skryptem z laboratorium numer 4)
########################################

load("KrukUWr2017.Rdata")
ls()
summary(cases)
summary(events)


class(cases)
cases<-as.data.frame(cases)
ncol(cases)
for(i in 1:ncol(cases)){
  med<-median(na.omit(cases[,i]))  
  cases[is.na(cases[,i]),i]<-med
} 

summary(dane)
summary(cases)

########################################
#ZAD 4. Na bazie ramki danych „events” stwórz następujące zmienne dla wszystkich spraw z ramki „cases” i dodaj je do nowej ramki np. „cases2”
########################################


dane6m<-events[events$Month<=6,] 
#a.	Y1- G (good), B (bad) na bazie wpłat 6M – czy były większe niż 50
dane6m[is.na(PaymentAmount),PaymentAmount:=0]
summary(dane6m)


wplaty <- dane6m[, .(SumaWplat = sum(PaymentAmount)), by = "CaseId"]
summary(dane6m)
summary(wplaty)


cases2 <- merge(cases,wplaty,by="CaseId")
summary(cases2)
rm(cases)

cases2$y1<-as.factor(ifelse(cases2$SumaWplat>=50,'G','B'))
table(cases2$y1)

#b.	Y2 - 1, 0 na bazie wpłat 6M – czy były większe niż 50
cases2$y2<-ifelse(cases2$SumaWplat>=50,1,0)
table(cases2$y2)

#c.	Y3 - Suma wpłat 6M
cases2$y3<-cases2$SumaWplat
summary(cases2)


#przekodowanie na factory
summary(cases2)
cases2$Product<-as.factor(cases2$Product)
cases2$Gender<-as.factor(cases2$Gender)


########################################
#ZAD 5. Podziel  zbiór „cases2” na zbiór uczący i testowy w proporcji 60:40.
########################################


ind.ucz<-createDataPartition(cases2$y2, times=1, p=0.6, list=FALSE)


testowe<-cases2[-ind.ucz,]
uczace<-cases2[ind.ucz,]

dim(testowe)
dim(uczace)
##sprawdzamy podział y3
summary(testowe)
# Mean   :   191.6
summary(uczace)
#Mean   :  190.74


########################################
#ZAD 6. Sprawdź działanie metod  (rpart, ctree) dla przypadków Y1,Y2 przy użyciu jednego predyktora: Age
########################################
table(uczace$y1)
(treey1c<- ctree(y1~Age,uczace))

(treey1r<- rpart(y1~Age,uczace))

(treey1r<- rpart(y1~Age,uczace,control=rpart.control(minsplit = 500, cp = 0.0001)))

table(Predict(treey1c))
plot(treey1c)


(treey2c<- ctree(y2~Age,uczace))
(treey2r<- rpart(y2~Age,uczace))
(treey2r<- rpart(y2~Age,uczace,control=rpart.control(minsplit = 1000, cp = 0.001)))
plot(treey2c)

table(Predict(treey2c))

#pytanie - jak zaklasyfikować wg nas, może więkse niż rzeczywiste prawdopodobieństwo?
summary(Predict(treey2c))

frakcja1<-summary(cases2$y1)
summary(cases2$y2)
(good<-frakcja1[2]/(frakcja1[1]+frakcja1[2]))

uczace$tree_age<-ifelse(Predict(treey2c)>=0.24,1,0)

m_klas_age<-prop.table(table(uczace$tree_age,uczace$y2))


table(cases2$y2)
#true negative TN
(tn<-m_klas_age[1,1])
#true positive TP
(tp<-m_klas_age[2,2])
#false negative FN
(fn<-m_klas_age[1,2])
#false positive FP
(fp<-m_klas_age[2,1])


#czułość i specyficzność
#SE (sensitivity, czułość) – określa zdolność klasyfikatora do wykrywania klasy pozytywnej 
#SE = TP / (TP + FN)
#SP (specificity, specyficzność) – określa zdolność klasyfikatora do wykrywania klasy negatywnej 
#SP = TN / (TN + FP)
#ACC (Total Accuracy) – całkowita sprawność klasyfikatora, określa prawdopodobieństwo poprawnej klasyfikacji, czyli stosunek poprawnych klasyfikacji do wszystkich klasyfikacji
#ACC = (TP + TN) / (TP + TN + FP + FN)

(SE <-tp / (tp + fn))
(SP = tn / (tn + fp))
(ACC = (tp + tn) / (tp + tn + fp + fn))
#czy lepszy niż wszystko do złej? (tak zwróciło drzewo na G, B)
table(cases2$y1,Predict(treey1c))
(acc_naive=(0+74526)/(74526+23547))
#ale
se=0
#dyskusja co lepsze - to zależy od zjawiska biznesowego:)

#########################################
#ZAD 7. Stwórz odpowiednie najlepsze (wg. macierzy klasyfikacji dla klasyfikacji i wg. błędu średniokwadratowego dla regresji) drzewo (odpowiednio do Y klasyfikacyjne, regresyjne) dla każdej ze zmiennych Y1, Y2, Y3 wybraną przez siebie metodą (rpart, ctree) wykorzystując tylko dane ze zbioru uczącego. Zadbaj o to, by drzewo nie było przeuczone.
########################################


#budujemy wyłącznie na zbiorze uczącym!!!
###ad y1
summary(uczace)
names(uczace)
#zostawiamy tylko y1 i usuwamy id sprawy
names(uczace[,-c(1,21,23,24)])
learn_Set<-uczace[,-c(1,21,23,24)]
test_set<-testowe[,-c(1,21,23,24)]
names(learn_Set)

(treey7<- ctree(y1~Age,learn_Set)) 
(treey1c_big<- ctree(y1~.,learn_Set)) 
plot(treey1c_big)

table(Predict(treey1c_big),learn_Set$y1)

m_klas<-table(Predict(treey1c_big),learn_Set$y1)

table(cases2$y2)

(tn<-m_klas[1,1])
(tp<-m_klas[2,2])
(fn<-m_klas[1,2])
(fp<-m_klas[2,1])


(SE <-tp / (tp + fn)) 
(SP = tn / (tn + fp)) 
(ACC = (tp + tn) / (tp + tn + fp + fn)) 


(treey1r_big<- rpart(y1~.,learn_Set))
(treey1r_big<- rpart(y1~.,learn_Set,control=rpart.control(minsplit = 1000, cp = 0.001)))

plot(treey1r_big)
text(treey1r_big,use.n = TRUE)

(m_klas<-table(predict(treey1r_big,type="class"),learn_Set$y1))

(tn<-m_klas[1,1])
(tp<-m_klas[2,2])
(fn<-m_klas[1,2])
(fp<-m_klas[2,1])


(SE <-tp / (tp + fn))
(SP = tn / (tn + fp)) 
(ACC = (tp + tn) / (tp + tn + fp + fn)) 


###porownanie ze zbiorem testowym
#length(predict(treey1r_big, newdata=test_set, type="class"))
(m_klas_test<-table(predict(treey1r_big, newdata=test_set, type="class"),test_set$y1))
(tn_t<-m_klas_test[1,1])
(tp_t<-m_klas_test[2,2])
(fn_t<-m_klas_test[1,2])
(fp_t<-m_klas_test[2,1])

(SE_t <-tp_t / (tp_t + fn_t))
(SP_t = tn_t / (tn_t + fp_t)) 
(ACC_t = (tp_t + tn_t) / (tp_t + tn_t + fp_t + fn_t)) 


length(Predict(treey1c_big, newdata=test_set))
(m_klas_test<-table(Predict(treey1c_big, newdata=test_set),test_set$y1))

###ad y2
#zostawiamy tylko y2 i usuwamy id sprawy
names(uczace)
names(uczace[,-c(1,21,22,24)])
learn_Set<-uczace[,-c(1,21,22,24)]
test_set<-testowe[,-c(1,21,22,24)]
names(learn_Set)

(treey2r_big<- rpart(y2~.,learn_Set))
(treey2r_big<- rpart(y2~.,learn_Set,control=rpart.control(minsplit = 1000, cp = 0.001)))
predict(treey2r_big)

(frakcja1<-summary(cases2$y2))
frakcja1[4]

learn_Set$tree_predict<-ifelse(predict(treey2r_big)>=0.5,1,0)
(m_klas<-table(learn_Set$tree_predict,learn_Set$y2))
(tn<-m_klas[1,1])
(tp<-m_klas[2,2])
(fn<-m_klas[1,2])
(fp<-m_klas[2,1])


(SE <-tp / (tp + fn))
(SP = tn / (tn + fp)) 
(ACC = (tp + tn) / (tp + tn + fp + fn)) 


test_set$tree_predict<-ifelse(predict(treey2r_big, newdata=test_set)>=0.5,1,0)
(m_klas_test<-table(test_set$tree_predict,test_set$y2))
(tn_t<-m_klas_test[1,1])
(tp_t<-m_klas_test[2,2])
(fn_t<-m_klas_test[1,2])
(fp_t<-m_klas_test[2,1])

(SE_t <-tp_t / (tp_t + fn_t))
(SP_t = tn_t / (tn_t + fp_t)) 
(ACC_t = (tp_t + tn_t) / (tp_t + tn_t + fp_t + fn_t)) 

#ad y3

names(uczace)
names(uczace[,-c(1,21,22,23)])
learn_Set<-uczace[,-c(1,21,22,23)]
test_set<-testowe[,-c(1,21,22,23)]
names(learn_Set)

(treey3r_big<- rpart(y3~.,learn_Set))
(treey3r_bigger<- rpart(y3~.,learn_Set,control=rpart.control(minsplit = 1000, cp = 0.001)))
plot(treey3r_bigger)
text(treey3r_bigger,use.n = TRUE)

predict(treey3r_bigger)

library(hydroGOF)
mse(learn_Set$y3, predict(treey3r_bigger))
#694822.8
plot(learn_Set$y3)
points(predict(treey3r_bigger),col="red")

mse(test_set$y3, predict(treey3r_bigger,newdata = test_set))
#641127.5

(treey3c_big<- ctree(y3~.,learn_Set))
plot(treey3c_big)
plot(learn_Set$y3)
points(Predict(treey3c_big),col="red")
