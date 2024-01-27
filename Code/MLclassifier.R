cctrl <-trainControl(method = "cv", number = 10, classProbs = TRUE)
k = 10

############ Training and independent test #############################
nDes = scaler(feature, min = 0, max = 1)
D = data.frame(nDes, Activity = Class)
D = remove_empty(D, "cols")
pos <- subset(D, Activity == 'Pos')
neg <- subset(D, Activity == 'Neg')
ntrpos <- floor(0.8 * nrow(pos))
ntrneg <- floor(0.8 * nrow(neg))
tridpos <- sample(seq_len(nrow(pos)), size = ntrpos)
tridneg <- sample(seq_len(nrow(neg)), size = ntrneg)
set.seed(123)
trpos <- pos[tridpos , ]
trneg <- neg[tridneg, ]
tspos <- pos[-tridpos , ]
tsneg <- neg[-tridneg, ]
Dtr = rbind(trpos, trneg)
Dts = rbind(tspos, tsneg)
id <- sample(1:k,nrow(Dtr),replace=TRUE)

#############Cross-validation test #####################
prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = k, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- subset(Dtr, id !=   c(h))
test  <- subset(Dtr, id  ==  c(h))
M <- train(Activity ~ ., data = train,  method = "MLlibrary", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, na.omit(testsetCopy)))
result <- data.frame( prediction, na.omit(testsetCopy))
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

###########Independent  test#######################
M <- train(Activity ~ ., data = Dtr,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts) 
predprob <- predict(M, Dts, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts[,ncol(Dts)])))$auc
Dat <-  table(data.frame( pred3, Dts$Activity))
result <- data.frame( pred3, Dts$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))



