M <- train(Activity ~ ., data = Dtr,  method = "pls", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)

M <- train(Activity ~ ., data = Dtr,  method = "knn", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)

M <- train(Activity ~ ., data = Dtr,  method = "mlp", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)

M <- train(Activity ~ ., data = Dtr,  method = "svmRadial", trControl = cctrl, tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)

M <- train(Activity ~ ., data = Dtr,  method = "svmPoly", trControl = cctrl, tuneLength = 3, metric=c("Accuracy"), na.action=na.exclude)

Grid <- expand.grid(.mtry=c(3, 5, 7,  9, 10), .ntree=c(20, 50, 100, 200, 300))
M <- train(Activity ~ ., data = Dtr,  method = customRF, trControl = cctrl,tuneGrid = Grid, metric=c("Accuracy"),na.action=na.exclude)

Grid <- expand.grid(nrounds = c(20, 50, 100, 200, 300),
                       max_depth = c(3, 5, 7,  9, 10), eta = c(0.1, 0.2, 0.3),
                       gamma = 0, colsample_bytree = .7, 
		       min_child_weight = 5,
                       subsample = 0.5)

M <- train(Activity ~ ., data = Dtr,  method = "xgbTree", trControl = cctrl,tuneGrid = Grid, metric=c("Accuracy"),na.action=na.exclude)
