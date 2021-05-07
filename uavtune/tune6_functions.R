# Model selector and tune
resampleListFunction <- function(trainset.lst, mydata, train = TRUE, rem.cols = TRUE){
  if (train){
    sub.dt <- left_join(trainset.lst, mydata, 
                        by = c( "Transect", "wyd"))
  }else {
    sub.dt <- anti_join(mydata, trainset.lst, 
                        by = c( "Transect", "wyd"))
  }
  if (rem.cols){
    # Remove non predictor columns:
    sub.dt <- subset(sub.dt, select= -c(Transect, Distance, wyd))
  }
  return(sub.dt)
}

# Tunning function
tuneListFunction <- function(train.dt, cv_lst, myhypara.dt){
  
  # Create tunning hyperparameter variables
  for(i in 1:nrow(myhypara.dt)) { 
    nam <- as.character(myhypara.dt$hyperpara[i])
    assign(nam,  as.character(myhypara.dt$value[i]) ) 
  }
  model_string <- myhypara.dt$model_string[1]
  #
  # Specify predictor space
  # Count of predictor variables
  total_pred <- ncol(train.dt) -1
  # Predictor subsets for RFE tunning
  pred_subsets = floor(seq(2, total_pred-1, length = 10)) 
  #
  # Specify resampling strategy
  ctrl_tr <- caret::trainControl(# Grid CV:
                                 method = "cv",
                                 # Adaptive search
                                 # method = 'adaptive_cv',
                                 # adaptive = list(min=5, alpha=0.02, 
                                 #                 method='gls', complete=TRUE),
                                 index = cv_lst,
                                 returnResamp = "all",
                                 allowParallel = TRUE)
  #
  #' Tune  model
  if (model_string == "nnet"){
    library(nnet)
    #
    model_grid <- expand.grid(decay = eval(parse( text = decay_space)),
                              size = eval(parse( text = size_space)) )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 linout = TRUE) # regression output should be linear.
  }else if (model_string =="rf"){
    library(randomForest)
    model_grid <- expand.grid(mtry = eval(parse( text = mtry_space)))
    
    # ntree_value <- as.numeric(ntree_value) ##convert to numeric
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 importance = TRUE, # save var. importance
                                 #ntree = ntree_value,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
    
    
  }else if (model_string =="gbm"){
    library(gbm)
    model_grid <- expand.grid(interaction.depth = eval(parse( text = depth_space)),
                              n.trees = eval(parse( text = ntrees_space)),
                              shrinkage = eval(parse( text = shrink_space)),
                              n.minobsinnode = eval( parse(text =  node_space)) 
                              )
    
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 tuneGrid = model_grid,
                                 method = model_string,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
    
    
  }else if (model_string =="svmRadialCost"){
    library(kernlab)
    model_grid <- expand.grid(C = eval(parse( text = c_space))
                              )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)  
    
  }else if (model_string =="svmRadial"){
    library(kernlab)
    model_grid <- expand.grid(C = eval(parse( text = c_space)),
                              sigma = eval(parse( text = sigma_space)) )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
  }else if (model_string =="rvmRadial"){
    library(kernlab)
    model_grid <- expand.grid( sigma = eval(parse( text = sigma_space)))
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)  
    
  }else if (model_string =="xgbTree"){
    library(xgboost)
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      max_depth = eval(parse( text = max_depth_space)),
      gamma = eval(parse( text = gamma_space)),
      colsample_bytree = eval(parse( text = colsample_bytree_space)),
      min_child_weight = eval(parse( text = min_child_weight_space)),
      subsample = eval(parse( text = subsample_space))
      )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="xgbDART"){
    library(xgboost)
    
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      max_depth = eval(parse( text = max_depth_space)),
      gamma = eval(parse( text = gamma_space)),
      colsample_bytree = eval(parse( text = colsample_bytree_space)),
      min_child_weight = eval(parse( text = min_child_weight_space)),
      subsample = eval(parse( text = subsample_space)),
      rate_drop = eval(parse( text = rate_drop_space)),
      skip_drop = eval(parse( text = skip_drop_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="xgbLinear"){
    library(xgboost)
    
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      lambda = eval(parse( text = lambda_space)),
      alpha = eval(parse( text = alpha_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="neuralnet"){
    library(neuralnet)
    ### Does not work.
    model_grid <- expand.grid(
      layer1 = eval(parse( text = layer1_space)),
      layer2 = eval(parse( text = layer2_space)),
      layer3 = eval(parse( text = layer3_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 rep = 3,
                                 threshold = 0.1,
                                 #learningrate=0.05,
                                 #algorithm = 'backprob',
                                 linear.output = TRUE) # regression output should be linear.
  }else if (model_string =="dnn"){
    library(deepnet)
    ### Does not work.
    model_grid <- expand.grid(
      layer1 = eval(parse( text = layer1_space)),
      layer2 = eval(parse( text = layer2_space)),
      layer3 = eval(parse( text = layer3_space)),
      hidden_dropout = eval(parse( text = hidden_dropout_space)),
      visible_dropout = eval(parse( text = visible_dropout_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr) 
  }
  
  return(model_profile)
  
}


# Tunning function 2 for lapply
tuneListFunction2 <- function(train_cv_list, myhypara.dt){
  train.dt = train_cv_list[[1]]
  cv_lst =   train_cv_list[[2]]
  
  # Create tunning hyperparameter variables
  for(i in 1:nrow(myhypara.dt)) { 
    nam <- as.character(myhypara.dt$hyperpara[i])
    assign(nam,  as.character(myhypara.dt$value[i]) ) 
  }
  model_string <- myhypara.dt$model_string[1]
  #
  # Specify predictor space
  # Count of predictor variables
  total_pred <- ncol(train.dt) -1
  # Predictor subsets for RFE tunning
  pred_subsets = floor(seq(2, total_pred-1, length = 10)) 
  #
  # Specify resampling strategy
  ctrl_tr <- caret::trainControl(# Grid CV:
    method = "cv",
    # Adaptive search
    # method = 'adaptive_cv',
    # adaptive = list(min=5, alpha=0.02, 
    #                 method='gls', complete=TRUE),
    index = cv_lst,
    returnResamp = "all",
    allowParallel = TRUE)
  #
  #' Tune  model
  if (model_string == "nnet"){
    library(nnet)
    #
    model_grid <- expand.grid(decay = eval(parse( text = decay_space)),
                              size = eval(parse( text = size_space)) )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 linout = TRUE) # regression output should be linear.
  }else if (model_string =="rf"){
    library(randomForest)
    model_grid <- expand.grid(mtry = eval(parse( text = mtry_space)))
    
    # ntree_value <- as.numeric(ntree_value) ##convert to numeric
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 importance = TRUE, # save var. importance
                                 #ntree = ntree_value,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
    
    
  }else if (model_string =="gbm"){
    library(gbm)
    model_grid <- expand.grid(interaction.depth = eval(parse( text = depth_space)),
                              n.trees = eval(parse( text = ntrees_space)),
                              shrinkage = eval(parse( text = shrink_space)),
                              n.minobsinnode = eval( parse(text =  node_space)) 
    )
    
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 tuneGrid = model_grid,
                                 method = model_string,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
    
    
  }else if (model_string =="svmRadialCost"){
    library(kernlab)
    model_grid <- expand.grid(C = eval(parse( text = c_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)  
    
  }else if (model_string =="svmRadial"){
    library(kernlab)
    model_grid <- expand.grid(C = eval(parse( text = c_space)),
                              sigma = eval(parse( text = sigma_space)) )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)
  }else if (model_string =="rvmRadial"){
    library(kernlab)
    model_grid <- expand.grid( sigma = eval(parse( text = sigma_space)))
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr)  
    
  }else if (model_string =="xgbTree"){
    library(xgboost)
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      max_depth = eval(parse( text = max_depth_space)),
      gamma = eval(parse( text = gamma_space)),
      colsample_bytree = eval(parse( text = colsample_bytree_space)),
      min_child_weight = eval(parse( text = min_child_weight_space)),
      subsample = eval(parse( text = subsample_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #rfeControl = ctrl_rfe,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="xgbDART"){
    library(xgboost)
    
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      max_depth = eval(parse( text = max_depth_space)),
      gamma = eval(parse( text = gamma_space)),
      colsample_bytree = eval(parse( text = colsample_bytree_space)),
      min_child_weight = eval(parse( text = min_child_weight_space)),
      subsample = eval(parse( text = subsample_space)),
      rate_drop = eval(parse( text = rate_drop_space)),
      skip_drop = eval(parse( text = skip_drop_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="xgbLinear"){
    library(xgboost)
    
    model_grid <- expand.grid(
      nrounds = eval(parse( text = nrounds_space)),
      eta = eval(parse( text = eta_space)),
      lambda = eval(parse( text = lambda_space)),
      alpha = eval(parse( text = alpha_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 nthread = 1) # stop xgboost parallel processing
  }else if (model_string =="neuralnet"){
    library(neuralnet)
    ### Does not work.
    model_grid <- expand.grid(
      layer1 = eval(parse( text = layer1_space)),
      layer2 = eval(parse( text = layer2_space)),
      layer3 = eval(parse( text = layer3_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr,
                                 rep = 3,
                                 threshold = 0.1,
                                 #learningrate=0.05,
                                 #algorithm = 'backprob',
                                 linear.output = TRUE) # regression output should be linear.
  }else if (model_string =="dnn"){
    library(deepnet)
    ### Does not work.
    model_grid <- expand.grid(
      layer1 = eval(parse( text = layer1_space)),
      layer2 = eval(parse( text = layer2_space)),
      layer3 = eval(parse( text = layer3_space)),
      hidden_dropout = eval(parse( text = hidden_dropout_space)),
      visible_dropout = eval(parse( text = visible_dropout_space))
    )
    
    model_profile = caret::train(VWC~., data = train.dt,
                                 ## preProcess = c("center", "scale"),
                                 #sizes = pred_subsets,
                                 method = model_string,
                                 tuneGrid = model_grid,
                                 #Inner resampling method
                                 trControl = ctrl_tr) 
  }
  
  return(model_profile)
  
}
