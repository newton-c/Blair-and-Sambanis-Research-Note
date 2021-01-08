# Trying to parallelize code

# Escalation model -------------------------------------------------------------
train_model <- randomForest(escalation_train_formula_civil_ns,
    type = regression, importance = TRUE, 
    na.action = na.omit, ntree = 100000, maxnodes = 5, 
    sampsize = 100, 
    replace = FALSE, do.trace = FALSE , 
    data = escalation_train_frame_civil_ns, forest = TRUE   
    )	



prediction <- as.numeric(predict(train_model, 
    newdata = data.matrix(escalation_test_frame_civil_ns), type="response"))      								
prediction[is.na(escalation_test_frame_civil_ns$test_DV_civil_ns)] <- NA

roc <- roc(test_DV_civil_ns, prediction, smooth=TRUE, auc = TRUE)
roc_escalation_inc_civil_ns <- roc


# Quad model -------------------------------------------------------------------
train_model <- randomForest(quad_train_formula_civil_ns,
    type = regression, importance = TRUE, 
    na.action = na.omit, ntree = 100000, maxnodes = 5, 
    sampsize = 100, 
    replace = FALSE, do.trace = FALSE , 
    data = quad_train_frame_civil_ns, forest = TRUE   
    )			         

prediction <- as.numeric(predict(train_model, 
    newdata = data.matrix(quad_test_frame_civil_ns), type="response"))      								
prediction[is.na(quad_test_frame_civil_ns$test_DV_civil_ns)] <- NA

roc <- roc(test_DV_civil_ns, prediction, smooth=TRUE, auc = TRUE)
roc_quad_inc_civil_ns <- roc


# Goldstein model --------------------------------------------------------------
train_model <- randomForest(goldstein_train_formula_civil_ns,
    type = regression, importance = TRUE, 
    na.action = na.omit, ntree = 100000, maxnodes = 5, 
    sampsize = 100, 
    replace = FALSE, do.trace = FALSE , 
    data = goldstein_train_frame_civil_ns, forest = TRUE   
    )			         

prediction <- as.numeric(predict(train_model, 
    newdata = data.matrix(goldstein_test_frame_civil_ns), type="response"))      								
prediction[is.na(goldstein_test_frame_civil_ns$test_DV_civil_ns)] <- NA

roc <- roc(test_DV_civil_ns, prediction, smooth=TRUE, auc = TRUE)
roc_goldstein_inc_civil_ns <- roc

# All CAMEO model --------------------------------------------------------------
