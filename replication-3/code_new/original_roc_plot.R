# Trying to parallelize code
# running models ---------------------------------------------------------------
rf_model <- function(model, train_data, test_data) {
    train_model <- randomForest(model, type = regression, importance = TRUE,  
                                na.action = na.omit, ntree = 100000, 
                                maxnodes = 5,  sampsize = 100,  replace = FALSE, 
                                do.trace = FALSE, data = train_data, 
                                forest = TRUE)
    
    prediction <- as.numeric(predict(train_model, 
                                     newdata = data.matrix(test_data),
                                     type="response"))  
    prediction[is.na(test_data$test_DV_civil_ns)] <- NA
    return(prediction)
}


escal <- rf_model(model = escalation_train_formula_civil_ns, 
                       train_data = escalation_train_frame_civil_ns,
                       test_data = escalation_test_frame_civil_ns)

quad <- rf_model(model = quad_train_formula_civil_ns,
                 train_data = quad_train_frame_civil_ns,
                 test_data = quad_test_frame_civil_ns)

goldstein <- rf_model(model = goldstein_test_formula_civil_ns,
                      train_data = goldstein_test_frame_civil_ns,
                      test_data = goldstein_test_frame_civil_ns)

all_CAMEO <- rf_model(model = all_CAMEO_test_formula_civil_ns,
                      train_data = all_CAMEO_test_frame_civil_ns,
                      test_data = all_CAMEO_test_frame_civil_ns)


# ROC plot ---------------------------------------------------------------------
escal_roc <- roc(test_DV_civil_ns, escal, smooth=FALSE, auc = TRUE)
quad_roc <- roc(test_DV_civil_ns, quad, smooth=FALSE, auc = TRUE)
goldstein_roc <- roc(test_DV_civil_ns, goldstein, smooth=FALSE, auc = TRUE)
all_CAMEO_roc <- roc(test_DV_civil_ns, all_CAMEO, smooth=FALSE, auc = TRUE)
prediction <- as.numeric((escal + quad + goldstein + all_CAMEO) / 4)

avg_roc <- roc(test_DV_civil_ns, prediction, smooth=FALSE, auc = TRUE)

pdf("figures_new/original/original_roc_plot.pdf",
    width = 5.5, height = 5)
plot(escal_roc, 
     ylab = "True positive rate", 
     xlab = "False positive rate", 
     lty = 1)

lines.roc(quad_roc, lty = 2)
lines.roc(goldstein_roc, lty = 3)
lines.roc(all_CAMEO_roc, lty = 4)
lines.roc(avg_roc, lty = 5)

legend("bottomright",
       c("Escalation", "Quad", "Goldstein", "CAMEO", "Average"),
       lty = c(1, 2, 3, 4, 5))

dev.off()

