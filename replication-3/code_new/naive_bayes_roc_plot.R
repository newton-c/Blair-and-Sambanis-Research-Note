# Naive Bayes
nb_model <- function(model, train_data, test_data) {
    train_model <- naiveBayes(model, data = train_data)
    prediction <- as.numeric(predict(train_model, 
                                     newdata = as.data.frame(test_data))) 
    prediction[is.na(test_data$test_DV_civil_ns)] <- NA
    return(prediction)
}

escal_nb <- nb_model(escalation_train_formula_civil_ns,
                     train_data = escalation_train_frame_civil_ns,
                     test_data = escalation_test_frame_civil_ns)

quad_nb <- nb_model(escalation_train_formula_civil_ns,
                    train_data = escalation_train_frame_civil_ns,
                    test_data = escalation_test_frame_civil_ns)

goldstein_nb <- nb_model(escalation_train_formula_civil_ns,
                         train_data = escalation_train_frame_civil_ns,
                         test_data = escalation_test_frame_civil_ns)

all_CAMEO_nb <- nb_model(escalation_train_formula_civil_ns,
                         train_data = escalation_train_frame_civil_ns,
                         test_data = escalation_test_frame_civil_ns)

                       
escal_roc_nb <- roc(test_DV_civil_ns, escal, smooth=FALSE, auc = TRUE)
quad_roc_nb <- roc(test_DV_civil_ns, quad, smooth=FALSE, auc = TRUE)
goldstein_roc_nb <- roc(test_DV_civil_ns, goldstein, smooth=FALSE, auc = TRUE)
all_CAMEO_roc_nb <- roc(test_DV_civil_ns, all_CAMEO, smooth=FALSE, auc = TRUE)
prediction_nb <- as.numeric((escal + quad + goldstein + all_CAMEO) / 4)

avg_roc_nb <- roc(test_DV_civil_ns, prediction_nb, smooth=FALSE, auc = TRUE)


# Comparing escalation models --------------------------------------------------
pdf("figures_new/naive_bayes/nb_vs_rf_roc_escalation.pdf", 
    width = 5.5, height = 5)
plot(escal_roc_nb,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(escal_roc, lty = 2)

legend("bottomright",
       c("Quad NB", "Quad RF"),
       lty = c(1, 2))

dev.off()


# Comparing quad models --------------------------------------------------------
pdf("figures_new/naive_bayes/nb_vs_rf_roc_quad.pdf", width = 5.5, height = 5)
plot(quad_roc_nb,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(quad_roc, lty = 2)

legend("bottomright",
       c("Quad NB", "Quad RF"),
       lty = c(1, 2))

dev.off()
