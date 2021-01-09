
# Gradient boosted models ------------------------------------------------------

# n.cores = 4 as that's the max my CPU has. If you have more cores, 
# setting n.cores to a higher number will speed up the estimation. If you have
# less than 4 cores, you'll have to set it to a number that works with your 
# machine.

# defining the function  -------------------------------------------------------
gb_model <- function(model, train_data, test_data) {
    train_data <- as.data.frame(na.omit(train_data))
    train_model <- gbm(formula(model), n.tree = 100000, data = train_data, 
                       distribution = "bernoulli", n.cores = 4)
    prediction <- as.numeric(predict(train_model, 
                                     newdata = as.data.frame(test_data),
                                     type="response")) 
    prediction[is.na(test_data$test_DV_civil_ns)] <- NA
    return(prediction)
}
    

# estimating models ------------------------------------------------------------
escal_gbm <- gb_model(model = escalation_train_formula_civil_ns, 
                       train_data = escalation_train_frame_civil_ns,
                       test_data = escalation_test_frame_civil_ns)

#train_model <- gbm(formula = escalation_train_formula_civil_ns,
#    data = escalation_train_frame_civil_ns, n.tree = 100000, 
#    distribution = "bernoulli", n.cores = 4)
#    
#prediction <- as.numeric(predict(train_model, 
#                                 newdata = as.data.frame(escalation_test_frame_civil_ns),
#                                 type="response"))

quad_gbm <- gb_model(model = quad_train_formula_civil_ns,
                 train_data = quad_train_frame_civil_ns,
                 test_data = quad_test_frame_civil_ns)

goldstein <- gb_model(model = goldstein_test_formula_civil_ns,
                      train_data = goldstein_test_frame_civil_ns,
                      test_data = goldstein_test_frame_civil_ns)

all_CAMEO <- gb_model(model = all_CAMEO_test_formula_civil_ns,
                      train_data = all_CAMEO_test_frame_civil_ns,
                      test_data = all_CAMEO_test_frame_civil_ns)


# ROC plot ---------------------------------------------------------------------
escal_roc_gbm <- roc(test_DV_civil_ns, escal_gbm, smooth=FALSE, auc = TRUE)
quad_roc_gbm <- roc(test_DV_civil_ns, quad_gbm, smooth=FALSE, auc = TRUE)
goldstein_roc_gbm <- roc(test_DV_civil_ns, goldstein, smooth=FALSE, auc = TRUE)
all_CAMEO_roc_gbm <- roc(test_DV_civil_ns, all_CAMEO, smooth=FALSE, auc = TRUE)
prediction_gbm <- as.numeric((escal_gbm + quad_gbm) / 2)

avg_roc_gbm <- roc(test_DV_civil_ns, prediction_gbm, smooth=FALSE, auc = TRUE)

pdf("figures_new/gradient_boosted/gbm_roc_plot.pdf", width = 5.5, height = 5)
plot(escal_roc_gbm, 
     ylab = "True positive rate", 
     xlab = "False positive rate", 
     lty = 1)

lines.roc(quad_roc_gbm, lty = 2)
lines.roc(goldstein_roc_gbm, lty = 3)
lines.roc(all_CAMEO_roc_gbm, lty = 4)
lines.roc(avg_roc_gbm, lty = 5)

legend("bottomright",
       c("Escalation", "Quad", "Goldstein", "CAMEO", "Average"),
       lty = c(1, 2, 3, 4, 5))

dev.off()


# Comparing escalation models --------------------------------------------------
pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_escalation.pdf", 
    width = 5.5, height = 5)
plot(escal_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(escal_roc, lty = 2)

legend("bottomright",
       c("Escalation GBM", "Escalation RF"),
       lty = c(1, 2))

dev.off()


# Comparing quad models --------------------------------------------------------
pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_quad.pdf", 
    width = 5.5, height = 5)
plot(quad_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(quad_roc, lty = 2)

legend("bottomright",
       c("Quad GBM", "Quad RF"),
       lty = c(1, 2))

dev.off()


# Comparing goldstein models ---------------------------------------------------
pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_gold.pdf", 
    width = 5.5, height = 5)
plot(goldstein_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(goldstein_roc, lty = 2)

legend("bottomright",
       c("Goldstein GBM", "Goldstein RF"),
       lty = c(1, 2))

dev.off()


# Comparing CAMEO models ------------------------------------------------------
pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_CAMEO.pdf", 
    width = 5.5, height = 5)
plot(all_CAMEO_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(all_CAMEO_roc, lty = 2)

legend("bottomright",
       c("CAMEO GBM", "CAMEO RF"),
       lty = c(1, 2))

dev.off()


# Comparing averages -----------------------------------------------------------
pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_average.pdf", 
    width = 5.5, height = 5)
plot(avg_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(avg_roc, lty = 2)

legend("bottomright",
       c("Average GBM", "Average RF"),
       lty = c(1, 2))

dev.off()


prediction_all <- as.numeric((escal + quad + goldstein + all_CAMEO + 
                              escal_gbm + quad_gbm) / 6)
avg_roc_all <- roc(test_DV_civil_ns, prediction_all, smooth=FALSE, auc = TRUE)

pdf("figures_new/gradient_boosted/gbm_vs_rf_roc_average.pdf", 
    width = 5.5, height = 5)
plot(avg_roc_gbm,
     xlab = "True positive rate",
     ylab = "False positive rate",
     lty = 1)

lines.roc(avg_roc, lty = 2)
lines.roc(avg_roc_all, lty = 3)

legend("bottomright",
       c("Average GBM", "Average RF", "Agerage All"),
       lty = c(1, 2, 3))

dev.off()

