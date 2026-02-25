df_re <- read.csv("data_new.csv")
table(Train$Result)
str(Train$Result)

########################## Read data #########################################################
# data=read.csv("baseline_imputed_final.csv",header = T,encoding = "GBK")
data <- df_re
data <- rename(data, Townsend_deprivation = 'Townsend.deprivation')
str(data)
# Factorize variables: only categorical variables are factorized (set dummy variables), and modify predictor names
data$PAD_new = factor(data$PAD_new, levels = c(0,1), labels = c('No','Yes'))    # Result encoding must NOT be changed

# Multivariate significant variables (if multivariate Logistic has few significant variables, use univariate significant variables)
# Factorize variables: only categorical variables are factorized (set dummy variables), and modify predictor names
# data$PAD_new = factor(data$PAD_new,levels = c(0,1),labels = c('No','Yes'))    # Result encoding must NOT be changed

# Multivariate significant variables (if multivariate Logistic has few significant variables, use univariate significant variables)

var = c("PAD_new", "diabetes", "CVD", "stoke",                 
        "Cholesterol_lowering_med", "Blood_pressure_med", "Insulin_med",             
        "Age_at_recruitment", "HC", "Monocyte_count",         
        "Neutrophill_count", "Albumin", "Creatinine",             
        "C_reactive_protein", "HbA1c", "TyG_ABSI")

# Reload data to ensure all variables are numeric
# data=read.csv("data.csv",header = T,encoding = "GBK")
colnames(data)
# Only factorize the outcome variable
# data$PAD_new = factor(data$PAD_new,levels = c(0,1),labels = c('No','Yes'))

set.seed(520)
inTrain = createDataPartition(y = data[,"PAD_new"], p = 0.6, list = F)
traindata = data[inTrain,]
testdata = data[-inTrain,]
dev = traindata
vad = testdata
table(dev$PAD_new, useNA = "ifany")
# Extract significant variables
dev = dev[,var]
# Balance the training set with SMOTE
newData = SMOTE(PAD_new~., 
                dev, 
                k = 5, # Number of nearest neighbors for new minority instances
                perc.over = 200, # Percentage of new minority cases to generate
                perc.under = 300) # Majority class size per minority class case
dev = newData
table(dev$PAD_new)
table(df1$PAD_new)
vad = vad[,var]
table(vad$PAD_new)
dev$PAD_new = factor(as.character(dev$PAD_new)) # Change outcome variable name and factorize
# Train models
models = c("glm","gbm","nnet",#"xgbTree",
           "AdaBoost.M1") # Parameters
# Model names
models_names = list(Logistic = "glm", GBM = "gbm", NeuralNetwork = "nnet", #RandomForest="rf",
                    # Xgboost="xgbTree",
                    Adaboost = "AdaBoost.M1")

# # Parameter settings
# glm.tune.grid = NULL
# svm.tune.grid = expand.grid(sigma = 0.001, C = 0.09)
# gbm.tune.grid(n.trees = 100, interaction.depth = 3,shrinkage = 0.1, n.minobsinnode = 5)
# nnet.tune..gr_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",RandomForest="rf",Xgboost="xgbTree",KNN="kknn",Adaboost="AdaBoost.M1")#
# id = expandgrid = expand.grid(size = 6,decay = 0.6)
# rf.tune.grid = expand.grid(mtry = 11,numRandomCuts = 3)
# xgb.tune.grid = expand.grid(nrounds = 10,max_depth = 3,eta = 0.001,
#                             gamma = 0.5,colsample_bytree = 0.5,min_child_weight = 1,subsample = 0.6)
# knn.tune.grid = expand.grid(kmax = 12 ,distance = 1,kernel = "optimal")
# ada.tune.grid = expand.grid(mfinal = 2,maxdepth = 2,coeflearn = "Zhu")

# # Automatic parameter optimization
glm.tune.grid = NULL
#svm.tune.grid = NULL
gbm.tune.grid = NULL
nnet.tune.grid = NULL
#rf.tune.grid = NULL
#xgb.tune.grid = NULL
#knn.tune.grid = NULL
ada.tune.grid = NULL

# # Specify parameter grids for hyperparameter tuning
glm.tune.grid = NULL
#svm.tune.grid = expand.grid(sigma = c(0.1,0.01,0.001), C = c(0.1,0.5))
gbm.tune.grid = expand.grid(n.trees = 100, interaction.depth = c(2,3), shrinkage = c(0.1,0.01), n.minobsinnode = 5)
nnet.tune.grid = expand.grid(size = c(3:5), decay = 0.6)
#rf.tune.grid = expand.grid(mtry = c(1:2))
#xgb.tune.grid = expand.grid(nrounds = 10,max_depth = c(3:5),eta = c(0.1,0.01,0.001),
#gamma = 0.5,colsample_bytree = 0.5,min_child_weight = 1,subsample = 0.6)
#knn.tune.grid = expand.grid(kmax = c(3:15), distance = 1, kernel = "optimal")
ada.tune.grid = expand.grid(mfinal = 2, maxdepth = c(2:5), coeflearn = "Zhu")

Tune_table = list(glm = glm.tune.grid,
                  #svmRadial = svm.tune.grid,
                  gbm = gbm.tune.grid,
                  nnet = nnet.tune.grid,
                  #extraTrees = rf.tune.grid,
                  xgbTree = xgb.tune.grid,
                  #kknn = knn.tune.grid,
                  AdaBoost.M1 = ada.tune.grid
)

# Prediction results
train_probe = data.frame(Result = dev$PAD_new)
test_probe = data.frame(Result = vad$PAD_new)

# Variable importance
importance = list()

# All models
ML_calss_model = list()

set.seed(520)
train.control <- trainControl(method = 'repeatedcv',
                              number = 10, 
                              repeats = 5, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary)
# Progress bar
pb = txtProgressBar(min = 0, max = length(models), style = 3)
for (i in seq_along(models)) {
  model <- models[i]
  model_name <- names(models_names)[which(models_names == model)]  
  set.seed(52)
  fit = train(PAD_new~.,
              data = dev,
              tuneGrid = Tune_table[[model]],
              metric = 'ROC',
              method = model,
              trControl = train.control)
  
  train_Pro = predict(fit, newdata = dev, type = 'prob')
  test_Pro = predict(fit, newdata = vad, type = 'prob')
  
  train_probe[[model_name]] <- train_Pro$Yes
  test_probe[[model_name]] <- test_Pro$Yes
  
  ML_calss_model[[model_name]] = fit  # Store model by name
  importance[[model_name]] = varImp(fit, scale = TRUE)  # Store importance by name
  
  setTxtProgressBar(pb, i) # Update progress bar
}
close(pb)  # Close progress bar
#################################################################################
#################################################################################
####### Additional model
# 9. LightGBM
train = dev
train$PAD_new = ifelse(train$PAD_new=="Yes",1,0)
dtrain = lgb.Dataset(as.matrix(train[2:ncol(train)]), label = train$PAD_new)
test = vad[,var]
test$PAD_new = ifelse(test$PAD_new=="Yes",1,0)
dtest = lgb.Dataset.create.valid(dtrain, as.matrix(test[2:ncol(test)]), label = test$PAD_new)
params = list(
  objective = "binary", 
  metric = "auc", 
  min_data = 1L, 
  learning_rate = 1.0, 
  num_threads = 2L,
  force_col_wise = T)
valids = list(test = dtest)
lightgbm_model = lgb.train(params = params, data = dtrain,
                           nrounds = 10L, 
                           valids = valids, 
                           early_stopping_rounds = 5L)

train_probe$LightGBM = predict(lightgbm_model, newdata = as.matrix(dev[2:ncol(dev)]), type = 'prob')
test_probe$LightGBM = predict(lightgbm_model, newdata = as.matrix(vad[2:ncol(vad)]), type = 'prob')

lightGBM_Imp = lgb.importance(lightgbm_model, percentage = TRUE)
write.csv(lightGBM_Imp, "LightGBM_important.csv", row.names = F)
# Importance plot
rt = read.csv("LightGBM_important.csv", header = T, check.names = F)
g <- ggplot(rt, aes(x = Gain, y = reorder(Feature, Gain))) 
p2 = g + geom_bar(aes(fill = Gain), stat = "identity",
                 width = 0.6, position = position_stack(reverse = TRUE), size = 1) +
  theme_classic() +
  scale_fill_gradient() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = "none",
        axis.text = element_text(size = 10, face = "bold", color = "black"),
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  labs(x = "Importance Scores", y = "Features", title = "LightGBM")
p2
##############################################################################################
##############################################################################################
# 10. xgboost model
library(xgboost)
X <- as.matrix(dev[, setdiff(names(dev), "PAD_new")])
y <- as.numeric(factor(dev$PAD_new, levels = c("No","Yes"))) - 1

dtrain <- xgb.DMatrix(data = X, label = y)
xgb_model <- xgb.train(
  params = list(
    max_depth = 4,
    learning_rate = 0.1,
    min_split_loss = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8,
    objective = "reg:logistic"
  ),
  data = dtrain,
  nrounds = 10
)
library(xgboost)
# ...previously your XGBoost model code...

library(ggplot2)

importance <- xgb.importance(model = xgb_model, feature_names = colnames(X))
importance$Feature <- factor(importance$Feature, levels = rev(importance$Feature))

p <- ggplot(importance, aes(x = Gain, y = Feature, fill = Gain)) +
  geom_bar(stat = "identity", width = 0.6, 
           position = position_stack(reverse = TRUE), size = 1) +
  #scale_fill_gradient(low = "#BEEBE9", high = "#4696EC") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none",
    axis.text = element_text(size = 10, face = "bold", color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  labs(x = "Importance Scores", y = "Features", title = "XGBoost")

print(p)

# Prediction (ensure probability [0,1], and in same row order as train/dev)
train_probe$Xgboost <- predict(xgb_model, X)
Xt <- as.matrix(vad[, setdiff(names(vad), "PAD_new")])
test_probe$Xgboost <- predict(xgb_model, Xt)
#################################################################################################
# Plot variable importance for training set
for(model_name in names(models_names)){
  # Get variable importance
  imp = importance[[model_name]]
  # Convert result to data frame
  imp_table <- as.data.frame(imp$importance)
  imp_table$Features <- rownames(imp_table)
  # Determine which column to plot
  if ("Yes" %in% colnames(imp_table)) {
    fill_col <- "Yes"
  } else if ("Overall" %in% colnames(imp_table)) {
    fill_col <- "Overall"
  } else {
    stop("Neither 'Yes' nor 'Overall' column found in importance table.")
  }
  # Use ggplot2 for plotting
  g = ggplot(imp_table, aes(x = !!sym(fill_col), y = reorder(Features, !!sym(fill_col))))
  p2 = g + geom_bar(aes(fill = !!sym(fill_col)), stat = "identity", width = 0.6, position = position_stack(reverse = TRUE), size = 1) +
    theme_classic() + scale_fill_gradient() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none",
          axis.text = element_text(size = 10, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),
          legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
    labs(x = "Importance Scores", y = "Features", title = paste0(model_name, " "))
  
  pdf(paste0(model_name, "_important.pdf"), 7, 5, family = "serif")
  print(p2)
  dev.off()
}

models_names = list(Logistic="glm",GBM="gbm",NeuralNetwork="nnet",#RandomForest="rf",
                    Xgboost="xgbTree",Adaboost="AdaBoost.M1",
                    LightGBM = "LightGBM")#
#######################################################################################################################
########################################################################################################################
Train = train_probe
Test = test_probe
library(pROC)
library(ggplot2)
library(ggsci)

# Model names (should match column names in Train/Test)
models_to_plot = names(models_names)  # e.g., c("Logistic","GBM","NeuralNetwork",...)

ROC_list_train = list()
ROC_label_train = list()

for (model_name in models_to_plot) {
  ROC = roc(response = Train$Result, predictor = Train[[model_name]], levels = c("No","Yes"))
  AUC = round(auc(ROC), 3)
  CI = ci.auc(ROC)
  label = paste0(model_name, " (AUC=", AUC, ",95%CI:", round(CI[1],3), "-", round(CI[3],3), ")")
  ROC_label_train[[model_name]] = label
  ROC_list_train[[model_name]] = ROC
}

# Plot training ROC curves for all models
ROC_plot_train <- pROC::ggroc(ROC_list_train, size=1, legacy.axes = TRUE) +
  theme_bw() +
  labs(title = 'Train set ROC curve') +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
        axis.text = element_text(size=12, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=12, face="bold"),
        legend.position = c(0.7, 0.25),
        legend.background = element_blank(),
        axis.title.y = element_text(size=12, face="bold"),
        axis.title.x = element_text(size=12, face="bold"),
        panel.border = element_rect(color="black", size=1),
        panel.background = element_blank()) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash') +
  ggsci::scale_color_npg(
    breaks = models_to_plot,
    labels = ROC_label_train
  )

roc_df_list_train <- lapply(names(ROC_list_train), function(m) {
  df <- data.frame(
    specificity = ROC_list_train[[m]]$specificities,
    sensitivity = ROC_list_train[[m]]$sensitivities,
    Model = m
  )
  return(df)
})
roc_df_train <- do.call(rbind, roc_df_list_train)
roc_df_train$Model <- factor(roc_df_train$Model, levels = models_to_plot)

# Line types for ROC curves per model
model_linetypes <- c("solid",
                     "dashed",
                     "dotted",
                     "dotdash",
                     "longdash",
                     "twodash",
                     "dotted",   # duplicated for visual difference
                     "longdash")

# Visualize ROC curve for training set with different line types/colors by model
p <- ggplot(roc_df_train, aes(x=1-specificity, y=sensitivity, color=Model, linetype=Model)) +
  geom_line(size=1) +
  theme_bw() +
  labs(title = 'Train Set ROC curve') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
    axis.text = element_text(size=12, face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=10, face="bold"),
    legend.position = c(0.65, 0.25),
    legend.background = element_blank(),
    axis.title.y = element_text(size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    panel.border = element_rect(color="black", size=1),
    panel.background = element_blank()
  ) +
  scale_color_npg(
    breaks = models_to_plot,
    labels = ROC_label_train
  ) +
  scale_linetype_manual(
    values = model_linetypes,
    breaks = models_to_plot,
    labels = ROC_label_train
  ) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), inherit.aes = FALSE, colour='grey', linetype = 'dotdash')
##############################
dev.off() # Close all residual graphics devices
p    # Reprint the ggplot object

ROC_list_test = list()
ROC_label_test = list()

for (model_name in models_to_plot) {
  ROC = roc(response = Test$Result, predictor = Test[[model_name]], levels = c("No","Yes"))
  AUC = round(auc(ROC), 3)
  CI = ci.auc(ROC)
  label = paste0(model_name, " (AUC=", AUC, ",95%CI:", round(CI[1],3), "-", round(CI[3],3), ")")
  ROC_label_test[[model_name]] = label
  ROC_list_test[[model_name]] = ROC
}

library(pROC)
library(ggplot2)

# Suppose models_to_plot are your model names, ROC_list_test is the list of ROC objects for each model
roc_df_list_test <- lapply(names(ROC_list_test), function(m) {
  df <- data.frame(
    specificity = ROC_list_test[[m]]$specificities,
    sensitivity = ROC_list_test[[m]]$sensitivities,
    Model = m
  )
  return(df)
})
roc_df_test <- do.call(rbind, roc_df_list_test)
roc_df_test$Model <- factor(roc_df_test$Model, levels = models_to_plot)

# Line types for ROC curves per model (test set)
model_linetypes <- c("solid",
                     "dashed",
                     "dotted",
                     "dotdash",
                     "longdash",
                     "twodash",
                     "dotted",   # duplicated for visual difference
                     "longdash")

# Visualize ROC curve for test set with different line types/colors by model
p <- ggplot(roc_df_test, aes(x=1-specificity, y=sensitivity, color=Model, linetype=Model)) +
  geom_line(size=1) +
  theme_bw() +
  labs(title = 'Test Set ROC curve') +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
    axis.text = element_text(size=12, face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=10, face="bold"),
    legend.position = c(0.6, 0.25),
    legend.background = element_blank(),
    axis.title.y = element_text(size=12, face="bold"),
    axis.title.x = element_text(size=12, face="bold"),
    panel.border = element_rect(color="black", size=1),
    panel.background = element_blank()
  ) +
  scale_color_npg(
    breaks = models_to_plot,
    labels = ROC_label_test
  ) +
  scale_linetype_manual(
    values = model_linetypes,
    breaks = models_to_plot,
    labels = ROC_label_test
  ) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), inherit.aes = FALSE, colour='grey', linetype = 'dotdash')
################################# Model Metrics Calculation and Visualization#############
library(pROC)
library(caret)
library(dplyr)
library(MLmetrics)
library(pROC)

models_to_plot <- names(models_names)
results <- data.frame(Model = character(),
                      AUC = numeric(),
                      Cutoff = numeric(),
                      Accuracy = numeric(),
                      Specificity = numeric(),
                      Precision = numeric(),
                      Recall = numeric(),
                      F1 = numeric(),
                      Gmean = numeric(),
                      stringsAsFactors = FALSE)

for (model_name in models_to_plot) {
  pred_scores <- test_probe[[model_name]]
  true_label <- Test$Result    # "No", "Yes" as the class labels

  # 1. Compute ROC object
  roc_obj <- roc(response = true_label, predictor = pred_scores, levels = c("No", "Yes"), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))

  # 2. Manually construct the Youden Index table
  roc_df <- data.frame(
    threshold = roc_obj$thresholds,
    sens = roc_obj$sensitivities,
    spec = roc_obj$specificities
  )
  roc_df$youden = roc_df$sens + roc_df$spec - 1
  best_cutoff <- roc_df$threshold[which.max(roc_df$youden)]

  # 3. Apply the best cutoff to generate predicted labels
  pred_label <- ifelse(pred_scores >= best_cutoff, "Yes", "No")

  # 4. Confusion matrix and performance metrics
  cm <- confusionMatrix(factor(pred_label, levels = c("No", "Yes")),
                        factor(true_label, levels = c("No", "Yes")))
  acc <- cm$overall["Accuracy"]
  spec <- cm$byClass["Specificity"]
  prec <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]
  gmean <- sqrt(as.numeric(cm$byClass["Sensitivity"]) * as.numeric(cm$byClass["Specificity"]))

  # 5. Append results
  results <- rbind(results, data.frame(
    Model = model_name,
    AUC = round(auc_val, 3),
    Cutoff = round(best_cutoff, 3),
    Accuracy = round(acc, 3),
    Specificity = round(spec, 3),
    Precision = round(prec, 3),
    Recall = round(recall, 3),
    F1 = round(f1, 3),
    Gmean = round(gmean, 3)
  ))
}

print(results)

result_test_cutoff <- results
# Print all results
print(results)
####################
results <- results[,-c(3)]
library(tidyr)
# Use "Model" as group, other metrics as variables, colored bars
results_long <- results %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
my_colors <- c("#99cbeb","#4d97cd","#ce362d", "#f8984e","#459943","#a3d393",
               "#63187999")

library(ggplot2)
p <- ggplot(results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width=0.75) +
  theme_bw() +
  labs(y="Value", x="", title="Model Performance Comparison (Test set)") +
  theme(axis.text.x = element_text(angle=30, hjust=1, size=12),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=16, hjust=0.5, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=11)) +
  scale_fill_brewer(palette = "Set3")
print(p)


### train
library(pROC)
library(caret)

models_to_plot <- names(models_names)
train_results <- data.frame(Model = character(),
                            AUC = numeric(),
                            Cutoff = numeric(),
                            Accuracy = numeric(),
                            Specificity = numeric(),
                            Precision = numeric(),
                            Recall = numeric(),
                            F1 = numeric(),
                            Gmean = numeric(),
                            stringsAsFactors = FALSE)

for (model_name in models_to_plot) {
  pred_scores <- train_probe[[model_name]]
  true_label <- Train$Result    # Ensure the labels are consistent

  # 1. Compute ROC
  roc_obj <- roc(response = true_label, predictor = pred_scores, levels = c("No", "Yes"), direction = "<")
  auc_val <- as.numeric(auc(roc_obj))

  # 2. Manually find the best cutoff using maximum Youden index
  roc_df <- data.frame(
    threshold = roc_obj$thresholds,
    sens = roc_obj$sensitivities,
    spec = roc_obj$specificities
  )
  roc_df$youden = roc_df$sens + roc_df$spec - 1
  best_cutoff <- roc_df$threshold[which.max(roc_df$youden)]

  # 3. Use best cutoff to generate pred_label
  pred_label <- ifelse(pred_scores >= best_cutoff, "Yes", "No")

  # 4. Compute performance table
  cm <- confusionMatrix(factor(pred_label, levels=c("No","Yes")),
                        factor(true_label, levels=c("No","Yes")))
  acc <- cm$overall["Accuracy"]
  spec <- cm$byClass["Specificity"]
  prec <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- cm$byClass["F1"]
  gmean <- sqrt(as.numeric(cm$byClass["Sensitivity"]) * as.numeric(cm$byClass["Specificity"]))

  # 5. Collect results
  train_results <- rbind(train_results, data.frame(
    Model = model_name,
    AUC = round(auc_val,3),
    Cutoff = round(best_cutoff, 3),
    Accuracy = round(acc,3),
    Specificity = round(spec,3),
    Precision = round(prec,3),
    Recall = round(recall,3),
    F1 = round(f1,3),
    Gmean = round(gmean,3)
  ))
}
result_train_cutoff <- train_results
# Print all results
print(results)
####################
train_results <- train_results[,-c(3)]
## Visualization below does not need to change
train_results_long <- train_results %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
p_train <- ggplot(train_results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width=0.75) +
  theme_bw() +
  labs(y="Value", x="", title="Model Performance Comparison (Train set)") +
  theme(axis.text.x = element_text(angle=30, hjust=1, size=12),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=16, hjust=0.5, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=11)) +
  scale_fill_brewer(palette = "Set3")
print(p_train)

train_results_long <- train_results %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
p_train <- ggplot(train_results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width=0.75) +
  theme_bw() +
  labs(y="Value", x="", title="Model Performance Comparison (Train set)") +
  theme(axis.text.x = element_text(angle=30, hjust=1, size=12),
        axis.text.y = element_text(size=11),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=16, hjust=0.5, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=11)) +
  scale_fill_manual(values = my_colors)
print(p_train)
#### Test set: results
# Train set: train_results

write.csv(results, "model_metric_table_test.csv", row.names = FALSE)
write.csv(train_results, "model_metric_table_train.csv", row.names = FALSE)
# If excel is needed, you can use writexl package
# install.packages("writexl")
library(writexl)
write_xlsx(results, "model_metric_table_test.xlsx")
write_xlsx(train_results, "model_metric_table_train.xlsx")
library(knitr)
kable(results, caption = "Model performance metrics on Test set")
kable(train_results, caption = "Model performance metrics on Train set")
# Take TEST as an example
results$AUC_CI <- paste0("(", sapply(models_to_plot, function(x) {
  roc_obj <- roc(response = Test$Result, predictor = test_probe[[x]], levels = c("No","Yes"))
  ci <- ci.auc(roc_obj)
  sprintf("%.3f-%.3f", ci[1], ci[3])
}), ")")

results$AUC <- paste0(results$AUC, results$AUC_CI)
results$AUC_CI <- NULL
library(DT)
datatable(results, caption = "Test set metrics")
###
train_results$AUC_CI <- paste0("(", sapply(models_to_plot, function(x) {
  roc_obj <- roc(response = Train$Result, predictor = train_probe[[x]], levels = c("No","Yes"))
  ci <- ci.auc(roc_obj)
  sprintf("%.3f-%.3f", ci[1], ci[3])
}), ")")

train_results$AUC <- paste0(train_results$AUC, train_results$AUC_CI)
train_results$AUC_CI <- NULL
library(DT)
datatable(train_results, caption = "Train set metrics")
################
library(ggplot2)

# Extract model and variable names
final_logistic <- ML_calss_model$Logistic
# Extract glm model (caret training object type, use $finalModel directly)
coeff_table <- summary(final_logistic$finalModel)$coefficients

# Organize into a nice dataframe (remove intercept, take absolute value, order by importance)
imp_df <- data.frame(
  Feature = rownames(coeff_table)[-1],
  Coef    = coeff_table[-1, "Estimate"]
)
imp_df$Imp = abs(imp_df$Coef)
imp_df = imp_df[order(imp_df$Imp, decreasing = TRUE), ]

# Plot
ggplot(imp_df, aes(x = Imp, y = reorder(Feature, Imp))) +
  geom_bar(stat = "identity", fill = "#4575b4", width = 0.6) +
  theme_classic() +
  labs(x = "Absolute Coefficient", y = "Features",
       title = "Feature Importance (Logistic Regression)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold")
  )
# Save plot
ggsave("Logistic_FeatureImportance.pdf", width = 7, height = 5, family = "serif")



