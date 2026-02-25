colnames(df1)
# (Recommended to install these packages)
#install.packages("factoextra")
# install.packages("cluster")

library(factoextra)
library(cluster)
# Or select K with the top 10 principal components (PCA, dimensionality reduction)

# Variable selection: can be customized, here is the recommended version
cluster_vars <- c(
  # Glucose metabolism
  "Glucose", "HbA1c",

  # Lipid metabolism
  "Cholesterol",  "LDL_direct", "Triglycerides",

  # Insulin resistance/metabolic indices
  "TyG", "mets_ir", "TyG_ABSI" # Pick two if space, "TyG" preferred if only one
)

# Step 1. Extract subset and remove rows with missing values
data_cluster <- na.omit(df1[,cluster_vars])

# Step 2. Standardize
data_scaled <- scale(data_cluster)

# Step 3. Select number of clusters (elbow plot)
# Use a subsample to help pick K
idx <- sample(1:nrow(data_scaled), 20000)
p <- fviz_nbclust(data_scaled[idx, ], kmeans, method = "wss")
# Add a red, dashed vertical line at k=4
p + geom_vline(xintercept = 4, linetype = "dashed",
               color = "#ED6663", size=0.4)

# For example: you select 4 clusters after visual inspection
set.seed(123)
km_result <- kmeans(data_scaled, centers = 4)

# Step 4. Add cluster results back to original data
df1$cluster <- NA
# Ensure that cluster assignment indices align
df1$cluster[as.numeric(rownames(data_cluster))] <- km_result$cluster

# Step 5. Check the number of samples in each cluster
table(df1$cluster)

# Step 6. 2D visualization of clusters
fviz_cluster(km_result, data = data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_classic())

# Step 7. Mean values and clinical characteristics by cluster
aggregate(df1[,cluster_vars], by=list(cluster=df1$cluster), FUN=mean, na.rm=TRUE)
library(survival)
library(survminer)

# install.packages("vegan")
# Load vegan package
library(vegan)

# Survival analysis: use K-means cluster as the grouping variable
fit_km <- survfit(Surv(follow_up_time, PAD_new) ~ cluster, data = df1)
n_clu <- length(unique(na.omit(df1$cluster)))
library(survminer)
# 1. Calculate log-rank test, get p-value text
logrank_test <- survdiff(Surv(follow_up_time, PAD_new) ~ cluster, data = df1)
p_val <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)
p_text <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", signif(p_val, 3)))
my_label <- paste("Log-rank test,", p_text)

# 2. Define colors and line types
palette_km <- c(
  "#D62728", # red
  "#1F77B4", # blue
  "#FF7F0E", # orange
  "#2CA02C", # green
  "#9467BD",  # purple
  "#8C564B"
)
line_types <- c("solid", "dashed", "dotted", "dotdash", "twodash", "longdash")

# 3. Survival curves (disable auto p-value)
p <- ggsurvplot(
  fit_km,
  data = df1,
  fun = "event",
  pval = FALSE,               # Disable auto p-value annotation
  risk.table =  FALSE,
  risk.table.col = "strata",
  legend.title = "Cluster",
  legend.labs = c("Isolated Dyslipidemia Group",
                  "Metabolically Healthy Group",
                  "Dyslipidemia with Insulin Resistance Group",
                  "Insulin Resistance/Glucose Dysfunction Group"#,
                  #"Cluster 5"#,"Cluster 6"
  ),
  palette = palette_km,
  linetype = line_types,
  size = 0.5,
  xlim = c(0, 15),
  ylim = c(0, 0.1),
  break.x.by = 3,
  censor = TRUE,
  ggtheme = theme_classic(base_size = 14),
  font.legend = list(face = "bold", size =10)
  #risk.table.fontsize = 3,
  #risk.table.height = 0.3,
  #risk.table.y.text.col = TRUE # Make risk table color match
)

# 4. Custom annotation of p-value on the plot
p$plot <- p$plot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 0.1)) +
  annotate("text", x =1, y = 0.045, label = my_label, size = 5, fontface = "bold", hjust = 0)
p$plot <- p$plot + theme(legend.position="right")

# 5. Show result
print(p)

# install.packages("fmsb")
library(dplyr)
library(fmsb)

# 1. Calculate the mean of each cluster
means_by_cluster <- df1 %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(across(all_of(cluster_vars), ~ mean(.x, na.rm=TRUE)))

# 2. Prepare data for fmsb input
radar_data <- rbind(
  apply(means_by_cluster[,-1], 2, max),
  apply(means_by_cluster[,-1], 2, min),
  as.matrix(means_by_cluster[,-1])
)
rownames(radar_data) <- c("max", "min", paste0("Cluster", means_by_cluster$cluster))
colnames(radar_data) <- cluster_vars # You can rename to Chinese if needed

# 3. Show each cluster separately
par(mfrow = c(1, 4))
cluster_colors <- c(
  "#D62728", # red
  "#1F77B4", # blue
  "#FF7F0E", # orange
  "#2CA02C", # green
  "#9467BD",  # purple
  "#8C564B"
)
par(mfrow = c(2, 3), mar = c(2, 5, 5, 2), oma = c(0,2,0,0))
group_names <- c("Isolated Dyslipidemia Group",
                 "Metabolically Healthy Group",
                 "Dyslipidemia with Insulin Resistance Group",
                 "Insulin Resistance/Glucose Dysfunction Group")
for (i in 1:4) {
  this_data <- as.data.frame(radar_data[c(1,2,2+i), ])
  radarchart(
    this_data,
    pcol = cluster_colors[i],
    pfcol = adjustcolor(cluster_colors[i], alpha.f=0.45),
    plwd = 2,
    cglcol = "gray", cglty = 1, axislabcol = "gray",
    vlcex =1.1
  )
  mtext(group_names[i], side = 3, line = 1, cex=1, font=2)
}
####################### Pairwise comparison of 4 groups
groups <- levels(factor(df1$cluster))
pair_list <- combn(groups, 2, simplify = FALSE) # All pairwise combinations
library(survival)
library(survminer)
library(ggplot2)

for (pair in pair_list) {
  # Subset for paired groups
  df_sub <- df1[df1$cluster %in% pair, ]
  df_sub$cluster <- factor(df_sub$cluster, levels = pair) # Ensure group order

  # Fit the survival curve
  fit_km_pair <- survfit(Surv(follow_up_time, PAD_new) ~ cluster, data = df_sub)

  # Log-rank test
  logrank_test_pair <- survdiff(Surv(follow_up_time, PAD_new) ~ cluster, data = df_sub)
  p_val_pair <- 1 - pchisq(logrank_test_pair$chisq, length(logrank_test_pair$n) - 1)
  p_text_pair <- ifelse(p_val_pair < 0.001, "p < 0.001", paste0("p = ", signif(p_val_pair, 3)))
  my_label_pair <- paste("Log-rank test,", p_text_pair)

  # Use your own palette for color
  pair_palette <- palette_km[which(groups %in% pair)]
  pair_linetype <- line_types[which(groups %in% pair)]

  # Plot
  p_pair <- ggsurvplot(
    fit_km_pair,
    data = df_sub,
    fun = "event",
    pval = FALSE,
    risk.table = FALSE,
    risk.table.col = "strata",
    legend.title = "Cluster",
    legend.labs = pair,
    palette = pair_palette,
    linetype = pair_linetype,
    size = 0.5,
    xlim = c(0, 17),
    #ylim = c(0, 0.1),
    break.x.by = 3,
    censor = TRUE,
    ggtheme = theme_classic(base_size = 12),
    font.legend = list(face = "bold", size =18)
    #risk.table.fontsize = 3,
    #risk.table.height = 0.3,
    #risk.table.y.text.col = TRUE
  )
  
  # Cox model (use pair[1] as reference)
  cox_fit <- coxph(Surv(follow_up_time, PAD_new) ~ cluster, data = df_sub)
  cox_sum <- summary(cox_fit)
  hr <- round(cox_sum$coefficients[1, "exp(coef)"], 2)
  lci <- round(cox_sum$conf.int[1, "lower .95"], 2)
  uci <- round(cox_sum$conf.int[1, "upper .95"], 2)
  pval <- signif(cox_sum$coefficients[1, "Pr(>|z|)"], 3)
  hr_label <- paste0("HR = ", hr, " (", lci, "-", uci, ") ")
  
  # Annotate
  p_pair$plot <- p_pair$plot +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    annotate("text", x = 0, y = 0.06, label = my_label_pair, size = 5, fontface = "bold", hjust = 0) +
    annotate("text", x = 0, y = 0.05, label = hr_label, size = 5, fontface = "bold", hjust = 0)
  
  print(p_pair)
  #ggsave(filename = paste0("KM_", pair[1], "_vs_", pair[2], ".pdf"), plot = p_pair$plot)
}

#############
# If fossil package not installed
# install.packages("fossil")
set.seed(123)
k <- 4 # Example, 4 clusters
data_scaled <- scale(data_cluster) # Standardized variables of interest
km_result <- kmeans(data_scaled, centers=k, nstart=100)
cluster_origin <- km_result$cluster
library(mclust)
n_boot <- 100
ari_boot <- numeric(n_boot)
for (i in 1:n_boot) {
  set.seed(10000 + i)
  idx <- sample(1:nrow(data_scaled), nrow(data_scaled), replace=TRUE)
  km_boot <- kmeans(data_scaled[idx, ], centers=k)
  cluster_boot_true <- rep(NA, nrow(data_scaled))
  cluster_boot_true[idx] <- km_boot$cluster
  idx_valid <- !is.na(cluster_boot_true)
  ari_boot[i] <- adjustedRandIndex(
    cluster_origin[idx_valid],
    cluster_boot_true[idx_valid]
  )
}
summary(ari_boot)
hist(ari_boot)
#############
# Install package if not installed
if(!requireNamespace("ConsensusClusterPlus")) {
  install.packages("BiocManager")
  BiocManager::install("ConsensusClusterPlus")
}
library(ConsensusClusterPlus)
data_mat <- as.matrix(data_scaled) # Convert to matrix
set.seed(10086)
idx <- sample(1:nrow(data_mat),2000)
data_sub <- data_mat[idx, ]
results <- ConsensusClusterPlus(
  data_sub,
  maxK = 7,           # Recommended 3~6
  reps = 100,         # 50 is fine
  pItem = 1,          # 80% bootstrapping, here 1 means all items
  pFeature = 1,       # Use all variables
  clusterAlg = "km",  # Kmeans is fastest
  distance = "euclidean",
  seed = 1234,
  plot = "png"
  #parallel = TRUE
)
library(ConsensusClusterPlus)
library(reshape2)
library(ggplot2)

K_range <- 2:6  # If maxK=8, use 2:8; here use 2:7 as example
cluster_consensus_list <- list()

for (k in K_range) {
  conc_mat <- results[[k]]$consensusMatrix
  group <- results[[k]]$consensusClass
  clusters <- unique(group)
  for (cl in clusters) {
    idx <- which(group == cl)
    if (length(idx) > 1) {
      conc_score <- mean(conc_mat[idx, idx][upper.tri(conc_mat[idx, idx])])
      cluster_consensus_list[[length(cluster_consensus_list) + 1]] <-
        data.frame(K = k,
                   cluster = cl,
                   consensus = conc_score)
    }
  }
}
dat2plot <- do.call(rbind, cluster_consensus_list)

ggplot(dat2plot, aes(x = as.factor(K), y = consensus, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "K", y = "Average consensus score", title = "cluster-consensus") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))
#########
# Replace NA (no sample in cluster) with 0
dat2plot$consensus[is.na(dat2plot$consensus)] <- 0
# Add a new column for fill color
dat2plot$fillcolor <- ifelse(dat2plot$consensus == 0, "empty", as.character(dat2plot$cluster))
library(RColorBrewer)
maxK <- max(dat2plot$K)  # Or max(K_range)
cluster_colors <- setNames(RColorBrewer::brewer.pal(maxK, "Set2"), as.character(1:maxK))

ggplot(dat2plot, aes(x = as.factor(K), y = consensus, fill = fillcolor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.65) +
  labs(x = "K", y = "Average consensus score", title = "cluster-consensus") +
  scale_fill_manual(
    values = c(cluster_colors, "empty" = "grey80"),
    breaks = c(as.character(1:maxK), "empty"),
    labels = c(as.character(1:maxK), "Empty"),
    name = "Cluster"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))
table(results[[4]]$consensusClass)
data_mat <- as.matrix(data_scaled) # convert to matrix
set.seed(10086)
idx <- sample(1:nrow(data_mat), 50000)
data_sub <- data_mat[idx, ]
results <- ConsensusClusterPlus(
  data_sub,
  maxK = 7,           # Recommended 3~6
  reps = 50,          # 50 is fine
  pItem = 1,          # 80% bootstrapping, here 1 means all items
  pFeature = 1,       # Use all variables
  clusterAlg = "km",  # Kmeans is fastest
  distance = "euclidean",
  seed = 1234,
  plot = "png"
  #parallel = TRUE
)
library(ConsensusClusterPlus)
library(reshape2)
library(ggplot2)

K_range <- 2:6  # If maxK=8, use 2:8; here for example 2:7
cluster_consensus_list <- list()

for (k in K_range) {
  conc_mat <- results[[k]]$consensusMatrix
  group <- results[[k]]$consensusClass
  clusters <- unique(group)
  for (cl in clusters) {
    idx <- which(group == cl)
    if (length(idx) > 1) {
      conc_score <- mean(conc_mat[idx, idx][upper.tri(conc_mat[idx, idx])])
      cluster_consensus_list[[length(cluster_consensus_list) + 1]] <-
        data.frame(K = k,
                   cluster = cl,
                   consensus = conc_score)
    }
  }
}
dat2plot <- do.call(rbind, cluster_consensus_list)

ggplot(dat2plot, aes(x = as.factor(K), y = consensus, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "K", y = "Average consensus score", title = "cluster-consensus") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))
#########
# Replace NA (no sample in this cluster) with 0
dat2plot$consensus[is.na(dat2plot$consensus)] <- 0
# Add a column as fill color
dat2plot$fillcolor <- ifelse(dat2plot$consensus == 0, "empty", as.character(dat2plot$cluster))
library(RColorBrewer)
maxK <- max(dat2plot$K)
cluster_colors <- setNames(RColorBrewer::brewer.pal(maxK, "Set2"), as.character(1:maxK))

ggplot(dat2plot, aes(x = as.factor(K), y = consensus, fill = fillcolor)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.65) +
  labs(x = "K", y = "Average consensus score", title = "cluster-consensus") +
  scale_fill_manual(
    values = c(cluster_colors, "empty" = "grey80"),
    breaks = c(as.character(1:maxK), "empty"),
    labels = c(as.character(1:maxK), "Empty"),
    name = "Cluster"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))
table(results[[4]]$consensusClass)
print(dim(data_mat))   # The original total size of your data
print(dim(data_sub))   # Actual sampled data size for Kmeans
summary(data_sub)
apply(data_sub, 2, sd)  # Check column standard deviation, if almost all zero
for (k in 2:7) {
  print(table(results[[k]]$consensusClass))
}
sum(is.na(data_sub))
ncol(data_sub)
#############
results <- ConsensusClusterPlus(
  t(data_sub),        # <-------- t(): transpose
  maxK = 7,
  reps = 50,
  pItem = 0.6,
  pFeature = 1,
  clusterAlg = "km",
  distance = "euclidean",
  seed = 1234,
  plot = "png"
)
table(results[[7]]$consensusClass)
library(ConsensusClusterPlus)
library(reshape2)
library(ggplot2)

K_range <- 2:7  # If maxK=8, can do 2:8; here use 2:7 as example
cluster_consensus_list <- list()

for (k in K_range) {
  conc_mat <- results[[k]]$consensusMatrix
  group <- results[[k]]$consensusClass
  clusters <- unique(group)
  for (cl in clusters) {
    idx <- which(group == cl)
    if (length(idx) > 1) {
      conc_score <- mean(conc_mat[idx, idx][upper.tri(conc_mat[idx, idx])])
      cluster_consensus_list[[length(cluster_consensus_list) + 1]] <-
        data.frame(K = k,
                   cluster = cl,
                   consensus = conc_score)
    }
  }
}
dat2plot <- do.call(rbind, cluster_consensus_list)
my_colors <- c("#99cbeb","#4d97cd","#ce362d", "#f8984e","#459943","#a3d393",
               "#63187999")
ggplot(dat2plot, aes(x = as.factor(K), y = consensus, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "K", y = "Average consensus score", title = "cluster-consensus") +
  scale_fill_manual(values=my_colors[1:length(unique(dat2plot$cluster))], name="Cluster") + 
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5))
library(DT)

# dat2plot should include K, cluster, consensus columns
# Make a clean wide table:
consensus_table <- dat2plot[, c("K", "cluster", "consensus")]

# You can add cluster sample size if you have it
# consensus_table$size <- ... 

# Display: K, cluster, consensus score
datatable(
  consensus_table,
  caption = "Consensus score for each cluster at different K",
  options = list(
    pageLength = 10,
    searching = FALSE,
    lengthChange = FALSE
  ),
  rownames = FALSE
)

library(reshape2)  # Recommend just using reshape2
consensus_table <- dat2plot[, c("K", "cluster", "consensus")]
tab_wide <- reshape2::dcast(consensus_table, K ~ cluster, value.var = "consensus")
tab_wide_3 <- tab_wide
tab_wide_3[,-1] <- round(tab_wide_3[,-1], 3)

library(DT)
datatable(
  tab_wide_3,
  caption = "Cluster consensus score by K (rounded to 3 decimals)",
  options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE),
  rownames = FALSE
)

datatable(tab_wide_3,
          caption = "Cluster consensus score by K (rounded to 3 decimals)",
          options = list(dom = 't', pageLength = 10, searching = FALSE, lengthChange = FALSE, order = list(list(0, 'asc'))),
          rownames = FALSE
) %>%
  formatRound(columns = 2:ncol(tab_wide_3), digits = 3) %>%
  formatStyle(
    columns = 2:ncol(tab_wide_3),
    backgroundColor = styleInterval(c(0.6, 0.7), c('#f5f5f5', '#ffffcc', '#e9746a')),
    'font-weight' = 'bold',
    'text-align' = 'center',
    color = 'black'
  )
############
install.packages("fpc")
library(fpc)
set.seed(123)
cb_result <- clusterboot(data_scaled, B=100, 
                         clustermethod = kmeansCBI, 
                         k = 4, 
                         seed=123)  # k=4 as the target cluster number

# Check Jaccard index (stability metric) for each cluster
print(cb_result$bootmean)

# Result interpretation:
# Generally, Jaccard index > 0.75 indicates a stable cluster
