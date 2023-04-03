library(ggplot2)
library(kernelshap)

# Turn ordinal factors into unordered
ord <- c("clarity", "color", "cut")
diamonds[, ord] <- lapply(diamonds[ord], factor, ordered = FALSE)

# Fit model
fit <- lm(log(price) ~ log(carat) * (clarity + color + cut), data = diamonds)

# Subset of 120 diamonds used as background data
bg_X <- diamonds[seq(1, nrow(diamonds), 450), ]

# Subset of 1018 diamonds to explain
X_small <- diamonds[seq(1, nrow(diamonds), 53), c("carat", ord)]

# Exact KernelSHAP (5 seconds)
system.time(
  ks <- kernelshap(fit, X_small, bg_X = bg_X)  
)
ks


system.time(
  ks <- kernelshap(glmnet_m, trainingSet1[,-1],
                   bg_X = trainingSet1,
                   type="prob"
                   )  
)
ks1 <- ks$S[[1]]

ks2 <- ks$S[[2]]

# SHAP values of first 2 observations:
#          carat     clarity     color        cut
# [1,] -2.050074 -0.28048747 0.1281222 0.01587382
# [2,] -2.085838  0.04050415 0.1283010 0.03731644

# Using parallel backend
library("doFuture")

registerDoFuture()
plan(multisession, workers = 2)  # Windows
# plan(multicore, workers = 2)   # Linux, macOS, Solaris

# 3 seconds on second call
system.time(

  ks3 <- kernelshap(glmnet_m, trainingSet1[,-1], 
                    bg_X = trainingSet1,type = "prob",
                    parallel = TRUE)  
)

# Visualization
library(shapviz)

sv <- shapviz(ks2,trainingSet1[,-1])
sv_importance(sv, "bee")
