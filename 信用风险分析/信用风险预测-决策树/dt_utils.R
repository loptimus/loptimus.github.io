# 安装所需的package，并载入
packages <- c(
  "ggplot2",              # 数据可视化
  "caret",                # 分类和回归训练
  "rpart",                # 树模型
  "rpart.plot",           # 树模型可视化
  "ROCR"                  # ROC曲线
)

# 载入指定的package
load.package <- function(package){
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    if(require(package, character.only = TRUE)){
      message(package, " installed and loaded")
    }
  }
}

# 载入所需的package
lapply(packages, load.package)

## 标准化和归一化
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]])[, 1]
  }
  return(df)
}

# 根据混淆矩阵计算F1 score
calc.f1.score <- function(confusionMatrix){
  df <- as.data.frame(confusionMatrix$table)
  tp <- df[which(df$Prediction == 1 & df$Reference == 1), "Freq"]
  fp <- df[which(df$Prediction == 1 & df$Reference == 0), "Freq"]
  fn <- df[which(df$Prediction == 0 & df$Reference == 1), "Freq"]
  round(2 * tp / (2 * tp + fp + fn), 4)
}

# 绘制ROC曲线
plot.roc.curve <- function(predictions, title.text = "ROC", ...){
  perf <- performance(predictions, "tpr", "fpr")
  plot.act(perf, main = title.text, ...)
  abline(0, 1, col = "red")
  auc <- performance(predictions, "auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc, 2)
  legend(0.4, 0.4, legend = c(paste0("AUC: ", auc)), 
         cex = 0.8, bty = "n", box.col = "white"
  )
}

# 绘制PR曲线
plot.pr.curve <- function(
  predictions, title.text = "prec/rec curve", ...
){
  perf <- performance(predictions, "prec", "rec")
  plot.act(perf, main = title.text, ...)
}

# 绘图操作
plot.act <- function(
  performance, family = "黑体", 
  col = "black", lwd = 2, lty = 1, 
  cex.main = 0.8, cex.lab = 0.8,
  xaxs = "i", yaxs = "i", ...
){
  plot(performance, family = family, 
       col = col, lty = lty, lwd = lwd, 
       cex.main = cex.main, cex.lab = cex.lab, 
       xaxs = xaxs, yaxs = yaxs, ...
  )
}