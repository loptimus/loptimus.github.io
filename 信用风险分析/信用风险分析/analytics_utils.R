# 所需的package
packages <- c(
  "mice",                 # 处理缺失值
  "ggplot2",              # 数据可视化
  "gridExtra",            # 图形网格布局
  "gmodels",              # 列联表
  "caret"                 # 分类和回归训练
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

# 将输入的data frame数据中指定的特征转换为factor类型
to.factors <- function(df, vars){
  for (var in vars) 
  {
    df[[var]] <- as.factor(df[[var]])
  }
  return(df)
}

# 获取特征名称
get.feature.name <- function(var.name){
  split.name <- strsplit(var.name, '$', fixed = TRUE)
  name.str <- split.name[[1]]
  name.str[length(name.str)]
}

# 单个特征x的直方图和密度图
visualize.distribution.x <- function(feature){
  var.name <- deparse(substitute(feature))
  feature.name <- get.feature.name(var.name)
  plot.hist <- 
    qplot(
      feature, geom = "histogram", fill = I("gray"), bins = 20,
      col = I("black")
    ) + 
    labs(title = paste(feature.name, "histogram"), x = feature.name) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  plot.density <- 
    qplot(
      feature, geom = "density", fill = I("gray"), 
      xlab = feature.name, col = I("black")
    ) + 
    ggtitle(paste(feature.name, "density")) + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  grid.arrange(plot.hist, plot.density, ncol = 2)
}

# y特征的因子水平与特征x的直方图和密度图
visualize.distribution.xy <- function(x.feature, y.feature) {
  x.var.name <- deparse(substitute(x.feature))
  x.feature.name <- get.feature.name(x.var.name)
  y.var.name <- deparse(substitute(y.feature))
  y.feature.name <- get.feature.name(y.var.name)
  df <- data.frame(x.feature, y.feature)
  colnames(df) <- c(x.feature.name, y.feature.name)
  ggplot(
    data = df, 
    aes_string(x = x.feature.name, fill = y.feature.name, colour = y.feature.name)
  ) + 
    geom_histogram(
      aes(y = ..density..), bins = 20,
      position = "identity", alpha = 0.5
    ) + 
    geom_density(alpha = 0.1) + 
    theme_bw()
}

# 箱线图
visualize.boxplot <- function(x.feature, y.feature){
  x.var.name <- deparse(substitute(x.feature))
  x.feature.name <- get.feature.name(x.var.name)
  y.var.name <- deparse(substitute(y.feature))
  y.feature.name <- get.feature.name(y.var.name)
  
  plot1 <- qplot(
    factor(""), x.feature, geom = "boxplot",
    xlab = x.feature.name,
    ylab = "values"
  ) + theme_bw()
  
  plot2 <- qplot(
    y.feature, x.feature, geom = "boxplot",
    xlab = y.feature.name,
    ylab = x.feature.name
  ) + theme_bw()
  grid.arrange(plot1, plot2, ncol = 2)
}

# 生成列联表
get.contingency.table <- function(
  y.feature, x.feature, 
  chisq.test = FALSE, fisher.test = FALSE
) {
  CrossTable(
    y.feature, x.feature,
    prop.r = FALSE, prop.t = FALSE, 
    prop.chisq = FALSE, chisq = chisq.test,
    fisher = fisher.test
  )
}

# 使用条形图来可视化因子变量的分布
visualize.barchart <- function(x.feature, y.feature = NULL){
  x.var.name <- deparse(substitute(x.feature))
  x.feature.name <- get.feature.name(x.var.name)
  x.stats.df <- as.data.frame(table(x.feature))
  colnames(x.stats.df) <- c(x.feature.name, "Freq")
  # 条形图的频数标注
  label.text <- geom_text(
    aes(label = Freq), vjust = -0.5, 
    colour = I("black"), position = position_dodge(.9), 
    size = 4
  )
  x.barchart <- 
    ggplot(
      data = x.stats.df, aes_string(x = x.feature.name, y = "Freq")
    ) + 
    geom_bar(
      stat = "identity", fill = I("gray"), col = I("black")
    ) +
    label.text + theme_bw()
  print(x.barchart)
  
  if (!is.null(y.feature)) {
    y.var.name <- deparse(substitute(y.feature))
    y.feature.name <- get.feature.name(y.var.name)
    df <- data.frame(x.feature, y.feature)
    df <- as.data.frame(table(df))
    colnames(df) <- c(x.feature.name, y.feature.name, "Freq")
    xy.barchart <- 
      ggplot(
        data = df, 
        aes_string(x = x.feature.name, y = "Freq", fill = y.feature.name)
      ) + 
      geom_bar(position = "dodge", stat = "identity") +
      label.text + theme_bw()
    print(xy.barchart)
  }
}
