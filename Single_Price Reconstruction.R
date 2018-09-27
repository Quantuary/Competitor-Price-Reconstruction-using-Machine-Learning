
library(gbm)

# df is the dataframe object

# train test split
smp_size <- floor(0.8 *nrow(df))
set.seed(42)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind,]
test <- df[-train_ind,]

# model function
rm(feature)
model <- function(df, feature=NULL, cv=3){
  
  if(is.null(feature)){
                      feature <- names(df)[!(names(df) %in% 'Premium') ]
                      }
  else {
        feature <- feature$var  
  }
  b <- paste(feature, collapse ="+")
  
  # GBM
  model_gbm <- gbm(as.formula(paste("Premium~",b,sep="")
                              ),
                   data = df,
                   distribution = "gaussian",
                   interaction.depth = 3,
                   shrinkage = 0.2,
                   n.minobsinnode = 1,
                   n.trees= 10000,
                   cv.folds=cv,
                   verbose = FALSE)
  return (model_gbm)
}

# Fit and find best iter
fitted_model <- model(train)
best.iter <- gbm.perf(fitted_model, method ="cv",overlay = TRUE)

# Train Score
score <- function(df1) {
  output <- predict(fitted_model, df1, best.iter)
  error <- abs(df1$Premium - output)
  mape <- mean( 100 * (error / df1$Premium))
  accuracy <- 100 - mape
  print (accuracy)
  print (mape) 
}
score(train)

# Filter for best parameter and retrain
feature <- summary.gbm(fitted_model)
feature <- feature[feature$rel.inf > 0.1,]
fitted_model <- model(train, feature=feature, cv=0) # use best parameter for learning rate

# replot feature importance
feature_importance <- function (){
  ggplot(feature, aes(x=reorder(var,rel.inf), y=rel.inf,fill=rel.inf))+ 
    geom_bar(stat="identity", position="dodge")+ coord_flip()+
    ylab("Relative Influence")+
    xlab("")+
    ggtitle("Feature Importance")+
    guides(fill=F)+
    scale_fill_gradient(low="red", high="blue")
  
}
feature_importance()

# test model
score(test)

