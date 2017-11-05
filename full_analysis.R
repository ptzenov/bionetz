is_train_set <- do.call(c,lapply(names(driving_patterns_datastructs),function(i){
  kkk <-  rep(1,length(31:nrow( driving_patterns_datastructs[[i]]$pac_components )))
  kkk[-30:-1] <- 0
  kkk
}))


pca_per_thing <- do.call(rbind,lapply(names(driving_patterns_datastructs),function(i)
  driving_patterns_datastructs[[i]]$pac_components[31:nrow(
    driving_patterns_datastructs[[i]]$pac_components
  ),]))

raw_features_per_thing <- do.call(rbind,lapply(names(driving_patterns_datastructs),function(i)
  driving_patterns_datastructs[[i]]$raw_dataset[31:nrow(
    driving_patterns_datastructs[[i]]$pac_components
  ),.( ABSOLUTE_LOAD,RPM,INTAKE_PRESSURE,
                         INTAKE_TEMP,RELATIVE_THROTTLE_POS,
                         SPEED,THROTTLE_POS, MAF
       
       )]))


labels_thing <- do.call(c,lapply(names(driving_patterns_datastructs),function(i)
  rep(i,nrow(
    driving_patterns_datastructs[[i]]$pac_components)-30
  )))
dim(pca_per_thing)
length(labels_thing)


library(h2o)
h2o.init()
# tr_ind <- sample(1:nrow(pca_per_thing),floor(3/4*nrow(pca_per_thing)))
dframe <- as.data.table(
  cbind(as.data.table(pca_per_thing),label=labels_thing)
)
# 
# colnames(dframe)
# train_thing <- as.h2o(dframe[tr_ind,],destination_frame = "train_pca")
# test_thing <- as.h2o(dframe[-tr_ind,],destination_frame = "test_pca")
# 
# x <- colnames(train_thing) %>% tail(-1)
# y <- "label"
# 
# automl_thing <- h2o.automl(
#   x=colnames(train_thing) %>% head(-1),
#   y="label",
#   training_frame = train_thing,
#   validation_frame = test_thing,
#   max_runtime_secs = 60
#   # ,distribution = "multinomial"
# )




# xgboost multilabel ------------------------------------------------------
library(xgboost)
dat <- dframe
train_index <- sample(1:nrow(dat), nrow(dat)*0.75)
train_index <- is_train_set==1
# Full data set
data_variables <- as.matrix(dat[,.SD,.SDcols=colnames(dat) %>% head(-1)])
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)

data_label_factor <- dat[,label] %>% as.factor() 
data_label <- data_label_factor%>% as.integer()-1
labels_dt <- data.table(data_label,data_label_factor) %>% unique


# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
numberOfClasses <- length(unique(test_label))
xbg_params <- xgb_params <- list("objective" = "multi:softprob",
                                 "eval_metric" = "mlogloss",
                                 "num_class" = numberOfClasses)
nround <- 150

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = 5,
                   verbose = FALSE,
                   prediction = TRUE)


bst <- xgboost(data = train_matrix, params = xgb_params,nrounds = 150)

library(dplyr)
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label+1)
head(OOF_prediction)


caret::confusionMatrix(factor(OOF_prediction$label), 
                factor(OOF_prediction$max_prob),
                mode = "everything")

library(xgboost)
importance_matrix <- xgb.importance(colnames(train_matrix), model = bst)
importance_matrix


xgb.plot.importance(importance_matrix[1:10,], rel_to_first = TRUE, xlab = "Relative importance")
# (gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE))
# gg + ggplot2::ylab("Frequency")

importance_matrix_123 <- as.data.table(importance_matrix[1:2,"Feature"])

OOF_prediction_dt <- as.data.table(OOF_prediction)

OOF_prediction_dt[labels_dt,,on=.(label=data_label)][labels_dt,,on=.(max_prob=data_label)]-> oof_pred_2
oof_pred_2[,actual:=data_label_factor]
oof_pred_2[,predicted:=i.data_label_factor]

oof_pred_2 %>% cbind(train_data) %>% as.data.table()->oof_pred_2

oof_pred_2 %>% ggplot(aes(x=pca_1_RELATIVE_THROTTLE_POS_abs_gradient,group=actual,fill=actual))+geom_density(alpha=0.1)
oof_pred_2 %>% ggplot(aes(x=pca_1_RELATIVE_THROTTLE_POS_abs_gradient,group=predicted,fill=predicted))+geom_density(alpha=0.1)+
  scale_x_continuous(-0.1,0.1)
kkk_names <- c("predicted","actual",grep(x=colnames(oof_pred_2),pattern="pca",value=T))

oof_pred_2[,.SD,.SDcols=kkk_names]->
  predictions_pca_true_labels

fwrite(predictions_pca_true_labels,file=,"predictions_pca_true_labels.csv")

cmatr <- caret::confusionMatrix(factor(predictions_pca_true_labels$actual), 
                factor(predictions_pca_true_labels$predicted),
                mode = "everything")
cmatr$table

oof_pred_2 %>% ggplot(aes(x=pca_1_RELATIVE_THROTTLE_POS_abs_gradient,group=predicted,colour=predicted,y=pca_1_MAF,
                          colours=predicted))+geom_point()

# +
#   scale_x_continuous(-0.1,0.1)


# plot confusion matrix ---------------------------------------------------









# cluster plotting---------
library(cluster)
library(HSAUR)
data(pottery)
km    <- kmeans(pottery,3)
dissE <- daisy(pottery) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)

oof_pred_2[,.(predicted,actual,pca_1_THROTTLE_POS_abs_gradient,pca_1_MAF)]->tst


# km    <- kmeans(pottery,3)
dissE <- daisy(tst[,.(pca_1_THROTTLE_POS_abs_gradient,pca_1_MAF)]) 
dE2   <- dissE^2
sk2   <- silhouette(as.integer(tst$predicted), dE2)
plot(sk2)


library(cluster)
library(fpc)

raw_features_per_thing[train_index,] %>% cbind(
  tst
)->tst


scale(cmatr$table,center=F,scale = colSums((cmatr$table))) %>%  as.data.frame()->matrix_normlized

plot <- ggplot(matrix_normlized)
plot + geom_tile(aes(y=Prediction, x=Reference, fill=Freq)) + 
  scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + labs(fill="Normalized\nFrequency")



plotcluster(
  tst[,.(pca_1_THROTTLE_POS_abs_gradient,pca_1_MAF)]
  , tst$actual)



# plot confusion matrix ---------------------------------------------------

ggplot(tst)+geom_tile(aes(x=))





plotcluster(
  tst[,.(MAF,SPEED)]
  , tst$predicted)

plotcluster(
  tst[,.(MAF,RELATIVE_THROTTLE_POS)]
  , tst$actual)


tst[actual%in%c("gentle_drive_10min","aggresive_drive_10min","parking_2min")] %>% ggplot(
  aes(x=MAF,group=predicted,colour=predicted,y=RELATIVE_THROTTLE_POS,size=actual,
                          colours=predicted))+geom_point()


tst[actual%in%c("gentle_drive_10min","aggresive_drive_10min")] %>% ggplot(
  aes(x=pca_1_THROTTLE_POS_abs_gradient,group=predicted,
      colour=actual,y=pca_1_MAF,size=actual,
                          colours=predicted))+geom_point()

tst[] %>% ggplot(
  aes(x=actual,fill=actual,y=pca_1_MAF))+geom_boxplot()+guides(fill=F)+coord_flip()

tst[] %>% ggplot(
  aes(x=actual,fill=actual,y=MAF))+geom_boxplot()+guides(fill=F)+coord_flip()

tst[] %>% ggplot(
  aes(x=actual,fill=actual,y=RELATIVE_THROTTLE_POS))+geom_boxplot()+guides(fill=F)+coord_flip()

tst[] %>% ggplot(
  aes(x=actual,fill=actual,y=RPM))+geom_boxplot()+guides(fill=F)+coord_flip()

tst[] %>% ggplot(
  aes(x=actual,fill=actual,y=pca_1_THROTTLE_POS_abs_gradient))+geom_boxplot()+guides(fill=F)+coord_flip()

tst[actual%in%c("gentle_drive_10min","aggresive_drive_10min")] %>% ggplot(
  aes(x=actual,fill=actual,y=pca_1_MAF))+geom_boxplot()+guides(fill=F)+coord_flip()





# tst[] %>% ggplot(
#   aes(x=pca_1_MAF))+facet_grid(.~actual)+geom_histogram(bins=10)
#   geom_vline(aes(xintercept=mean(pca_1_MAF)), linetype="dashed", size=1, colour="red")