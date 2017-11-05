# source("load_data_dataset.R")


if(!require(pacman)){
  install.packages("pacman")
}

pacman::p_load("readr","data.table","magrittr","stringr","dataMaid",
               "magrittr")



#' Title
#'
#' @param dataset_cleaned  the dataset
#' @param window_size window size
#' @param num_pca obv
#' @param forget_factor see the f argument in online'PCA::incRpca 
#' @param initial_window - the initial window- we'll have 0s there...
#'
#' @return
#' @export
#'
#' @examples
get_list_ranks <- function(
  dataset_cleaned,
  window_size=30,
  num_pca=3,
  forget_factor=1/256,# check this re:sampling rate
  initial_window=30
) {
  pacman::p_load("onlinePCA","data.table")
  
  
numeric_columns <- colnames(dataset_cleaned)[
  sapply(dataset_cleaned,class)%in%c("numeric","integer")]
setDT(dataset_cleaned)

x <- as.matrix(
  dataset_cleaned[,.SD,.SDcols=numeric_columns]
)

n <- nrow(x)

n0 <- initial_window
q <- num_pca
# dim(x) # (100,50)
## Incremental PCA (IPCA, centered)
pca_obj <- prcomp(x[1:n0,],rank. = q) # initialization
# pca <- prcomp(x) # initialization

xbar <- pca_obj$center
pca <- list(values=pca_obj$sdev[1:q]^2, vectors=pca_obj$rotation[,1:q])

i <- n0+1
# x[i,]

# pca[1:(n0+i)]


projections_old <- scale(x[1:n0,], pca_obj$center, pca_obj$scale) %*% pca_obj$rotation 
projections_new <- scale(x[n0+1:2,], pca_obj$center, pca_obj$scale) %*% pca_obj$rotation 
# rank(2,1:10)
# rank(c(projections_new[,1],projections_old[,1]))->ranks
# ranks[c(1,2)]/length(unique(ranks))
# plot(density(projections_old[,1]))

# fill the matrix of ranks
ranks <- matrix(data = 0,nrow=n,ncol=q)
pac_components <- matrix(data = 0,nrow=n,ncol=q*ncol(x))
# browser()
combine <- function(..., prefix = "", sep = "_") {
  paste0(prefix, levels(interaction(..., sep = sep)))
}
colnames(pac_components) <-  combine("pca",1:q,colnames(x))


i <- n0+1
# browser()
for (i in (n0+1):n){
  projections_old <- scale(x[1:(i-1),], pca_obj$center, 
                         pca_obj$scale) %*% pca_obj$rotation 
  projections_new <- scale(matrix(x[i,],nrow=1), pca_obj$center, 
                         pca_obj$scale) %*% pca_obj$rotation 
  ranks_new_normalized <- apply(rbind(projections_new,projections_old),2,function(x){
    rnks <- rank(x)
    rnks/length(unique(rnks))
  })[1,]
  # ranks_new_normalized
  ranks[i,] <- ranks_new_normalized
  pac_components[i,] <- c(pca$vectors)
  
  
  xbar <- updateMean(xbar, x[i,], i-1)
  pca <- incRpca(pca$values, pca$vectors, x[i,], i-1, q = q,
                 center = xbar,f=forget_factor)
}

ranks %>% as.data.table()->ranks_dt
print(pac_components)

list(ranks=ranks,pac_components=pac_components)
# likelyhood <- 2*apply(ranks[ranks[,1]!=0,],2,function(x)abs(x-0.5))[,1:3] %>% 
#   apply(1,prod)

# 0.4^3
  
}

pacman::p_load("dataMaid")
driving_pattern <- "gentle_drive_10min"
analyse_driving_pattern <- function(
  driving_pattern_string= "gentle_drive_10min",
  num_pca=3,
  clean_dm=F) {
  
result_list <- list()
dataset <- read_csv(
  str_c("data/",driving_pattern_string,".csv",collapse = "")
  ,
col_types = cols(BAROMETRIC_PRESSURE = col_double(),
DISTANCE_W_MIL = col_double(), INTAKE_PRESSURE = col_double(),
RUN_TIME = col_double(), SPEED = col_double()))


setDT(dataset)
# library(magrittr)
dataset %>% unique(by="time")->dataset
result_list[['raw_dataset']] <- dataset



nodiff_datas <- dataset[,.(ACCELERATOR_POS_D,ACCELERATOR_POS_E,
                           ABSOLUTE_LOAD,INTAKE_PRESSURE,
                           MAF,RELATIVE_THROTTLE_POS
                           )]

abs_diff_datas <- dataset[,.(ABSOLUTE_LOAD,RPM,INTAKE_PRESSURE,
                         INTAKE_TEMP,RELATIVE_THROTTLE_POS,
                         SPEED,THROTTLE_POS
                         )]

noabs_diff_datas <- copy(abs_diff_datas)
abs_diff_datas <- lapply(abs_diff_datas,function(x)c(0,abs(diff(x)))) %>% 
  as.data.table()

noabs_diff_datas <- lapply(noabs_diff_datas,function(x)c(0,(diff(x)))) %>% 
  as.data.table()

colnames(abs_diff_datas) <- str_c(colnames(abs_diff_datas),"_abs_gradient")
colnames(noabs_diff_datas) <- str_c(colnames(abs_diff_datas),"_gradient")
dset <- cbind(nodiff_datas,abs_diff_datas,noabs_diff_datas)

fwrite(dset,file=str_c("prepared_dataset_",driving_pattern,"csv"))
if(clean_dm) dataMaid::clean(dset,output="html",
      file=str_c("summary",driving_pattern,".html",collapse = ""))

ranks_dt_gentle <- get_list_ranks(
  dset,
  initial_window = 30,
  num_pca = num_pca,
  forget_factor = 30
)
pac_components <- ranks_dt_gentle$pac_components
ranks_dt_gentle <- ranks_dt_gentle$ranks


likelyhood <- 2*apply(ranks_dt_gentle[ranks_dt_gentle[,1]!=0,],2,function(x)abs(x-0.5))[,1:num_pca] %>%
  apply(1,prod)

ranks_dt_gentle <- ranks_dt_gentle%>% as.data.table()
colnames(ranks_dt_gentle) <- sapply(1:ncol(ranks_dt_gentle),
          function(i)str_c("PCA_",i))
ranks_dt_gentle[,lp:=0]
ranks_dt_gentle[-length(likelyhood):(-1) ,lp:=likelyhood]
ranks_dt_gentle[,lp_rolling:=zoo::rollmean(lp,4,fill=0)]

ranks_dt_gentle
# aggregation part --------------------------------------------------------
# ranks_dt_gentle[,possible_outlier:=0]
ranks_dt_gentle[,possible_outlier:=lp_rolling>0.38^num_pca]
result_list[['estimated_ranks_dt']] <- ranks_dt_gentle
original_dataset_with_outliers <- cbind(dataset,
                                        ranks_dt_gentle$possible_outlier)

result_list[['original_dataset_outliers']] <- original_dataset_with_outliers
# likelyhood <- 2*apply(ranks[ranks[,1]!=0,],2,function(x)abs(x-0.5))[,1:3] %>% 
#   apply(1,prod)

# ranks_dt_gentle
result_list[["pac_components"]] <- pac_components
result_list=result_list
}


kkk <- analyse_driving_pattern("gentle_drive_10min")
kkk

driving_pattern_files <- c("aggresive_drive_10min",
                           "gentle_drive_10min",
                           "highway_drive_10min",
                           "parking_2min",
                           "stand_still_1min"
                           )

driving_patterns_datastructs <- list()
for(driving_pattern in driving_pattern_files){
  print(str_c("processing",driving_pattern))
  driving_patterns_datastructs[[driving_pattern]] <- 
    analyse_driving_pattern(driving_pattern,num_pca = 2)
}


# export new dataset ------------------------------------------------------

driving_patterns_datastructs$gentle_drive_10min$original_dataset_outliers$V2 %>% 
driving_patterns_datastructs$aggresive_drive_10min$original_dataset_outliers$V2 %>% table
driving_patterns_datastructs$highway_drive_10min$original_dataset_outliers$V2 %>% table
driving_patterns_datastructs$aggresive_drive_10min$original_dataset_outliers$V2 %>% table
driving_patterns_datastructs$aggresive_drive_10min$original_dataset_outliers$V2 %>% table

for(i in names(driving_patterns_datastructs)){
  print(i)
  driving_patterns_datastructs[[i]]$original_dataset_outliers$V2 %>% table %>% print
  driving_patterns_datastructs[[i]]$original_dataset_outliers %>% fwrite(
    str_c("results_with_outlier_",i,".csv",collapse = T))
}


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


# library(h2o)
# h2o.init()
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
# Full data set
data_variables <- as.matrix(dat[,.SD,.SDcols=colnames(dat) %>% head(-1)])
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)

data_label_factor <- dat[,label] %>% as.factor() 
data_label <- data_label_factor%>% as.integer()
labels_dt <- data.table(data_label,data_label_factor) %>% unique


# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = 5,
                   verbose = FALSE,
                   prediction = TRUE)


bst <- xgboost(data = train_matrix, params = xgb_params,nrounds = 50)

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
