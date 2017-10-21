source("load_data_dataset.R")
numeric_columns <- colnames(dataset)[
  sapply(dataset,class)%in%c("numeric","integer")]

x <- as.matrix(
  dataset[,.SD,.SDcols=numeric_columns]
)


library(keras)
library(scales)
K <- keras::backend()

# Parameters --------------------------------------------------------------

batch_size <- nrow(dataset)
original_dim <- ncol(x)
latent_dim <- 2
intermediate_dim <- 3
epochs <- 2500L
epsilon_std <- 0.4

# Model definition --------------------------------------------------------

# inp_data
x <- layer_input(shape = c(original_dim))
h <- layer_dense(x, intermediate_dim, activation = "relu")
z_mean <- layer_dense(h, latent_dim)

# we instantiate these layers separately so as to reuse them later
decoder_h <- layer_dense(units = intermediate_dim, activation = "relu")
decoder_mean <- layer_dense(units = original_dim, activation = "relu")

h_decoded <- decoder_h(z_mean)
x_decoded_mean <- decoder_mean(h_decoded)

# end-to-end autoencoder
vae <- keras_model(x, x_decoded_mean)

# encoder, from inputs to latent space
encoder <- keras_model(x, z_mean)

# generator, from latent space to reconstructed inputs
decoder_input <- layer_input(shape = latent_dim)
h_decoded_2 <- decoder_h(decoder_input)
x_decoded_mean_2 <- decoder_mean(h_decoded_2)
generator <- keras_model(decoder_input, x_decoded_mean_2)


vae_loss <- function(x, x_decoded_mean){
  xent_loss <- (original_dim/1.0)*loss_binary_crossentropy(x, x_decoded_mean)
  xent_loss <- (original_dim/1.0)*loss_mean_absolute_error(x, x_decoded_mean)
  kl_loss <- -0.5*K$mean(1 + z_log_var - K$square(z_mean) - K$exp(z_log_var), axis = -1L)
  xent_loss + kl_loss
}

# keras::optimi
vae %>% compile(optimizer = "rmsprop", loss = loss_mean_squared_error)


# Data preparation --------------------------------------------------------


x_train <- as.matrix(
  dataset[,.SD,.SDcols=numeric_columns]
)
sample_x_test <- sample(nrow(x_train),size=floor(1/4*nrow(x_train)))
x_test <- x_train[sample_x_test,]
x_train <- x_train[-sample_x_test,]


# colMeans(x_train/apply(x_train,2,mean))
col_means <- apply(x_train,2,mean)
col_min <- apply(x_train,2,min)
col_max <- apply(x_train,2,max)
x_train <- sapply(1:ncol(x_train),
                  function(i)
                    # squish(x_train[,i])
                    x_train[,i]/col_means[i]
                    
                  )

x_test <- sapply(1:ncol(x_test),
                 # function(i)squish(x_test[,1]))
                 function(i)x_test[,i]/col_means[i])



# Model training ----------------------------------------------------------

vae %>% fit(
  x_train, x_train, 
  shuffle = TRUE, 
  epochs = epochs, 
  batch_size = batch_size, 
  validation_data = list(x_test, x_test)
)

# x_test_predicted <-predict(, x_test, batch_size = batch_size) 
x_test_predicted <-predict(vae, x_test, batch_size = batch_size) 
# x_test_predicted <-predict(generator, x_test, batch_size = batch_size) 


# Visualizations ----------------------------------------------------------

# library(ggplot2)
# library(dplyr)
# x_test_encoded <- predict(encoder, x_test, batch_size = batch_size)
# 
# x_test_encoded %>%
#   as_data_frame() %>% 
#   mutate(class = as.factor(mnist$test$y)) %>%
#   ggplot(aes(x = V1, y = V2, colour = class)) + geom_point()
# 
# # display a 2D manifold of the digits
# n <- 15  # figure with 15x15 digits
# digit_size <- 28
# 
# # we will sample n points within [-4, 4] standard deviations
# grid_x <- seq(-4, 4, length.out = n)
# grid_y <- seq(-4, 4, length.out = n)
# 
# rows <- NULL
# for(i in 1:length(grid_x)){
#   column <- NULL
#   for(j in 1:length(grid_y)){
#     z_sample <- matrix(c(grid_x[i], grid_y[j]), ncol = 2)
#     column <- rbind(column, predict(generator, z_sample) %>% matrix(ncol = 28) )
#   }
#   rows <- cbind(rows, column)
# }
# rows %>% as.raster() %>% plot()