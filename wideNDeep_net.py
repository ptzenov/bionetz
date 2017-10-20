import os
import pandas as pd
import numpy as np
import tensorflow as tf

# from keras import initializers
import keras as ks
from keras.layers import Dense, Lambda,Input,LSTM
from keras.models import Model
import keras.backend as K


tf.logging.set_verbosity(tf.logging.ERROR)
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

df_train = pd.read_csv("../data/BTC_data_2017-01-01_2017-08-01.csv")
df_train['Date'] = df_train['Date'].apply(pd.to_datetime)

df_test = pd.read_csv("../data/BTC_data_2017-08-01_2017-09-01.csv")
df_test['Date'] = df_test['Date'].apply(pd.to_datetime)

print (df_train.columns)
print (df_test.columns)


assert set(df_train.columns) == set(df_test.columns), "train and test data have differnet columns "
# this is our target category to predict
label = "BTC_USD"
date = "Date"


CATEGORICAL_COLUMNS = [ ]
CONTINUOUS_COLUMNS = list(set(df_train.columns) - set([label,date]))


wide_colnames=CATEGORICAL_COLUMNS
deep_colnames=CONTINUOUS_COLUMNS


import predictors.utils as putil
# this is a Keras Lambda layer implementing the ordinal 2 binary conversion
binary_encoder = Lambda(putil.ordinal_2_binary, output_shape=[putil.MAX_BITS])

# deep layer stuff
wd, dd = 0.02, 0.1
dnn_input = Input(shape = [len(deep_colnames)] )

deep_layers = [dnn_input]
hu_configs = [40,40,20,1] # hidden units size
for d in hu_configs:
    layer = putil.ConcreteDropout(Dense(units=d, activation='relu'),
                              weight_regularizer=wd, dropout_regularizer=dd)(deep_layers[-1])
    deep_layers.append(layer)
deep_output  = deep_layers[-1]

# convert all categorical variables into binary encodings with 8 bits each
# if len(wide_colnames) > 0:
#     wide_input = [Input(shape = [1]) for _ in wide_colnames] # <- convert numeric to binary
#     wide_inputs_bin = [binary_encoder(wide_in) for wide_in in wide_input]
#     wide_inputs_concat = ks.layers.concatenate(wide_inputs_bin)
#     wide_output = putil.ConcreteDropout(Dense(1, activation = 'relu'),
#                               weight_regularizer=wd, dropout_regularizer=dd)(wide_inputs_concat)


# recurrent layer stuff
num_unrollings = 10
lstm_input = Input(shape = [num_unrollings,1])
lstm_1 = LSTM(units=64, return_sequences = True)(lstm_input)
lstm_2 = LSTM(units=32, return_sequences = True)(lstm_1)
lstm_3 = LSTM(units=16)(lstm_2)
lstm_out = Dense(units=1,activation='relu')(lstm_3)

# output = ks.layers.concatenate([deep_output,wide_output])
# output = ks.layers.concatenate([deep_output,wide_output,lstm_out])
output = ks.layers.concatenate([deep_output,lstm_out])

def format_seq_data(df,  # pandas data frame
                    x_cols,  # input cols to take data from
                    order_col,  # column to order by
                    num_unrollings=5  # size of recurrent sequence
                    ):
    df_ = df.copy().sort_values(order_col)
    df_ = df_[x_cols].shift(1).fillna(0).reset_index(drop=True)
    res_ = np.zeros([len(df_), num_unrollings, len(x_cols)])

    for seq in range(len(df_) - 1):
        diff = (len(df_) - seq)
        if diff >= num_unrollings:
            num_els = num_unrollings
        else:
            num_els = diff
        res_[seq, 0:num_els, :] = df_.iloc[seq: (seq + num_unrollings), :].as_matrix()
    return res_
lstm_input_data = format_seq_data(df_train, [label], [date], num_unrollings)

### now concat and set prediction
prediction  = Dense(units=1,activation = 'relu')(output)

D = 1
mean = putil.ConcreteDropout(Dense(D), weight_regularizer=wd, dropout_regularizer=dd)(prediction)
log_var = putil.ConcreteDropout(Dense(D), weight_regularizer=wd, dropout_regularizer=dd)(prediction)
out = ks.layers.concatenate([mean, log_var])

def heteroscedastic_loss(true, pred):
    mean = pred[:, :D]
    log_var = pred[:, D:]
    precision = K.exp(-log_var)
    return K.sum(precision * (true - mean) ** 2. + log_var, -1)

def relative_error(true, pred):
    return K.sum(K.abs(true -  pred[:, :D]))/K.sum(true)

# kerModel = Model(wide_input+[dnn_input,lstm_input],out)
# kerModel = Model([dnn_input,lstm_input],out)
kerModel = Model([dnn_input,lstm_input], prediction)

kerModel.compile(optimizer = 'adam',loss = "mean_squared_error")

# now train the keras model
# wide_col_categories_dict = []
# wide_colnames_int = []
# for col in wide_colnames:
#     all_cats = list(set(list(df_train[col]) + list(df_test[col])))
#     all_cats_dict = {cat: idx+1 for idx,cat in enumerate(all_cats)}
#     wide_col_categories_dict.append(all_cats_dict)
#     newname = col+'_int'
#     df_train[newname] = df_train[col].map(all_cats_dict)
#     df_test[newname] = df_test[col].map(all_cats_dict)
#     wide_colnames_int.append(newname)


deep_input_data = df_train[[c for c in deep_colnames]].as_matrix()
# wide_input_data = [ df_train[c].as_matrix() for c in wide_colnames_int ]

labels = df_train[label].as_matrix()
# kerModel.fit([wide_input_data, deep_input_data, lstm_input_data], labels, epochs=150, batch_size= 50,verbose =2)
kerModel.fit([deep_input_data, lstm_input_data], labels, epochs=150, batch_size= 50,verbose =2)
kerModel.fit([deep_input_data, lstm_input_data], labels, epochs= 2000, batch_size= 50,verbose =2)
kerModel.fit([deep_input_data, lstm_input_data], labels, epochs= 1000, batch_size= 10,verbose =2)




