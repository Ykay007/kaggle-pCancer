# load libraries
library('tidyverse') # tidy data manipulations
library('stringr')   # string manipulation
library('gridExtra') # visualisation
library('ggfortify') # visualisation
library('ggraph') # visualisation
library('readr') # visualisation
library('tidytext') # text mining
library('tidyr')     # data transofrmation
library('magrittr')  # data transofrmation
library('SnowballC') # text analysis
library('wordcloud') # test visualisation
library('dummies')   # one-hot encoding

# source functions
source("utils/evalUtils.R")
source("utils/preprocessUtils.R")
source("utils/generalUtils.R")


# read files --------------

# variants & genes
train <- read_csv('data/trainingVariants')
test  <- read_csv('data/testVariants')

# text data
train_txt <- readTxtData(path = "data/trainingText")
test_txt  <- readTxtData(path = "data/testText")

# train/test: data types conversion
train %<>% mutate_at(vars(-ID), funs(factor))
test %<>% mutate_at(vars(-ID), funs(factor))



# preprocess text ------------------------

# join the datasets
# train_combined <- full_join(train, train_txt)


# some NLP -----------------

# train: text preprocessing
t1  <- textPreprocess(df = train_txt, grams = "1") # 1-gram
t1b <- textPreprocess(df = train_txt, grams = "2") # bigrams
t1t <- textPreprocess(df = train_txt, grams = "3") # trigrams

# combine 1-gram and 2-grams
tr_combined <- bind_rows(t1, t1b, t1t)

# test: text preprocessing
t2  <- textPreprocess(df = test_txt, grams = "1")  # 1-gram
t2b <- textPreprocess(df = test_txt, grams = "2")  # bigrams
t2t <- textPreprocess(df = test_txt, grams = "3")  # trigrams

# combine 1-gram and 2-grams
tst_combined <- bind_rows(t2, t2b, t2t)

# add the response for t1
t1_class <- full_join(tr_combined, train %>%  select(Class, ID), by = "ID")

# word cloud
# t1 %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 25))

# 
# # word frequency by class (frequency > 500 words)
# frequency <- t1_class %>%
#   count(Class, word) %>%
#   filter(n > 5e2) %>%
#   group_by(Class) %>%
#   mutate(freq = n / sum(n)) %>%
#   select(-n)
# 
# # top 10 for each class
# frequency %>%
#   group_by(Class) %>%
#   arrange(-freq) %>%
#   filter(row_number() %in% 1:10) -> tmp
# 
# # plot it
# ggplot(tmp, aes(x = word, y = freq)) +
#   geom_point(alpha = 0.1, size = 2.5) +
#   ggrepel::geom_label_repel(aes(label = word)) +
#   facet_wrap(~Class, scales = "free_y") +
#   labs(y = "Frequency", x = "Word") +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         legend.position="none")



# TF-IDF -------------
frequency <- t1_class %>%
  count(Class, word)

# create tf-idf
tf_idf <- frequency %>%
  bind_tf_idf(word, Class, n)

# tf-idf top 20
# tf_idf %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word)))) %>%
#   top_n(20, tf_idf) %>%
#   ggplot(aes(word, tf_idf, fill = Class)) +
#   geom_col(position = "dodge") +
#   labs(x = NULL, y = "tf-idf") +
#   coord_flip()

# word frequency by class
tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Class) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%  
  ggplot(aes(word, tf_idf, fill = Class)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  theme(legend.position = "none") +
  facet_wrap(~ Class, ncol = 3, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format())



# extract top 50 words per class ----------------
word_list <- tf_idf %>% 
  group_by(Class) %>% 
  top_n(50, tf_idf) %>% 
  ungroup %>% 
  distinct(word)



# one-hot encode the words in the word_list from txt data ----
train_wide <- wideTerms(df = tr_combined, wordList = word_list)

# convert ID column and join wide term matrix with labeles, genes and variations
train$ID %<>% as.character()
train_wide <- left_join(train, train_wide, by = "ID")

# filling NAs
train_wide[is.na(train_wide)] <- 0



# association rules ----------
# require(arules)
# 
# feats <- train_wide %>% 
#   select(Gene, Class, Variation) %>% 
#   mutate_all(.funs = as.factor)
# 
# rules <- apriori(feats, parameter = list(supp = 0.01, conf = 0.9, target = "rules"))
# inspect(head(rules, by = "lift"))



# create numerical matrix for training -------

# one-hot encode the genes and variations
genes      <- as_tibble(dummy(train_wide$Gene))
variations <- as_tibble(dummy(train_wide$Variation))

# remove categorical features
train_wide %<>% select(-Gene, -Variation)

# add one-hot encoded features
train_wide <- bind_cols(train_wide, genes, variations)




# benchmark modellling ------------

# no need for ID no more..
train_wide %<>% select(-ID)

# data partition
indices <- caret::createDataPartition(y = train_wide$Class, times = 1, p = 0.75, list = FALSE)
tr   <- train_wide[indices,]
cv   <- train_wide[-indices,]

# strings should be factors
tr %<>% mutate_at("Class", .funs = as.factor)
cv %<>%  mutate_at("Class", .funs = as.factor)

# response
response = "Class"

# features
features = setdiff(names(train_wide), response)

# sda model
require(sda)

# train
sda_model <- sda(Xtrain = as.matrix(tr[features]),  L = tr$Class, diagonal = FALSE)

# predict
predictions <- predict.sda(sda_model, as.matrix(cv[features]))$class

# get evaluation df
performance <- tibble(pred = predictions, truth = cv$Class)

# compute confusion matrix
caret::confusionMatrix(performance$pred, performance$truth)

# glmnet model
glm_model   <- glmnet::glmnet(x = as.matrix(tr[features]),
                              y = tr$Class,
                              family = "multinomial")

# evaluation df
perf <- tibble(pred = as.vector(predict(glm_model, as.matrix(cv[features]), type = "class", s = 0.005)),
               truth = cv$Class)

# compute confusion matrix
caret::confusionMatrix(perf$pred, performance$truth)



# XGboost model ----------------------

# parameters
param <- list("objective"   = "multi:softprob",
              "eval_metric" = "mlogloss")

# Create xgb matrix
trainMat = xgboost::xgb.DMatrix(data = as.matrix(tr[features]),   # training data
                       label = as.numeric(tr$Class) - 1, # above threshold (binary)
                       weight = rep(1, nrow(tr))) # weights


# cv to find number of iterations
set.seed(456)
bst.cv = xgboost::xgb.cv(param = param, # algorithm parameters
                data = trainMat,        # predictors matrix
                nfold = 4,              # cross validation folds
                nthread = 2,            # number of cores to use, default learning rate
                nrounds = 2000,         # number of (max) iterations
                num_class = 9,
                early_stopping_rounds = 30) 

# train on entire data
FullTrainMat = xgboost::xgb.DMatrix(data = as.matrix(train_wide[features]),   # training data
                                label = as.numeric(train_wide$Class) - 1, # above threshold (binary)
                                weight = rep(1, nrow(train_wide))) # weights

# train the model
set.seed(647)
xgb_model = xgboost::xgboost(param = param,          
                    data = FullTrainMat,             
                    nthread = 2,
                    num_class = 9,
                    nrounds = bst.cv$best_iteration) # number of iterations found in the cv

# predict & evaluate
perf_xgb <- tibble(pred = predict(xgb_model, as.matrix(cv[features]), type = "class"),
               truth = as.numeric(cv$Class) - 1)

# evaluate
caret::confusionMatrix(perf_xgb$pred, perf_xgb$truth)



# test data preprocessing -----------------

# one-hot encode the words in the word_list from txt data ----
test_wide <- wideTerms(df = tst_combined, wordList = word_list)

# convert ID column and join wide term matrix with labeles, genes and variations
test$ID %<>% as.character()
test_wide <- left_join(test, test_wide, by = "ID")

# filling NAs
test_wide[is.na(test_wide)] <- 0

# one-hot encode the genes and variations
ts_genes      <- as_tibble(dummy(test_wide$Gene))
ts_variations <- as_tibble(dummy(test_wide$Variation))

# remove categorical features
test_wide %<>% select(-Gene, -Variation)

# add one-hot encoded features
test_wide <- bind_cols(test_wide, ts_genes, ts_variations)

# features list
ts_features <- intersect(names(test_wide), names(train_wide))

# remove colnames that are not present in the training data
test_ready <- test_wide[ts_features]




# predict on test data -----------

# predict
predictions <- predict(xgb_model, as.matrix(test_ready[ts_features]), type = "prob")

# reshape
probs <- t(matrix(predictions, nrow = 9, ncol = length(predictions)/9))

# add ID
probs <- as_tibble(probs)
probs %<>% mutate(ID = test_wide$ID)

# rename and re-arrange columns
names(probs) <- c(paste0("class",1:9), "ID")
probs %<>% select(ID, everything())

# export
write_csv(probs, "xgboostPreds2.csv")
