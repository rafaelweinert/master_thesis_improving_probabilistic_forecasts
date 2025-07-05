#%% load libraries ----

# remove all objects from the workspace
rm(list = ls())

# read the score_data RData file 
library(dplyr)
library(crch)
library(TailCalibration)
library(rlang)
library(ggplot2)

path = r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\postprocessing_wind_gusts)"

# load("postprocessing_wind_gusts/wind_gust_data/score_data.RData")
# load("postprocessing_wind_gusts/wind_gust_data/loc_data.RData")
# load("postprocessing_wind_gusts/wind_gust_data/ens_data/ens_data_step0.RData")
# load("postprocessing_wind_gusts/wind_gust_data/pp_data/bqn_step0.RData")


# load forecast data
lead_10 <- load(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\postprocessing_wind_gusts\wind_gust_data\pp_forecasts\forecast_data_step10.RData)")


#%% pp_forecasts ----

# data structure:

# forecast_data_stepX.RData
# - fc_data -> list of 12 dataframes
# |- ens
#     |- ens_1 -> list of 63727 values
#     |- ens_2
#     |- ...
# |- emos -> matrix of 63727 x 2 values (mean and variance)
# |- emos_sea
# |- emos_bst
# |- mbm
# |- ...

# - obs_vec: list of 63727 observed values
# - pit_rank

idx <- 10

# select one example forecast data point
fc_step1_ens_1 <- as.numeric(fc_data$ens[idx,])

# plot a pdf of a censored logistic distribution with mean and variance 
x <- seq(0, 20, length = 1000)

# Plot histogram and line plot in one

hist(fc_step1_ens_1, probability = TRUE, add = FALSE, main = "Histogram and PDF", xlab = "Values", ylab = "Density", xlim = c(0,20), breaks = 5)

# EMOS
fc_step1_emos_1 <- fc_data$emos[idx,]
y_emos <- dtlogis(x, fc_step1_emos_1[1], fc_step1_emos_1[2], left = 0)
lines(x, y_emos, col = "red")

# EMOS SEA
fc_step1_emos_sea_1 <- fc_data$emos_sea[idx,]
y_emos_sea <- dtlogis(x, fc_step1_emos_sea_1[1], fc_step1_emos_sea_1[2], left = 0)
lines(x, y_emos_sea, col = "blue")

# EMOS BST
fc_step1_emos_bst_1 <- fc_data$emos_bst[idx,]
y_emos_bst <- dtlogis(x, fc_step1_emos_bst_1[1], fc_step1_emos_bst_1[2], left = 0)
lines(x, y_emos_bst, col = "green")

# MBM
fc_step1_mbm_1 <- fc_data$mbm[idx,]
hist(fc_step1_mbm_1, probability = TRUE, add = TRUE, col = "purple", xlim = c(0,20), breaks = 5)

# MBM SEA
fc_step1_mbm_sea_1 <- fc_data$mbm_sea[idx,]
hist(fc_step1_mbm_sea_1, probability = TRUE, add = TRUE, col = "orange", xlim = c(0,20), breaks = 5)

# horizontal line for observed value
abline(v = obs_vec[idx], col = "red")




#%% observed values ----

# histogram of all observed values
hist(obs_vec, probability = TRUE, add = FALSE, main = "Histogram of observed values", xlab = "Values", ylab = "Density", breaks = 100)

# Calculate the quantiles
q <- quantile(obs_vec, probs = c(0, 0.9, 0.95, 0.99, 0.999))

# Add horizontal lines for each quantile with different colors and line types
colors <- c("black", "blue", "green", "orange", "red")
ltys <- c(1, 2, 3, 4, 5)

for(i in 1:length(q)) {
    abline(v = q[i], col = colors[i], lty = ltys[i])
}

# Add a legend
legend("topright", 
             legend = c("0% (min)", "90%", "95%", "99%", "99.9%"), 
             col = colors, 
             lty = ltys, 
             title = "Quantiles")


#%% non-postprocessed ensemble forecast ----

# combined ratio
com <- tc_prob(obs_vec, as.matrix(fc_data$ens), t = c(-Inf, 10, 13, 18), lower = 0)
plot_ptc(com, names = c("  S", " 10", "13", "18"), ylims = c(0, 1.1), title = "IFS")

# severity ratio
sev <- tc_prob(obs_vec, as.matrix(fc_data$ens), t = c(-Inf, 10, 13, 18), ratio = "sev", lower = 0)
plot_ptc(sev, ratio = "sev", names = c("  S", " 10", "13", "18"), ylims = c(0, 1.02), title = "")

# occurrence
occ <- tc_prob(obs_vec, as.matrix(fc_data$ens), t = c(-Inf, 10, 13, 18), ratio = "occ", lower = 0)
plot_ptc(occ, ratio = "occ", names = c("  S", " 10", "13", "18"), ylims = c(-0.2, 6), xlab = "t (m/s)", title = "")


##%% non postprocessed ensemble forecasts train + test ----

load(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\postprocessing_wind_gusts\wind_gust_data\ens_data\ens_data_step1.RData)")
head(df_train)

# Remove rows with missing values in obs or columns starting with "ens_"
df_train <- df_train %>% filter(complete.cases(select(., obs, starts_with("ens_"))))
obs_train <- df_train$obs
ens_train <- df_train %>% select(starts_with("ens_"))

# combined ratio
com <- tc_prob(obs_train, as.matrix(ens_train), t = c(-Inf, 10, 13, 18), lower = 0)
plot_ptc(com, names = c("  S", " 10", "13", "18"), ylims = c(0, 1.2), title = "IFS")

# severity ratio
sev <- tc_prob(obs_train, as.matrix(ens_train), t = c(-Inf, 10, 13, 18), ratio = "sev", lower = 0)
plot_ptc(sev, ratio = "sev", names = c("  S", " 10", "13", "18"), ylims = c(0, 1.02), title = "")

# occurrence
occ <- tc_prob(obs_train, as.matrix(ens_train), t = c(0:25), ratio = "occ", lower = 0)
plot_ptc(occ, ratio = "occ", names = c("  S", " 10", "13", "18"), ylims = c(-0.2, 6), xlab = "t (m/s)", title = "")







########################### ens_data ########################################

load(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\postprocessing_wind_gusts\wind_gust_data\ens_data\ens_data_step1.RData)")

# data structure:

# ens_data_stepX.RData
# - df_test -> 64050 observations, 147 variables (ensemble members, observation, metadata)
# - df_train -> 322700 observations, 147 variables (ensemble members, observation, metadata)
# - add_vars -> list of 60 strings (metadata column names)

# df_test contains ensemble forecasts (are the same as in pp_forecasts) -> select columns
df_test_ens <- df_test %>% select(starts_with("ens_"))


# some of the rows in pp_forecasts are missing -> select them
fc_step1_ens_rownames <- as.integer(rownames(fc_step1_ens))
df_test_ens_rownames <- as.integer(rownames(df_test_ens))

missing_indexes <- setdiff(df_test_ens_rownames,fc_step1_ens_rownames )

# compare the two dataframes without the missing indexes
df_test_ens_filtered <- df_test_ens %>% filter(!row_number() %in% missing_indexes) %>% select(!c('ens_mean', 'ens_sd'))
any(fc_step1_ens != df_test_ens_filtered) # -> all elements are equal

df_test_ens[1,]


########################### pp_data ########################################

# take for example EMOS BST
load(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\postprocessing_wind_gusts\wind_gust_data\pp_data\emos_bst_step1.RData)")

# data structure:

# emos_bst_stepX.RData
# - est_pp -> list of 175 locations
# |- hr0_step1_loc10004
# |- hr0_step1_loc10007
# |- ...
# - pred_pp -> list of 175
# |- hr0_step1_loc10004
#       |- f
#       |- scores_pp
#       |- scores_ens
#       |- n_test
#       |- runtime
# |- hr0_step1_loc10007
# |- ...

names(pred_pp$hr0_step1_loc10004)






########################### test data ########################################
load(paste(path, r"(\df_test.RData)", sep = ""))


# select those where location = 1
df_test <- df_test %>% filter(location == 1)

View(df_test)




emos_step0 <- data.frame(fc_data[2])


fc_data4_9 <- data.frame(fc_data[9])

# to csv
write.csv(fc_data1, "fc_data1.csv")	

pit_rank1 <- pit_rank[1]

str(pit_rank1$epc[1])

plot(array(pit_rank1$epc[1]))

head(pit_rank1$epc$hr0_step1_loc10004)

data.frame(pit_rank1$epc$hr0_step1_loc10004)


View(fc_data1$ens)

plot(x = c(1:125), y = fc_data4_9[1,])
