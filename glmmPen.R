library(dplyr)
library(glmmPen)

# 資料處理，將所有類別型變數轉為 factor
data <- data %>%
  mutate(
    module_presentation = as.factor(module_presentation),
    score = as.factor(score), 
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    age_band = as.factor(age_band)
    # 可以繼續加其他類別變數
  )

model <- glmmPen(
  score ~ region + highest_education + age_band + studied_credits + imd_band + 
    late_ratio + completion_ratio + homepage_mean_click_interval + 
    homepage_std_click_interval + homepage_mean_clicks + homepage_click_variance + homepage_mean_click_interval + 
    input_std_click_interval + input_mean_clicks + input_click_variance + 
    output_mean_click_interval + output_std_click_interval + 
    output_mean_clicks + output_click_variance + early_score + 
    late_score + mid_score  + 
    (1 | module_presentation),  # 隨機截距
  data = data,
  family = "binomial",
  covar = "independent",
  optim_options = optimControl(),
  tuning_options = selectControl(BIC_option = "BICq", pre_screen = TRUE, search = "abbrev"),
  BICq_posterior = "MyModel_Posterior_Draws"
)
summary(model)



# 資料處理：轉成合適格式、類別轉因子
base_data <- base_data %>%
  mutate(
    score_bin = ifelse(score == "group3", 1, 0),
    module_presentation = as.factor(module_presentation),
    # 其他類別變數確保是 factor
    gender = as.factor(gender),
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    age_band = as.factor(age_band),
    imd_band = as.factor(imd_band)
  )

# 自變數 X 取出成矩陣（用 model.matrix 做虛擬變數編碼）+ region + highest_education + age_band + imd_band
X <- model.matrix(~ gender - 1, data = base_data)
y <- base_data$score_bin
# 群組向量
group <- base_data$module_presentation


# 模型擬合
fit = glmmPen(
  formula = y ~ X + (1 | group),
  family = "binomial",
  covar = "independent",
  optim_options = optimControl(),
  tuning_options = selectControl(BIC_option = "BICq", pre_screen = TRUE, search = "abbrev"),
  BICq_posterior = "MyModel_Posterior_Draws"
)

# 檢視結果
summary(fit)
fixef(fit)
ranef(fit)

idx = sample(1:3000, size = 300, replace = FALSE)
# Selected column index values:
(idx = idx[order(idx)])

test_data <- base_data[idx,]

if(file.exists("Test_Basal_Posterior_Draws.bin")) file.remove("Test_Basal_Posterior_Draws.bin")
if(file.exists("Test_Basal_Posterior_Draws.desc")) file.remove("Test_Basal_Posterior_Draws.desc")

optim_options   <- optimControl(
  var_restrictions = "fixef",   # 只懲罰固定效應
  nMC_start   = 50,             # Monte-Carlo 抽樣量下修
  nMC_max     = 100,
  maxitEM     = 40,             # EM 最多 40 回
  conv_EM     = 0.005           # 放寬收斂容忍
)

tuning_options <- lambdaControl(  # 先固定一組 λ
  lambda0 = 0.01,
  lambda1 = 0.01
)

fit_safe = glmmPen(
  formula        = score_bin ~ gender + (1 | module_presentation),
  data           = test_data,          # 或 y, X, group 物件
  family         = "binomial",
  covar          = "independent",
  penalty        = "lasso",
  optim_options  = optim_options,
  tuning_options = selectControl(BIC_option = "BICq", pre_screen = T, 
                                 search = "abbrev"),      # 不存/讀 posterior 檔
)

test_model = glmmPen(
  formula = score_bin ~ gender + (1 | module_presentation),
  data = test_data,
  family = "binomial", covar = "independent", 
  optim_options = optimControl(),
  tuning_options = selectControl(BIC_option = "BICq", pre_screen = T, 
                                 search = "abbrev"),
  BICq_posterior = "Test_Basal_Posterior_Draws"
)


set.seed(123)
n <- 100
test_data <- data.frame(
  score_bin = rbinom(n, 1, 0.5),
  gender = factor(sample(c("M", "F"), n, replace = TRUE)),
  module_presentation = factor(sample(1:5, n, replace = TRUE))
)


fit <- glmmPen(
  formula = score_bin ~ gender + (1 | module_presentation),
  data = base_data,
  family = "binomial",
  penalty = "lasso"
)

class(fit)


# basal data from glmmPen package
data("basal")

# Extract response
y = basal$y
# Select a sampling of 10 TSP covariates from the total 50 covariates
set.seed(1618)
idx = sample(1:50, size = 10, replace = FALSE)
# Selected column index values:
(idx = idx[order(idx)])
X = basal$X[,idx]
# Selected predictors:
colnames(X)
group = basal$group
# Levels of the grouping variable:
levels(group)

start_basal = proc.time()

set.seed(1618)
fitB = glmmPen(formula = y ~ X + (X | group), 
               family = "binomial", covar = "independent", 
               optim_options = optimControl(),
               tuning_options = selectControl(BIC_option = "BICq", pre_screen = T, 
                                              search = "abbrev"),
               BICq_posterior = "Basal_Posterior_Draws")

fitT = glmmPen(formula = y ~ X + (X | group), 
               family         = "binomial",
               covar          = "independent",
               optim_options  = optim_options,
               tuning_options = selectControl(BIC_option = "BICq", pre_screen = T, 
                                              search = "abbrev"),)

end_basal = proc.time()
# Time needed to complete the algorithm
end_basal - start_basal

# Illustration of use of methods
summary(fitB)
print(fitB)
