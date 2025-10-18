library(dplyr)
library(lme4)
library(sjPlot)

data_normalized <- data_normalized %>%
  mutate(
    score_bin = ifelse(score == "group3", 1, 0),
    module_presentation = as.factor(module_presentation),
    gender = as.factor(gender),
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    age_band = as.factor(age_band),
    imd_band = as.factor(imd_band)
  )

model <- glmer(
  score_bin ~ gender + region + highest_education + age_band + imd_band + 
    homepage_mean_clicks + homepage_mean_click_interval + homepage_click_variance + homepage_std_click_interval + 
    input_mean_click_interval + input_mean_clicks + input_click_variance + input_std_click_interval + 
    output_mean_click_interval + output_mean_clicks + output_click_variance + output_std_click_interval + 
    early_score + mid_score + late_score + late_ratio + 
    (1 | module_presentation),  # 隨機截距
  data = data_normalized,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model)

base_model <- glmer(
  score_bin ~ region + highest_education + age_band + imd_band + gender + 
    (1 | module_presentation),  # 隨機截距
  data = data_normalized,
  family = binomial(link = "logit")
)
summary(base_model)

tab_model(base_model, model, )
library(performance)

r2_res <- r2(model)   # 計算 R²
print(r2_res)

m <- model.matrix(model)
head(m)

cor_matrix <- cor(data, use = "complete.obs")
print(round(cor_matrix, 2))

# 資料處理，將所有類別型變數轉為 factor
base_data <- base_data %>%
  mutate(
    module_presentation = as.factor(module_presentation),
    imd_band = as.factor(imd_band),
    gender = as.factor(gender),
    score = as.factor(score), 
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    age_band = as.factor(age_band)
    # 可以繼續加其他類別變數
  )

base_model <- glmer(
  score ~ region + highest_education + age_band + imd_band + gender + 
    (1 | module_presentation),  # 隨機截距
  data = base_data,
  family = binomial(link = "logit")
)
summary(base_model)
