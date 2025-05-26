library(dplyr)
library(lme4)

data <- data %>%
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
  score_bin ~ gender + region + highest_education + age_band + 
    late_ratio + 
    homepage_mean_clicks + homepage_mean_click_interval + #homepage_std_click_interval + 
    input_std_click_interval + input_mean_clicks +  
    output_mean_click_interval + output_mean_clicks + output_click_variance + #output_std_click_interval + 
    early_score + late_score + mid_score +
    (1 | module_presentation),  # 隨機截距
  data = data,
  family = binomial(link = "logit")
)
summary(model)

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
