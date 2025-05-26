# 讀取資料
# dat <- read_csv("data_0428_3c.csv")

# 載入必要套件
library(mgcv)
library(dplyr)

# 資料處理，將所有類別型變數轉為 factor
dat <- dat %>%
  mutate(
    module_presentation = as.factor(module_presentation),
    score = as.factor(score),  # score 是多分類
    region = as.factor(region),
    highest_education = as.factor(highest_education),
    age_band = as.factor(age_band)
    # 可以繼續加其他類別變數
  )


# 將 score 轉換為數字，因為 mgcv 需要數字的類別（從 0 開始）
dat$score <- as.numeric(dat$score) - 1  # 假設 score 原本是字符類別，從 0 開始

# 建立混合效應多分類模型
model <- gam(
  list(
    score ~ region + highest_education + age_band + studied_credits + late_ratio + 
      completion_ratio + input_mean_click_interval + input_std_click_interval + 
      input_mean_clicks + input_click_variance + output_mean_click_interval + 
      output_std_click_interval + output_mean_clicks + output_click_variance + 
      early_score + late_score + mid_score + 
      s(module_presentation, bs = "re"),
    
    # 重複公式：每個分類（對應到 score 的各個類別）
    ~ region + highest_education + age_band + studied_credits + late_ratio + 
      completion_ratio + input_mean_click_interval + input_std_click_interval + 
      input_mean_clicks + input_click_variance + output_mean_click_interval + 
      output_std_click_interval + output_mean_clicks + output_click_variance + 
      early_score + late_score + mid_score + 
      s(module_presentation, bs = "re")
  ),
  data = dat,
  family = multinom(K = 2),   # 5 類別，因此 K = 4
)

# 顯示模型摘要
summary(model)

library(car)
# （可選）檢查固定效應之間的多重共線性
# 建一個簡單的線性模型（不帶隨機效應）來計算 VIF
vif_model <- lm(score ~ region + highest_education + age_band + studied_credits + late_ratio + 
                  completion_ratio + input_mean_click_interval + input_std_click_interval + 
                  input_mean_clicks + input_click_variance + output_mean_click_interval + 
                  output_std_click_interval + output_mean_clicks + output_click_variance + 
                  early_score + late_score + mid_score, data = dat)
vif(vif_model)

library(tidyr)
library(ggplot2)
library(dplyr)

# 建立一個新的資料框，僅改變 input_std_click_interval，其餘保持固定（例如取平均）
newdat <- dat %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  slice(rep(1, 100)) %>%
  mutate(input_std_click_interval = seq(
    min(dat$input_std_click_interval, na.rm = TRUE),
    max(dat$input_std_click_interval, na.rm = TRUE),
    length.out = 100
  ))

# 若有類別變數，也要設定為某個 level（例如 region = "London Region"）
newdat$region <- "London Region"
newdat$highest_education <- "HE Qualification"
newdat$age_band <- "35-55"
newdat$module_presentation <- levels(dat$module_presentation)[1]
newdat$score <- NULL  # 確保沒有 response variable（避免干擾）

# 預測：回傳 matrix，轉為 data frame 並命名欄位
pred <- as.data.frame(predict(model, newdata = newdat, type = "response"))

# 確保有正確的欄位名稱（代表每個類別）
colnames(pred) <- paste0("score_", seq_len(ncol(pred)) - 1)

# 結合 input_std_click_interval 和預測結果
pred_df <- cbind(newdat["input_std_click_interval"], pred)
pred_long <- pivot_longer(pred_df, cols = starts_with("score_"), names_to = "class", values_to = "prob")

# 畫圖！
ggplot(pred_long, aes(x = input_std_click_interval, y = prob, color = class)) +
  geom_line(size = 1.2) +
  labs(
    title = "prediction  vs input_std_click_interval",
    x = "input_std_click_interval",
    y = "預測機率",
    color = "score group"
  ) +
  theme_minimal()

