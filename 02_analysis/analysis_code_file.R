#Environmentの整理
rm(list = ls())

#必要なパッケージの読み込み
pacman::p_load(tidyverse, modelsummary)


#----(a)記述統計----
#----1.NAのカウント----

#Master Dataの読み込み
master_data <- read_csv("01_cleaning/cleaning_data/master_clean_data.csv")

#各列のNAの数をカウント
na_counts <- sapply(master_data, function(x) sum(is.na(x)))
print(na_counts)


#----2.記述統計の作成----

#大学のグループ分け
df_neverswitchers <- filter(master_data, is.na(yearofsem)) #学期制無変更群
df_switchers <- filter(master_data, !is.na(yearofsem)) #学期制変更群
df_grouped_institutions <- list("All" =master_data, "Never switchers" = df_neverswitchers, "Switchers" = df_switchers) #リスト化

#2学期制の割合計算
semester_calc <- function(df){summarise(df, mean_semester = round(mean(df$semester), 2), sd_semester = round(sd(df$semester), 2))} #計算用の関数を定義(有効数字2桁)
semester_frac <- map(df_grouped_institutions, semester_calc) #2学期制の割合を計算
semester_calender <- bind_rows(semester_frac, .id = "Group") #データフレーム化

#4年卒業率の計算
grad4yr_calc <- function(df){summarise(df, mean_grad4yr = round(mean(df$gradrate4yr), 2), sd_grad4yr = round(sd(df$gradrate4yr), 2))} #計算用の関数を定義(有効数字2桁)
grad4yr_frac <- map(df_grouped_institutions, grad4yr_calc)
fouryrgradrate <- bind_rows(grad4yr_frac, .id = "Group") #データフレーム化

#4年卒業率の計算（女性）
womengrad4yr_calc <- function(df){summarise(df, mean_womengrad4yr = round(mean(df$womengradrate4yr, na.rm = TRUE), 2), sd_womengrad4yr = round(sd(df$womengradrate4yr, na.rm = TRUE), 2))} #計算用の関数を定義(有効数字2桁)
womengrad4yr_frac <- map(df_grouped_institutions, womengrad4yr_calc)
women_fouryrgradrate <- bind_rows(womengrad4yr_frac, .id = "Group") #データフレーム化

#4年卒業率の計算（男性）
mengrad4yr_calc <- function(df){summarise(df, mean_mengrad4yr = round(mean(df$mengradrate4yr, na.rm = TRUE), 2), sd_mengrad4yr = round(sd(df$mengradrate4yr, na.rm = TRUE), 2))} #計算用の関数を定義(有効数字2桁)
mengrad4yr_frac <- map(df_grouped_institutions, mengrad4yr_calc)
men_fouryrgradrate <- bind_rows(mengrad4yr_frac, .id = "Group") #データフレーム化

#データの結合
statistics <- left_join(semester_calender, fouryrgradrate, by = "Group")
statistics <- left_join(statistics, women_fouryrgradrate, by = "Group")
statistics <- left_join(statistics, men_fouryrgradrate, by = "Group")
statistics <- as.data.frame(t(statistics))

#要約統計の表示
print(statistics)


#----3.4年卒業率の推移----

#各年の4年卒業率の平均を計算
mean_gradrate4yr <- aggregate(master_data$gradrate4yr, by = list(master_data$year), FUN = mean)

#列名の変更
colnames(mean_gradrate4yr) <- c("year", "mean_gradrate4yr")

#図示
ggplot(mean_gradrate4yr, ,mapping = aes(x = year, y = mean_gradrate4yr)) + geom_line() + labs(title = "Four-year graduation rate", x = "", y = "4-year graduation rate") +theme_minimal()


#----4.semester導入率の推移----

# 各年のsemester導入率を計算
semester_rate <- aggregate(master_data$semester, by = list(master_data$year), FUN = mean)

#列名の変更
colnames(semester_rate) <- c("year", "semester_rate")

#図示
ggplot(semester_rate, mapping = aes(x = year, y = semester_rate)) + geom_line() + labs(title = "Share on semesters", x = "", y = "Fraction of schools on semesters") + theme_minimal()


#----5.散布図の作成----

#女子学生比率の算出
master_data$per_women_cohort <- master_data$w_cohortsize / master_data$totcohortsize

#白人学生割合の算出
master_data$per_white_cohort <- master_data$white_cohortsize / master_data$totcohortsize

#散布図の作成(女子学生比率)
ggplot(master_data, aes(x = per_women_cohort, y = gradrate4yr)) + geom_point() + labs(x = "女子学生比率", y = "4年卒業率")

#散布図の作成(白人学生割合)
ggplot(master_data, aes(x = per_white_cohort, y = gradrate4yr)) + geom_point() + labs(x = "白人学生割合", y = "4年卒業率")

#散布図の作成(学費)
ggplot(master_data, aes(x = instatetuition, y = gradrate4yr)) + geom_point() + labs(x = "学費", y = "4年卒業率")


#----(b)回帰分析----

#回帰分析
result <- lm(gradrate4yr ~ semester, data =master_data)

#表に出力
modelsummary(result)