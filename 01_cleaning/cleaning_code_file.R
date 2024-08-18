#Environmentの整理
rm(list = ls())

#使用するパッケージのインストール&ロード
pacman::p_load(tidyverse, readxl)


#----(a)Semester Dataの整形----

#生データの読み込み
semester_data_1 <- read_csv("01_cleaning/semester_raw_data/semester_data_1.csv")
semester_data_2 <- read_csv("01_cleaning/semester_raw_data/semester_data_2.csv")

#データを縦に結合
semester_combined_data <- rbind(semester_data_1, semester_data_2)

#列名の指定
colnames(semester_combined_data) <- semester_combined_data[1, ]
semester_combined_data <- semester_combined_data[-1, ]

#"Y"列の削除
semester_combined_data <- semester_combined_data[ ,-6]

#semester制導入年列の作成
semester_grouped_data <- group_by(semester_combined_data, unitid)
semester_grouped_data <- mutate(semester_grouped_data, yearofsem = ifelse(semester == 1 & lag(quarter) == 1, year, NA))
semester_grouped_data <- fill(semester_grouped_data, yearofsem, .direction = "updown")

#semester制導入後ダミーの作成
semester_grouped_data <- mutate(semester_grouped_data, after = ifelse(is.na(yearofsem), NA, ifelse(yearofsem <= year, 1, 0)))

#クリーニング後データの出力
semester_clean_data <- semester_grouped_data
write_csv(semester_clean_data, file = "01_cleaning/cleaning_data/semester_clean_data.csv")


#----(b)Gradrate Dataの整形----

#生データの読み込み
years <- c(1991:1993, 1995:2010) #読み込む年の指定
file_paths <- paste0("01_cleaning/gradrate_raw_data/", years, ".xlsx") #ファイルパスの作成
gradrate_raw_data_list <- map(file_paths, read_excel) #データの読み込み
gradrate_combined_data <- bind_rows(gradrate_raw_data_list) #データの結合

#女子学生の4年卒業率のスケール変化
gradrate_combined_data$womengradrate4yr <- gradrate_combined_data$women_gradrate_4yr*0.01

#男子学生の4年卒業率算出
gradrate_combined_data$m_4yrgrads <- as.numeric(gradrate_combined_data$m_4yrgrads)
gradrate_combined_data$mengradrate4yr <- gradrate_combined_data$m_4yrgrads / gradrate_combined_data$m_cohortsize

#男女合計の4年卒業率算出
gradrate_combined_data$totcohortsize <- as.numeric(gradrate_combined_data$totcohortsize)
gradrate_combined_data$gradrate4yr <- gradrate_combined_data$tot4yrgrads / gradrate_combined_data$totcohortsize 

#有効数字を3桁に調整
gradrate_combined_data$mengradrate4yr <- round(gradrate_combined_data$mengradrate4yr, 3)
gradrate_combined_data$gradrate4yr <- round(gradrate_combined_data$gradrate4yr, 3)

#クリーニング後データの出力
gradrate_clean_data <- data.frame(gradrate_combined_data)
write_csv(gradrate_clean_data, file = "01_cleaning/cleaning_data/gradrate_clean_data.csv")


#----(c)Covariates Dataの整形----

#生データの読み込み
covariates_raw_data <- read_excel("01_cleaning/covariates_raw_data/covariates.xlsx")

#列名の変更
names(covariates_raw_data)[names(covariates_raw_data) == "university_id"] <- "unitid"

#aaaaの削除
covariates_raw_data$unitid <- gsub("aaaa", "", covariates_raw_data$unitid)

#wide型への拡張
covariates_wide_data <- pivot_wider(covariates_raw_data, names_from = category, values_from = value)

#使用するデータ年を揃える(1991:1993 & 1995:2010)
covariates_wide_data <- filter(covariates_wide_data, !(year %in% c(1987:1990, 1994, 2011:2016)))

#unitidを揃える(outcomeデータに合わせて)
covariates_wide_data$unitid <- as.double(covariates_wide_data$unitid)
covariates_clean_data <- semi_join(covariates_wide_data, gradrate_clean_data, by = "unitid")

#クリーニング後データの出力
write_csv(covariates_clean_data, file = "01_cleaning/cleaning_data/covariates_clean_data.csv")



