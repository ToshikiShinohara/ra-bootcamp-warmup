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


#----(b)Graduate Dataの整形----

#生データの読み込み
