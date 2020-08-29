#rm(list=ls())

# ライブラリのロード
library(tidyverse)
library(rvest)
library(dplyr)
library(lubridate)
options(digits = 2)


########################################
# 前処理 ####
########################################
# 所沢人口(2020-08-08確認) ####
toko.jinkou = 344424

# データ読み込み ####
toko_url <- 'https://www.city.tokorozawa.saitama.jp/kenko/oshirase/tokorozawa_corona.html'
read_html(toko_url) %>% html_table() -> ht

# テーブルを引っこ抜く ####
# [[1]]は市内在住者の陽性者の状況なので除外
# 2020.08.08 表が増えたのでht[[4]]を追加。。
# toko.d <- rbind(ht[[2]],ht[[3]],ht[[4]])

# ht[1]を除外してbind_rows
toko.d <- bind_rows(ht[-1])


# 整形 ####
toko.d %>% filter(!str_detect(その他,"削除")) %>% 
  mutate(
    年月日 = ymd(paste0("2020年",発表日))
    , 年代 = str_replace(年代,"未就学児","10代")
    , 同居家族数 = case_when( 
        同居家族 == "なし" ~ "0"
      , 同居家族 == "調査中" ~ NA_character_
      , str_detect(同居家族,"名") ~  str_remove(同居家族, "名")
    )
    , 同居家族数 = as.numeric(同居家族数)
  ) %>% arrange(年月日) -> toko1

# toko1を日で集計する
toko1.ymd <- toko1 %>% group_by(年月日) %>% summarise(positives_byDay = n())

# toko2: 報告なしの日は0として、日別で感染者を集計
toko2  <- data.frame(年月日 = seq(min(toko1.ymd$年月日), max(toko1.ymd$年月日),1))
toko2  <- toko2 %>% 
  left_join( toko1.ymd, by = "年月日" ) %>% 
  mutate(positives_byDay = replace_na(positives_byDay,0) )



########################################
# グラフ ####
########################################
# 所沢市のコロナ累積陽性者数 ####
toko1 %>% group_by(年月日) %>% 
  summarise(件数=n()) %>% 
  ggplot(aes(年月日,cumsum(件数))) + 
  geom_line() +
  ggtitle(paste0("所沢市のコロナ累積陽性者数","(",Sys.Date(),"データ取得)")) +
  theme_gray (base_family = "HiraKakuPro-W3")


# 所沢市のコロナ陽性者数 ####
toko1 %>% mutate(性別=factor(性別, levels=c("男性","女性"))) %>% 
  ggplot(aes(年代,fill=職業)) + 
  geom_bar(stat = "count", position = "dodge") +
  facet_grid(性別 ~ paste0(month(年月日),"月")) +
  theme_gray (base_family = "HiraKakuPro-W3") +
  ggtitle(paste0("所沢市のコロナ陽性者数","(",Sys.Date(),"データ取得)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) 
  

# 曜日別・年代別 ####
toko1 %>% mutate(旬 = factor( if_else( day(年月日) < 11,"月初",if_else(day(年月日) < 21,"月中","月末")),levels=c("月初","月中","月末") )) %>%
  mutate(年代２極化 = if_else(年代 %in% c("60代","70代","80代","90代"), "高齢者(60代以上)", "非高齢者(50代以下)")) %>% 
  ggplot(aes( x = 旬, y=..count..,fill=年代２極化, )) + 
  geom_bar(position = "dodge") + 
  facet_grid(paste0(month(年月日),"月") ~ .) +
  theme_gray (base_family = "HiraKakuPro-W3")


# 曜日別・年代別 ####
toko1 %>% mutate(曜日 = factor(recode(wday(年月日),`1`="日",`2`="月",`3`="火", `4`="水",`5`="木", `6`="金",`7`="土"),levels=c("月","火","水","木","金","土","日") ) ) %>% 
  mutate(年代２極化 = if_else(年代 %in% c("60代","70代","80代","90代"), "高齢者(60代以上)", "非高齢者(50代以下)")) %>% 
  ggplot(aes( x = 曜日,y=..count..,fill=年代２極化)) + 
  geom_bar(position = "dodge") + 
  facet_grid(paste0(month(年月日),"月") ~ .) +
  ggtitle(paste0("所沢市のコロナ陽性者数(曜日別・年代別)","(",Sys.Date(),"データ取得)")) +
  theme_gray (base_family = "HiraKakuPro-W3")


# 直近１週間の新たな感染者数(10万人当たり) ####
toko2 %>% 
  mutate(x = cumsum(positives_byDay) ) %>% # 累積和
  mutate(y = lag(x,7)) %>% # 一週間前の累積和
  mutate(z = x - y ) %>% # 引き算して一週間分の累積和
  mutate(z = ( (z/toko.jinkou) * 100000 ) ) %>% # 人口10万人あたりに換算
  ggplot(aes(年月日,z)) + 
  geom_line() +
  geom_vline(xintercept = seq(ymd(20200407),ymd(20200525),0.5) , col="red", alpha=0.4, linetype=3) +
  annotate("text",x=ymd(20200407),y=2.5,label="4/7〜5/25 \n緊急事態宣言",hjust=0, size = 5, family="HiraKakuPro-W3") +
  ylab("10万人あたりのコロナ陽性者数") +
  ggtitle("所沢市の10万人あたりのコロナ陽性者数(直近1週間の新規感染者数)",
    subtitle=paste0(Sys.Date(),"データ取得","　所沢市人口 = ",format(toko.jinkou,big.mark = ","),"人とします。") )+
  theme_gray (base_family = "HiraKakuPro-W3")


# 年代性別月ごとのコロナ陽性者数
toko1 %>% ggplot(aes(年代, fill=性別)) + geom_bar(position = "dodge") +
  facet_wrap(~paste0(month(年月日),"月")) +
  ggtitle("年代性別月ごとのコロナ陽性者数",
          subtitle=paste0(Sys.Date(),"データ取得") )+
    theme_gray (base_family = "HiraKakuPro-W3")
