# ----------------------------
# パッケージ
# ----------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(data.table)


# ----------------------------------
# データのインポート
# ----------------------------------

# データのインポート
d <- fread("scopus.csv") 

# 列名の確認
names(d)

# 列の選択
d <- d %>% select(EID, DOI, 出版年, タイトル, 出版物名, 著者情報 = `著者 + 所属機関`)

# EID以外の情報をネスト
d <- d %>% nest(data = setdiff(names(.), "EID"))

# ----------------------------------
# 関数作成
# ----------------------------------


clean.df <- function(x, y) {
    
    data <- x
    
    # 著者名の整理
    df <- data %>% 
        
        # 著者情報を";"で分割し、一人一行に展開
        mutate(著者情報 = str_split(著者情報, "; ")) %>%
        unnest(著者情報) %>%
        
        # コンマの数で所属情報の有無を判断し、条件分岐
        mutate(著者名 = ifelse(str_count(著者情報, ",") >= 2,                       
                            str_extract(著者情報, "^.*?,.*?(?=,)"), 著者情報)) %>%  
        mutate(著者所属 = ifelse(str_count(著者情報, ",") >= 2, 
                             str_replace(著者情報, "(^.*?,.*?, )(.*$)", "\\2"), NA)) %>% 
        
        select(-著者情報) %>%
        mutate(EID = y)

    df
}


# ---------------------
# 関数適用
# ---------------------

# 新しく作成したclean列に関数を適用した後のdfを格納

d$clean <- map2(d$data, d$EID, clean.df)


# ---------------------
# 出力
# ---------------------

d$clean %>% 
    bind_rows %>%
    write.csv("cleaned_df.csv")





