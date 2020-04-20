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
d <- d %>% select(EID, DOI, Year = 出版年, Title = タイトル, Journal = 出版物名, Auth_Info = `著者 + 所属機関`, Address = 連絡先住所)

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
        mutate(Auth_Info = str_split(Auth_Info, "; ")) %>%
        unnest(Auth_Info) %>%
        
        # コンマの数で所属情報の有無を判断し、条件分岐
        mutate(Author = ifelse(str_count(Auth_Info, ",") >= 2,                       
                            str_extract(Auth_Info, "^.*?,.*?(?=,)"), Auth_Info)) %>%  
        mutate(Institution = ifelse(str_count(Auth_Info, ",") >= 2, 
                             str_replace(Auth_Info, "(^.*?,.*?, )(.*$)", "\\2"), NA)) %>% 
        
        mutate(Auth_Fst = c(1, rep(0, nrow(.)-1))) %>%
        mutate(Auth_Corr = Author == str_extract(Address, "^.*?(?=;)")) %>%
        
        select(-Auth_Info, -Address) %>%
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





