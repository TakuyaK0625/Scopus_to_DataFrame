# Scopus_to_DataFrame
Scopusからダウンロードできる論文データを、著者名単位のデータフレームに展開するためのコードです。各著者名には筆頭著者か否か（Auth_Fst）と、責任著者か否か（Auth_Corr）の情報も加えています。

## 備考
DOI: DOI  
Year: 出版年
Title: 論文タイトル
Journal: 出版物名
Author: 著者
Institution: 所属機関
Auth_Fst : 筆頭著者か否か（1/0）
Auth_Corr: 責任著者か否か（TRUE/False, NAは連絡先住所がない論文）
EID: EID

## 注意点
* 責任著者の判定には連絡先住所列を用いており、同列の先頭の人名とAuthor列が同一であるかを判定しています。そのため、責任著者が複数いたり、そもそも連絡先住所に何も情報が入っていない場合には、正確な情報にはなりませんのでご注意ください。
* 所属機関が複数ある著者の場合、所属機関の情報は同じ行のInstitution列に格納されます（本当は行を分けたかったのですが、切り分ける規則が思い当たりませんでした）。
