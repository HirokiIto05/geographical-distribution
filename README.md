# geographical-distribution

## 概要
このレポジトリは、下記の論文の一部を再現するのに必要なコードを提供しています。<br>[橋本, 伊藤, 近本(2023) "外国人と日本人の人口分布の比較：地方の労働力を支える技能実習生", RIETI Policy Discussion Papers )](https://www.rieti.go.jp/jp/publications/summary/23080007.html)

**※本文Section「3.1 国勢調査」に関連するコードは含まれません。**


## 実行の流れ
1. 必要に応じてcurrent directoryを変更
2. `git clone https://github.com/HirokiIto05/geographical-distribution.git`
4. `renv::restore()`を実行し、必要なpackageをインストール
5. `06_administration/execute.r`を実行

**dockerを使う場合**
1. 必要に応じてcurrent directoryを変更
2. `git clone https://github.com/HirokiIto05/geographical-distribution.git`
4. `docker compose up -d`
5. `http://localhost:8787/`に接続
6. `renv::restore()`を実行し、必要なpackageをインストール
7. `06_administration/execute.r`を実行


## データ

### Section 3.2 住民基本台帳に基づく人口、人口動態及び世帯数調査

**ダウンロード先URL**<br>
https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200241&tstat=000001039591<br>

**ディレクトリ**<br>
- 日本人人口<br>
`01_data/raw/jumin_kihon_daicho/japanese/`

- 外国人人口<br>
`01_data/raw/jumin_kihon_daicho/overseas/`


### Section 3.3 在留外国人統計（旧登録外国人統計）

**ダウンロード先URL**<br>
https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00250012&tstat=000001018034<br>

**ディレクトリ**<br>
`01_data/raw/foreign_residents/`


### 市町村合併関係
近藤(2019), "市町村合併を考慮した市区町村パネルデータの作成"<br>
https://www.rieti.go.jp/jp/publications/summary/19030013.html<br>

**ダウンロード先URL**<br>
https://github.com/keisukekondokk/municipality-converter<br>

`municipality-converter/data_converter/municipality_converter_jp.csv`<br>


**ディレクトリ**<br>

`01_data/raw/municipality_converter`<br>