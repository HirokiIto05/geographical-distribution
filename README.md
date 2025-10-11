# geographical-distribution

## 概要
このレポジトリは、下記の論文の一部を再現するのに必要なコードを提供しています。

**論文**: [橋本, 伊藤, 近本(2023) "外国人と日本人の人口分布の比較：地方の労働力を支える技能実習生", RIETI Policy Discussion Papers](https://www.rieti.go.jp/jp/publications/summary/23080007.html)

> **注意**: 本文Section「3.1 国勢調査」に関連するコードは含まれません。

## 実行手順

### 通常の実行方法

1. **環境準備**
   - 必要に応じてcurrent directoryを変更
   - rawデータを各データ提供元からダウンロードし、所定のディレクトリに配置（[データセクション](#データ)を参照）

2. **リポジトリのセットアップ**
   ```bash
   git clone https://github.com/HirokiIto05/geographical-distribution.git
   cd geographical-distribution
   ```

3. **パッケージのインストール**
   ```r
   renv::restore()
   ```

4. **分析の実行**
   ```r
   source("06_administration/execute.r")
   ```

### Dockerを使用する場合

1. **環境準備**
   - 必要に応じてcurrent directoryを変更
   - rawデータを配置（[データセクション](#データ)を参照）

2. **リポジトリのクローンとコンテナ起動**
   ```bash
   git clone https://github.com/HirokiIto05/geographical-distribution.git
   cd geographical-distribution
   docker compose up -d
   ```

3. **RStudio Serverへアクセス**
   - ブラウザで `http://localhost:8787/` に接続

4. **パッケージのインストールと実行**
   ```r
   renv::restore()
   source("06_administration/execute.r")
   ```

> **Tips**: renvを使用している場合、ホストPCとコンテナ内のcacheパスを共有することで、パッケージのインストール時間を短縮できます。詳細は`docker-compose.yml`を参照してください。

## データ

### 3.2 住民基本台帳に基づく人口、人口動態及び世帯数調査

**データソース**: [e-Stat](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200241&tstat=000001039591)

**配置ディレクトリ**:
```
01_data/raw/jumin_kihon_daicho/
├── both/         # 全人口データ
├── japanese/     # 日本人人口データ
└── overseas/     # 外国人人口データ
```

### 3.3 在留外国人統計（旧登録外国人統計）

**データソース**: [e-Stat](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00250012&tstat=000001018034)

**配置ディレクトリ**:
```
01_data/raw/foreign_residents/
```

### 市町村合併関係データ

**出典**: 近藤(2019), "[市町村合併を考慮した市区町村パネルデータの作成](https://www.rieti.go.jp/jp/publications/summary/19030013.html)"

**データソース**: [GitHub - municipality-converter](https://github.com/keisukekondokk/municipality-converter)

**取得ファイル**: `municipality-converter/data_converter/municipality_converter_jp.csv`

**配置ディレクトリ**:
```
01_data/raw/municipality_converter/
```

## フォルダ構造（概要）

全てのディレクトリを提示しているわけではない。

```
geographical-distribution/
├── 01_data/                      # データ
│   ├── raw/                        # 生データ
│   │   ├── jumin_kihon_daicho/       # 住民基本台帳データ
│   │   ├── foreign_residents/        # 在留外国人統計データ
│   │   └── municipality_converter/   # 市町村合併データ
│   └── intermediate/               # 中間処理データ
├── 02_clean/                     # クリーニングスクリプト
├── 03_analysis/                
├── 05_output/                  
├── 06_administration/          
│   └── execute.r                 # メイン実行スクリプト
├── docker-compose.yml          
├── renv.lock                   # パッケージ
└── README.md                   
```

## トラブルシューティング

- パッケージのインストールでエラーが発生する場合は、Rのバージョンを確認してください
- Dockerでの実行時にポート8787が使用できない場合は、`docker-compose.yml`でポート設定を変更してください
- データファイルのパスエラーが発生する場合は、ファイルが正しいディレクトリに配置されているか確認してください