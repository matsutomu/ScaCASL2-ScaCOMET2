
# CASL2 / COMET2  by Scala  [![Build Status](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2.svg?branch=master)](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2)

CASL2 / COMET2 を Scalaで実装しました。以下を大変参考にさせて頂きました。

- [PyCASL2 & PyCOMET2 Java](https://github.com/oguna/pycasl2-pycomet2-java)
- [CASLIIアセンブラ & シミュレータ PyCASL2 & PyCOMET2](http://www.image.med.osaka-u.ac.jp/member/nakamoto/pycasl2/index.html)
- [YACASL2](http://www.j8takagi.net/yacasl2/)
- [進数変換](https://hogehoge.tk/tool/number.html)

## 概要

ScaCasl2 & ScaComet2 は、[独立行政法人情報処理推進機構](https://www.ipa.go.jp/) の基本情報処理技術者試験で出題されるアセンブラ言語（CASLII）のアセンブラと実行環境のCOMETIIをScalaで実装したものとなります。

CASLII 及び COMETII の仕様は、[試験要綱及びシラバスなど](https://www.jitec.ipa.go.jp/1_04hanni_sukiru/_index_hanni_skill.html#yougo)をご参照下さい。下部にPDF形式で「試験で使用する情報技術に関する用語・プログラム言語など」Ver3.0が存在します。

COMETIIの構成は、基本的に16bitをひとかたまりとして考えた『1語』を基本単位として考えています。
- 主記憶（= memory） は更に65536語が並んだ箱
- レジスタ（= Register）は、主記憶とは別の箱（物理的なハードウェアではメモリより高速にアクセス可能）
- レジスタにはGR0からGR7やSP（= StackPointer）、PR（= ProgramRegister）、FR (= FlagRegister)が存在
- Java版と同様にGR8がSPを兼ねる
と言った特徴があります。

## ビルド & 実行
sample ディレクトリにbuild用のシェルとCASLIIサンプルを置いています。

```shell
$cd sample
$./build.sh
$java -jar sca-casl2-assembly-0.1.jar sample01.cas
[success]output to sample/sample01.com
$java -jar sca-comet2-assembly-0.1.jar sample01.com
load /Users/matsutomu/work/github/ScaCASL2-ScaCOMET2/sample/sample01.com ...
done.
HELLO CASL2 & COMET2
```

## ScaCasl2
### 利用方法

CASLII のファイルをアセンブルする方法です。
```
$java -jar sca-casl2-assembly-"version".jar [options] <CASL file path> <Assemble file path>
```

例えば、sample01.cas をアセンブルする時は以下の内容です。
```
$java -jar sca-casl2-assembly-0.1.jar sample01.cas
[success]output to sample/sample01.com
```

実行ファイルを指定する場合の例です。
```
$java -jar sca-casl2-assembly-0.1.jar sample01.cas sample-exe.com
[success]output to sample/sample-exe.com
```

options  

| パラメータ | 内容 |  
 -- | --   
| -v | バージョンの表示 |  
| -h | ヘルプ表示 |  
| -a | アセンブルをしつつ dump & コード & シンボル表をコンソールに表示 |


## ScaComet2
### 利用方法
COMETII を実行する方法です。
```
$java -jar sca-comet2-assembly-0.1.jar [options1] [options2] <execute file path>
```

sample01.com を実行する場合には以下のようになります。
```
$java -jar sca-comet2-assembly-0.1.jar sample01.com
load /Users/matsutomu/work/github/ScaCASL2-ScaCOMET2/sample/sample01.com ...
done.
HELLO CASL2 & COMET2
```

◆ options1  
| パラメータ | 内容 |  
 -- | --   
| -v | バージョンの表示 |  
| -h | ヘルプ表示 |  
| -d | デバッグモードで実行 |  

◆ options2  
| パラメータ | 内容 |  
 -- | -- 
| -c | step数をカウントして表示 |  
| -du | 最終状態をlast_state.txtに保存 |  
| -w | 引数に指定されたレジスターの情報を表示 |  

### デバッグモード
更新予定

## Other
### Unit Test
$sbt test

### Coverage Report
$sbt clean coverage test  
$sbt coverageReport

# License
CASL2 / COMET2 by Scala はGPL2に基づくフリーソフトウェアとして公開しています。