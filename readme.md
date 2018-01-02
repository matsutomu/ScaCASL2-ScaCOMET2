
# CASL2 / COMET2  by Scala  [![Build Status](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2.svg?branch=master)](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2)

## 目次
- [概要](#概要)
- [動作環境](#動作環境)
- [ビルド及び実行](#ビルド及び実行)
- [利用方法](#利用方法)
- [その他](#その他)
- [謝意](#謝意)
- [ライセンス](#ライセンス)


## 概要

ScaCasl2 & ScaComet2 は、[独立行政法人情報処理推進機構](https://www.ipa.go.jp/) の基本情報処理技術者試験で出題されるアセンブラ言語（CASLII）のアセンブラと実行環境のCOMETIIをScalaで実装したものとなります。

CASLII 及び COMETII の仕様は、[試験要綱及びシラバスなど](https://www.jitec.ipa.go.jp/1_04hanni_sukiru/_index_hanni_skill.html#yougo)をご参照下さい。下部にPDF形式で「試験で使用する情報技術に関する用語・プログラム言語など」Ver3.0が存在します。

COMETIIの構成は、基本的に16bitをひとかたまりとして考えた『1語』を基本単位として考えています。
- 主記憶（= memory） は更に65536語が並んだ箱
- レジスタ（= Register）は、主記憶とは別の箱（物理的なハードウェアではメモリより高速にアクセス可能）
- レジスタにはGR0からGR7やSP（= StackPointer）、PR（= ProgramRegister）、FR (= FlagRegister)が存在
- Java版と同様にGR8がSPを兼ねる
と言った特徴があります。

## 動作環境

ビルド、実行及び開発には以下が必要です。
- [Java](http://www.oracle.com/technetwork/jp/java/javase/overview/8-whats-new-2157071-ja.html) 1.8+
- [Scala](https://www.scala-lang.org/) 2.12.3+ (開発するなら)

## ビルド及び実行

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

## 利用方法
### ScaCasl2

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

#### options  

| パラメータ | 内容 |  
 -- | --   
| -v | バージョンの表示 |  
| -h | ヘルプ表示 |  
| -a | アセンブルをしつつ dump & コード & シンボル表をコンソールに表示 |


### ScaComet2
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

#### options1

| パラメータ | 内容 |  
 -- | --   
| -v | バージョンの表示 |  
| -h | ヘルプ表示 |  
| -d | デバッグモードで実行 |  

#### options2

| パラメータ | 内容 |  
 -- | -- 
| -c | step数をカウントして表示 |  
| -du | 最終状態をlast_state.txtに保存 |  
| -w | 引数に指定されたレジスターの情報を表示 |  

#### デバッグモード
options1に"-d"を指定して実行した場合に、デバックモードで実行可能です。

```
$java -jar sca-comet2-assembly-0.1.jar -d sample02.com
load /Users/matsutomu/work/github/ScaCASL2-ScaCOMET2/sample/sample02.com ...
done.
ScaComet2>
```

上記状態で、"s"を入力してEnterを押すと1stepだけ実行されます。
その状態で"p"を入力すると、レジスターの情報が表示されます。

```
ScaComet2>s
ScaComet2>p
PR #0003 [ #0003: #1000 #001C         LD       GR0, #001C ]  STEP 1
SP #FF00(  65280) FR(OF, SF, ZF) 000  (      0)
GR0 #0000(      0) GR1 #0000(      0) GR2 #0000(      0) GR3 #0000(      0)
GR4 #0000(      0) GR5 #0000(      0) GR6 #0000(      0) GR7 #0000(      0)
ScaComet2>
```

"r"を入力すると最後まで実行します。

| コマンド | パラメータ | 内容 |  
 --  | -- | -- 
| q  | - | プログラムを終了 |  
| r  | - | プログラムを続けて実行 |  
| s  | - | 1ステップ実行 |  
| p  | -  | レジスターの情報を表示 |  
| b  | Address | 指定したアドレスにブレークポイントを設定 |  
| i  | - | ブレークポイントの一覧を表示 |   
| d  | Index   | 指定したインデックスのブレークポイントを削除 |  
| di | Address | 指定したアドレスからディスアセンブリしてコードを表示 |   
| du | Address | 指定したアドレスからメモリーの情報を16進数で表示 |  
| st | - | スタックの情報を表示（128語） |  
| j  | Address | 指定したアドレスをプログラムレジスターに設定（次の実行アドレスを指定） |   
| m  | Address  Value | メモリーの指定したアドレスに値を設定 |  
| h  | - | ヘルプを表示 |  


## その他
### Unit Test
$sbt test

### Coverage Report
$sbt clean coverage test  
$sbt coverageReport

## 謝意
はるか昔の学生の頃は、アセンブラ（CASLII / COMETII）なんて全然理解できませんでした。
が、その後色々経験し、Github上で色々なソースを見る事ができ、Scalaを学習する事で実装する事ができました。
特に、以下のソースコード及びサイトを大変参考にさせてもらいました。

- [PyCASL2 & PyCOMET2 Java](https://github.com/oguna/pycasl2-pycomet2-java)
- [CASLIIアセンブラ & シミュレータ PyCASL2 & PyCOMET2](http://www.image.med.osaka-u.ac.jp/member/nakamoto/pycasl2/index.html)
- [YACASL2](http://www.j8takagi.net/yacasl2/)
- [進数変換](https://hogehoge.tk/tool/number.html)
- [dwango scala text](http://dwango.github.io/scala_text/)


## ライセンス
CASL2 / COMET2 by Scala はGPL2に基づくフリーソフトウェアとして公開しています。