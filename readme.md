
# CASL2 / COMET2  by Scala  [![Build Status](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2.svg?branch=master)](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2)

CASL2 / COMET2 を Scalaで実装しました。以下を大変参考にさせて頂きました。

- [PyCASL2 & PyCOMET2 Java](https://github.com/oguna/pycasl2-pycomet2-java)
- [CASLIIアセンブラ & シミュレータ PyCASL2 & PyCOMET2](http://www.image.med.osaka-u.ac.jp/member/nakamoto/pycasl2/index.html)
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

```shell
$sbt
sbt>assembly
$cp ./sca-casl2/target/scala-2.12/sca-casl2-assembly-1.0.jar ./
$cp ./sca-comet2/target/scala-2.12/sca-comet2-assembly-1.0.jar ./
$java -jar sca-casl2-assembly-1.0.jar -a sample01.cas
$java -jar sca-comet2-assembly-1.0.jar sample01.com
```

## ScaCasl2

更新予定

## ScaComet2

更新予定

## Other
### Unit Test
$sbt test

### Coverage
$sbt clean coverage test
$sbt clean coverage it:test
$sbt coverageReport

# License
CASL2 / COMET2 by Scala はGPL2に基づくフリーソフトウェアとして公開しています。