
# CASL2 / COMET2  by Scala  [![Build Status](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2.svg?branch=master)](https://travis-ci.org/matsutomu/ScaCASL2-ScaCOMET2)

CASL2 / COMET2 を Scalaで実装しました。

以下を大変参考にさせて頂きました。

[PyCASL2 & PyCOMET2 Java](https://github.com/oguna/pycasl2-pycomet2-java)

他のサイトも随時追加予定。


# memo
$sbt
sbt>assembly
$cp ./sca-casl2/target/scala-2.12/sca-casl2-assembly-1.0.jar ./
$java -jar sca-casl2-assembly-1.0.jar -a count1.cas

$sbt clean coverage test
$sbt clean coverage it:test
$sbt coverageReport

# License
CASL2 / COMET2 by Scala はGPL2に基づくフリーソフトウェアとして公開しています。