#!/bin/sh

cd `dirname $0`/..
rm -rf */target

sbt assembly
rm ./sample/sca-*.jar
cp ./sca-casl2/target/scala-2.12/sca-casl2-assembly-0.1.jar ./sample/
cp ./sca-comet2/target/scala-2.12/sca-comet2-assembly-0.1.jar ./sample/