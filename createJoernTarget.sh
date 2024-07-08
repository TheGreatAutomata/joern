#!/bin/bash

rm -r -f joernTarget
rm -f joernTarget.zip
rm -f joernTarget.tar.gz
cp -r joern-cli/target/universal joernTarget
#rm -r -f joernTarget/stage/frontends joernTarget/stage/schema-extender
#zip -r joernTarget.zip joernTarget
