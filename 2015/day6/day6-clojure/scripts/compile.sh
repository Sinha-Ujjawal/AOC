lein do clean, uberjar

gu install native-image

native-image -jar ./target/uberjar/day6-clojure-0.1.0-SNAPSHOT-standalone.jar \
             -H:Name=./target/day6-clojure \
             --report-unsupported-elements-at-runtime \
             --initialize-at-build-time \
             --verbose \
             --no-fallback \
             --no-server \
             "-J-Xmx3g"
