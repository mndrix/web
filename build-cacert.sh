#!/bin/sh

# See http://curl.haxx.se/docs/caextract.html
curl -L https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt > cacert-web.pem
