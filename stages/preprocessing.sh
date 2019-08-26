#!/bin/bash
sed -i '/poste/d' test.txt
sed -i '/^$/d' test.txt
wc -l test.txt
sed -i 's/\s\s\+/#/g' test.txt
wc -l test.txt
