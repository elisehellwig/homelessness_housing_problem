#!/bin/bash

for i in $(seq 2007 2019);
do
    Rscript src/1_download_CoCs.R $i FALSE TRUE
done