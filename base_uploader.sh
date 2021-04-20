#!/bin/bash

python3 base_downloader.py

git add .
git commit -m "$desc"
git push
