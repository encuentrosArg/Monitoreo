#!/bin/bash

git pull

python3 base_downloader.py

git add .
git commit -m "Actualizacion de base"
git push

