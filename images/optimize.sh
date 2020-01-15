#!/bin/sh

for i in *.gif; do
    ffmpeg -i "$i" -c vp9 -b:v 0 -crf 41 "${i%.*}.webm";
    ffmpeg -i "$i" -c vp9 -b:v 0 -crf 41 "${i%.*}.mp4";
done

## https://css-ig.net/pingo
pingo *.png *.jpg
