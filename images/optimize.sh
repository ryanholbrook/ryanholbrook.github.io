#!/bin/sh

for i in intro-to-dl2.gif; do
    # webm (VP9) Encoding
    # Goal is best quality
    ffmpeg -y -i "$i" -vf scale=800:-1 -c:v libvpx-vp9 -b:v 0 -crf 30 -pass 1 -an -f webm /dev/null && \
        ffmpeg -i "$i" -vf scale=800:-1 -c:v libvpx-vp9 -b:v 0 -crf 30 -deadline best -pass 2 -an "${i%.*}.webm";
    # MP4 (H.264) Encoding
    # Goal is widest compatibility
    # See https://trac.ffmpeg.org/wiki/Encode/H.264 for compatibility information
    ffmpeg -i "$i" -vf scale=800:-1 -c:v libx264 -profile:v baseline -level 3.0 -pix_fmt yuv420p -crf 33 -preset veryslow -tune animation -an "${i%.*}.mp4";
done

## https://css-ig.net/pingo
#pingo *.png *.jpg
