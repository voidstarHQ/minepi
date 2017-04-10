for img in xx24/*; do convert "$img" -alpha set -define bmp:format=bmp4 xx32/"${img##xx24/}" ; done && file xx32/*
