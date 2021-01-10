#!/bin/bash

MODE=2560x1440 

xrandr \
  --output DisplayPort-0 --mode $MODE --pos 1440x530 --rotate normal --primary \
  --output DisplayPort-1 --mode $MODE --pos 0x0 --rotate left \
  --output DisplayPort-2 --mode $MODE --pos 4000x0 --rotate right \
  --output HDMI-A-0 --off \
  --output DVI-D-0 --off \

