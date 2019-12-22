# hbacklight
Backlight controller for Linux systems

## Installation
```
git clone https://github.com/paroxayte/hbacklight.git
cd hbacklight
cabal install
```

In order to use hbacklight without sudo, you may need to create a udev rule and ensure you are part of the group granted permissions. EG:
```
» cat /etc/udev/rules.d/10-backlight.rules
ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"
```
## Usage
```
Usage: hbacklight (-i|--id TARGET) [-v|--verbose] [-d|--delta [+,-,%,~]AMOUNT]
  Adjust backlight device

Available options:
  -h,--help                Show this help text
  -i,--id TARGET           Identifier of backlight device
  -v,--verbose             informative of backlight device state
  -d,--delta [+,-,%,~]AMOUNT
                           Modify the backlight value, ~ sets the value to
                           AMOUNT, else shift is relative. Defaults to ~
```

## Examples
```
» hbacklight -vi intel_backlight
device            intel_backlight
bl_power                        0
brightness                    200
actual_brightness             200
max_brightness               7500
type                          raw
» hbacklight -i intel_backlight -d 1000
```
