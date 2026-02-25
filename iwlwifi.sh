#!/bin/sh

###
# suspend/resume doesn't restart wifi. Place this file as root with +x in
# /usr/lib/systemd/system-sleep/iwlwifi.sh
###

case $1/$2 in
	pre/*)
	modprobe -r iwlmvm iwlwifi
	;;
	post/*)
	modprobe iwlmvm iwlwifi
esac

