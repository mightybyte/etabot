#!/bin/sh

while true; do
  echo Restarting at `date` >> stdout
  dist/build/etabot/etabot > stdout 2> stderr
done
