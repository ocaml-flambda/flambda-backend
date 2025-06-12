#!/bin/sh

grep "wrong argument 'linearization'" compiler-output.raw | grep "stop-after" | sed 's/^.*: wrong argument/wrong argument/'
