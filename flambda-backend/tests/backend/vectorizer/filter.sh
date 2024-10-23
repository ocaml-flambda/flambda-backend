#!/bin/bash

grep "*** Vectorization" $1 --line-regexp --max-count=1
