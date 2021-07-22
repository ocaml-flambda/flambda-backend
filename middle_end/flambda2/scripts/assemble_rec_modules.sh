#!/bin/bash

set -eu -o pipefail

if [ "$#" != "3" ]; then
    echo "syntax: $0 TEMPLATE_FILE REC_MODULES_FILE OUTPUT_FILE"
    exit 1
fi

TEMPLATE=$1
REC_MODULES_FILE=$2
OUTPUT=$3

DELIMITER="-- module rec binding here --"
# CR mshinwell: Fix this to use something else -- we should be able to
# delete the warning stanzas on .rec.ml files -- they are overridden by the
# one in the .templ.ml file.
WARNING_DELIMITER="@@@ocaml.warning"

REC_BINDINGS=$(cat $REC_MODULES_FILE)

echo "(* Generated automatically -- do not edit *)" > $OUTPUT
echo >> $OUTPUT

echo "# 1 \"$TEMPLATE\"" >> $OUTPUT
temp=$(mktemp)
grep --color=never -B 1000000 -- "$DELIMITER" $TEMPLATE \
    | sed '$d' >> $temp
TEMPLATE_FIRST_PART_LENGTH=$(wc -l $temp | awk '{print $1}')
cat $temp >> $OUTPUT
rm -f $temp

FIRST=1

for ML in $REC_BINDINGS; do
    MLI=${ML}i
    MODNAME=$(echo $ML | \
      sed 's/\.ml$//' | \
      sed 's:^.*/::' | \
      awk '{ print toupper(substr($0,1,1)) tolower(substr($0,2)) }' | \
      sed 's/\.rec//')

    if [ "$FIRST" = "1" ]; then
        echo "  module rec $MODNAME : sig" >> $OUTPUT
    else
        echo "  and $MODNAME : sig" >> $OUTPUT
    fi

    LINE=$(cat $MLI | grep -B 1000000 "$WARNING_DELIMITER" | wc -l)
    echo "# $(($LINE + 1)) \"$MLI\"" >> $OUTPUT

    cat $MLI \
        | grep -A 1000000 "$WARNING_DELIMITER" \
        | tail -n +2 \
        >> $OUTPUT

    echo "  end = struct" >> $OUTPUT

    LINE=$(cat $ML | grep -B 1000000 "$WARNING_DELIMITER" | wc -l)
    echo "# $(($LINE + 1)) \"$ML\"" >> $OUTPUT

    cat $ML \
      | grep -A 1000000 "$WARNING_DELIMITER" \
      | tail -n +2 \
      >> $OUTPUT

    echo "  end" >> $OUTPUT
    echo >> $OUTPUT

    FIRST=0
done

echo "# $(($TEMPLATE_FIRST_PART_LENGTH + 2)) \"$TEMPLATE\"" >> $OUTPUT
grep --color=never -A 1000000 -- "$DELIMITER" $TEMPLATE | \
    tail -n +2 >> $OUTPUT
