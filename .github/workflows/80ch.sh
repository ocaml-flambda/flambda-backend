#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                    James Rayman, Jane Street, New York                 *#
#*                                                                        *#
#*   Copyright 2025 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# 80ch.sh: A GitHub-actions-friendly script which finds lines added in the
#          current feature which are longer than 80 characters. To be less
#          annoying, 80ch.sh never fails, but instead emits warnings, and only
#          checks a subset of all files (See SKIP FILES).

set -u

feature_base="HEAD^1" # GitHub automatically makes us a merge commit (HEAD),
                      # so our feature base is just the first parent (HEAD^1)
                      # of this merge commit

find_diff() {
# Iterate through all files changed since this branch forked off of main.
#
#  Separate file names with NULLs. This is      Read the next token
#  standard for lists of files since file       |    Tokens are delimited
#  names may contain newlines        |          |    |         with NULLs
#                                    |-         |--- |-------
git diff --name-only "$feature_base" -z | while read -d $'\0' -r changed_file
#                                                             ^| ^^^^^^^^^^^|
#                   Don't allow backslashes to escape characters            |
#                                            Store the token in $changed_file
do
  # SKIP FILES

  # Only check regular files that currently exist
  [[ -f "$changed_file" ]] || continue

  # Only check ml, mli, and mly files for long lines
  [[ "$changed_file" =~ \.ml(i|y)?$ ]] || continue

  # Don't check tests for long lines
  # (a more sophisticated script would instead remove %%expect blocks)
  [[ "$changed_file" == testsuite/tests/* ]] && continue

  # Don't check autoformatted files (there's a bug in `ocamlformat` that
  # sometimes produces lines over 80 characters)
  ocamlformat --print-config "$changed_file" 2>&1 \
  | grep -q disable=false && continue

  # Bash doesn't allow for comments on multiline commands, so excuse the
  # workaround of putting a group of arguments in a variable
  f='--old-line-format= --unchanged-line-format= --new-line-format=%dn:%L'
  #  |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ |^^^^^^^^^^^^^^^^^^^^^^^
  #  Don't print deleted or unchanged lines      Show added lines (%L),
  #                                              prepended with the line number
  #                                              (%dn) and a colon

  #                                                If $changed_file is new, then
  #                                                `git show` will error, but
  #                                                ignoring the error results in
  #         Print the contents of $changed_file    a blank output, which is what
  #         at the feature base                    we want
  #         |------------------------------------- |----------
  diff $f <(git show "$feature_base:$changed_file" 2>/dev/null) \
          "$changed_file" \
  | grep -E ":.{81}" | while read -r -d $'\n' long_line
  # |^^^^^^^^^^^^^^^
  # Search through the output of `diff`,
  # looking for 81 characters after the line
  # number and colon
  do
    line="${long_line#*:}"    # These parameter expansions split
    number="${long_line%%:*}" # on the first colon

    # Warning workflow command for GitHub Actions
    printf '::error file=%s,line=%s,title=%s::%s\n' \
      "$changed_file" "$number" \
      'Line is longer than 80 characters' \
      'Consider rewrapping this line'
  done
done
}

output="$(find_diff)"
printf '%s' "$output"
[[ -z "$output" ]] # if there is output, exit 1
