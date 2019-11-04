#!/usr/bin/env bash

out="$1"
date=$(date +"%x")

cat > $out <<EOF
---
title: Amulet Error Index ($date)
date: $date
omit_footer: true
omit_header: true
---

This is an index of all the Amulet errors with long-form explanations,
as can be found [here] in the compiler repository, generated at $date.

[here]: https://github.com/tmpim/amulet/tree/master/doc

Keep in mind that the compiler can also print these explanations, using
the \`amc explain [number]\`{} subcommand.
EOF

cat errors.txt | while read -r file_desc; do
  fname=$(echo $file_desc | cut -d' ' -f1)
  num=$(echo $fname | sed -re 's;\w+/0*([0-9]{1,4}).txt;\1;g')
  anchor=$(echo $file_desc | cut -d' ' -f3)

  printf "### E%.4d: \"%s\" {#%s}\n" $num $anchor $anchor >> $out

  cat errors/$fname | \
    sed -re 's/`([ ,.])/`{.amulet}\1/g' \
    >> $out

  echo >> $out
done
