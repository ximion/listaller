#!/bin/bash
# set -x
[[ "$#" != 1 ]] && echo "Usage: $(basename $0) <path>" && exit 
pfad=${1}
to="utf-16"
files=$(find "$pfad" -iname "*.po" -type f -print)

    count=0
    for f in ${files[*]}; do
        count=$(( $count + 1))
        type="$(file "$f" | sed 's/^.*:\s\(UTF-8\|ASCII\)\s.*/\1/' | tr '[[:upper:]]' '[[:lower:]]')"
        if [ "$type" = "$to" ]; then
            echo -n "$count: $f is already $to! => done."
        else
            case $type in
                ASCII|ISO-8859-*|UTF-8|ascii|iso-8859-*|utf-8)
                echo -n "$count: Recode now $f to $to!"
                iconv -f "$type" -t "$to" "$f" > "${f}.${to}"
                mv "${f}.${to}" "$f"
                [[ $? = 0 ]] && echo -n " => done." || echo " => failed."
                ;;
                *)
                echo -n "$count: $f has wrong type => failed."
                ;;
                esac && echo
        fi
done
