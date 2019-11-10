for d in ./tests/unit/*.tz ; do bash -c "echo $d ; ./michelson.py run $d |  diff -y - \"$d.expected\"" | less ; done
