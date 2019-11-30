echo "" > diff_file.txt ;  for d in ./tests/unit/*.tz ; do bash -c "echo $d ; ./michelson.py run $d |  diff -y - \"$d.expected\"" >> diff_file.txt ; done
