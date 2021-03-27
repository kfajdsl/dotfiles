## Create dir file for GNU info

dir="$HOME/.cache/info"
mkdir -p "$dir"

now=$(date "+%s")
if [ -f "$dir/dir"  ]; then
    mtime=$(stat --format="%Y" .cache/info/dir)
else
  mtime=0
fi
let age="$now - $mtime"
if [ ! $age -lt 21600  ]; then

  infopath=("${(@s[:])INFOPATH}")
  for i in $infopath; do
    if [ -d $i ]; then
        for j in "$i/"*; do
          if [[ "$j" =~ (-[0-9]+|/dir)$  ]]; then
          else
            install-info --quiet "$j" "$dir/dir"
          fi
        done
    fi
  done

fi

export INFOPATH=$dir:$INFOPATH

