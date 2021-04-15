cd $1
dir=$(pwd)
curbr=$(git rev-parse --abbrev-ref HEAD)
cd ..
git fetch -p
git fetch
rm -rf $dir
git worktree prune
git branch -D $curbr
