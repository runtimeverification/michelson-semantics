
cd ..
git worktree add $1
cd $1
git rebase dev
git rebase -i master
