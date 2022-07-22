git filter-branch -f --commit-filter '
        if [ "$GIT_COMMITTER_NAME" = "PharaohCola13" ] || [ "$GIT_COMMITTER_NAME" = "spencerriley620" ] || [ "$GIT_COMMITTER_NAME" = "Spencer Riley" ]; 
        then
                GIT_COMMITTER_NAME="PharaohCola13";
                GIT_AUTHOR_NAME="PharaohCola13";
                GIT_COMMITTER_EMAIL="academic@sriley.dev";
                GIT_AUTHOR_EMAIL="academic@sriley.dev";
                git commit-tree "$@";
        else
                git commit-tree "$@";
        fi' HEAD