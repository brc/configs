if [ "$(hostname)" = Bretts-MacBook-Pro.local ]; then
    alias vm='ssh -o LogLevel=QUIET -t bunker'
    alias pb='vm cat /tmp/mbp |pbcopy'
    # alias emacs='vm /home/brc/bin/emacsclient.sh'
    # alias em=emacs
    alias kpf='~/pf'
    alias gf-sync-all='/gf/common/tool/gitlab-sync/gitlab-sync --dir /gf/ --all'

    export KEYBASE_USER="$(cat /f/c/keybase.username)"
    export GITLAB_TOKEN="$(cat /f/c/gitlab/brc-token)"
    export CLOUDFLARE_API_TOKEN="$(cat /f/c/cloudflare-api-token-*_network)"
    export CLOUDSDK_PYTHON=/opt/homebrew/opt/python@3.12/libexec/bin/python

    if [ "${SHELL##*/}" = zsh ]; then
        source /opt/homebrew/share/zsh/site-functions/_google_cloud_sdk
        source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
        source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    fi

    # don't know where this came from, but got appended to ~/.zshrc or similar
    PATH="/Users/brett/perl5/bin${PATH:+:${PATH}}"; export PATH;
    PERL5LIB="/Users/brett/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="/Users/brett/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"/Users/brett/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=/Users/brett/perl5"; export PERL_MM_OPT;

    ##########################################################################
    # Functions to wrap rsync
    ##########################################################################
    vm-get() {
        if [ -z "$1" ]; then
            >&2 echo "usage: ${FUNCNAME[0]} <SRC> [<DST>]"
            return 1
        fi
        #
        local src="$1"
        local dst="$2"
        shift 2
        #
        set -x
        rsync -vrlptcu "bunker:$src" "${dst:-.}" "$@"
        set +x
    }

    vm-backport() {
        if [ -z "$1" ]; then
            >&2 echo "usage: ${FUNCNAME[0]} <SRC> <DST>"
            return 1
        fi
        #
        local src="$1"
        local dst="$2"
        shift 2
        #
        set -x
        rsync -vrlptcu  "${src}" "bunker:${dst}" "$@"
        set +x
    }

    rsync-safe() {
        local op="$1"
        local path_spec="$2"
        local me="${FUNCNAME[0]}"
        shift 2

        if [ -z "${op}" -o -z "${path_spec}" ] ||
           [ "${op}" != get -a "${op}" != backport ]
        then
            >&2 echo "usage: ${me} {get|backport} {<PATH>} [<RSYNC_ARGS>]"
            return 1
        fi

        vm-"${op}" "${path_spec}" "${path_spec}" \
            "$@" \
            --itemize-changes \
            --dry-run ||
                return 1

        if ! rsync-confirm; then
            >&2 echo Aborting.
            return 1
        fi

        vm-"${op}" "${path_spec}" "${path_spec}" "$@"
    }

    rsync-confirm() {
        local confirmation='I will not lose work'
        cat <<-__EOF__

	Please review the output VERY carefully.
	Are you 100% sure you want to do this?

	__EOF__
        printf "%s: " "Please type: '${confirmation}'"
        read
        [ "${REPLY}" = "${confirmation}" ]
    }

    gen-rsync-excl() {
        printf -- "--exclude %s\n" "$@"
    }

    gen-find-excl() {
        local inner
        inner="$(printf -- "-o -path '%s' " "$@" |cut -b4-)"
        printf -- "\( %s \) -prune\n" "$inner"
    }

    # usage: vm-find-args /target1 /target2 ... [-- /ignore1 /ignore2 ...]
    vm-find-args() {
        local dirs=()
        local excl=
        local max_days=30

        # Collect directory args until "--"
        while [[ $# -gt 0 && "$1" != "--" ]]; do
            dirs+=("$1")
            shift
        done

        # Skip the "--" delimiter
        if [[ "$1" == "--" ]]; then
            shift
        fi

        # Remaining args are exclusions
        if [ -n "$1" ]; then
            excl="$(gen-find-excl "$@")"
        fi
        #
        printf -- \
        "%s %s -o -mtime -${max_days} \( -type f -o -type l \) -printf '%s'\n" \
            "${dirs[*]}" \
            "${excl}" \
            '%T+ %p\n'
    }

    # usage: vm-show /target1 /target2 ... [-- /ignore1 /ignore2 ...] [-- <DIFF_ARGS>]
    #   NB. XXX Can't pass diff(1) args without exclusion args!
    vm-show() {
        local dirs=()
        local excl=()

        # Collect directory args until "--"
        while [[ $# -gt 0 && "$1" != "--" ]]; do
            dirs+=("$1")
            shift
        done
        [[ "$1" == "--" ]] && shift

        # Collect exclusions until "--"
        while [[ $# -gt 0 && "$1" != "--" ]]; do
            excl+=("$1")
            shift
        done
        [[ "$1" == "--" ]] && shift

        # Remaining args go to diff(1)
        diff "$@" \
            <(ssh bunker "find $(vm-find-args "${dirs[@]}" -- "${excl[@]}")" |sort -r) \
            <(eval "/opt/homebrew/bin/gfind $(vm-find-args "${dirs[@]}" -- "${excl[@]}")" |sort -r)
    }

    vm-diff() {
        for f in $(vm-show "$@" -- --color=no |awk '{print $3}' |sort -u); do
            if [ -n "$f" ]; then
                echo "$f":
                diff <(ssh bunker "cat '$f' || echo /dev/null") "$f"
                echo
            fi
        done
    }

    ##########################################################################
    # /gf
    ##########################################################################
    RSYNC_GIT_EXCLUSIONS=(
        .claude/
        .venv/
        '.terraform*/'
        'backend/charts/vault/tmp/kubectl.*.out'
        common/tool/gitlab-sync/gitlab-sync
        istio/releases/
        'bin/istioctl-*'
    )
    FIND_GIT_EXCLUSIONS=(  # NB. Leading asterisk, no trailing slash
        '*/.git'  # NOTE don't show this in diff
        '*/.claude'
        '*/.venv'
        '*/.terraform*'
        '*/backend/charts/vault/tmp/kubectl.*.out'
        '*/common/tool/gitlab-sync/gitlab-sync'
        '*/istio/releases'
        '*/bin/istioctl-*'
    )

    RSYNC_GIT_FILTER_ARGV=(
        --no-checksum       # don't checksum the files (too slow)
        --open-noatime      # don't update the atimes (maybe faster)
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/gf
        $(gen-rsync-excl "${RSYNC_GIT_EXCLUSIONS[@]}")
    )

    get-git() {
        local repo_path="$1"
        shift
        #
        if [ -z "${repo_path}" ]; then
            >&2 echo "Argument error: No repo path provided"
            return 1
        fi
        #
        rsync-safe \
            get \
            "${repo_path}" \
            "${RSYNC_GIT_FILTER_ARGV[@]}" \
            "$@"
    }

    backport-git() {
        local repo_path="$1"
        shift
        #
        if [ -z "${repo_path}" ]; then
            >&2 echo "Argument error: No repo path provided"
            return 1
        fi
        #
        rsync-safe \
            backport \
            "${repo_path}" \
            "${RSYNC_GIT_FILTER_ARGV[@]}" \
            "$@"
    }

    show-git() {
        vm-show "${1:-/gf/}" -- "${FIND_GIT_EXCLUSIONS[@]}"
    }

    diff-git() {
        vm-diff "${1:-/gf/}" -- "${FIND_GIT_EXCLUSIONS[@]}"
    }

    ##########################################################################
    # /gi/configs
    ##########################################################################
    RSYNC_DOTFILE_EXCLUSIONS=(
        'home/spacemacs/.cache/*'  # NOTE Used this way for prior --include
        .claude/
        .git/index
        home/.vim/.undo/
        home/.zsh/history
        'home/.zsh/.zcompdump*'
        home/spacemacs/recentf
        home/spacemacs/tree-sitter/
        home/spacemacs/elpa/
        home/spacemacs/quelpa/
        home/spacemacs/eln-cache
    )
    FIND_DOTFILE_EXCLUSIONS=(  # NB. Leading asterisk, no trailing slash
        '*/home/spacemacs/.cache'
        '*/.claude'
        '*/.git'  # NOTE Don't show this in diff
        '*/home/.vim/.undo'
        '*/home/.zsh/history'
        '*/home/.zsh/.zcompdump*'
        '*/home/spacemacs/recentf'
        '*/home/spacemacs/tree-sitter'
        '*/home/spacemacs/elpa'
        '*/home/spacemacs/quelpa'
        '*/home/spacemacs/eln-cache'
    )

    RSYNC_DOTFILE_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/git/brc/configs
        # INCLUDES ---------------------------------------
        #--include home/spacemacs/.cache/

        # XXX This doesn't work correctly across boxes with Projectile because
        #     Projectile dereferences symlinks and the paths end up being
        #     different (so the Treemacs path won't match the project).
        #--include home/spacemacs/.cache/treemacs-persist

        # EXCLUDES ---------------------------------------
        $(gen-rsync-excl "${RSYNC_DOTFILE_EXCLUSIONS[@]}")
    )

    get-dotfiles() {
        rsync-safe \
            get \
            /git/brc/configs/ \
            "${RSYNC_DOTFILE_FILTER_ARGV[@]}" \
            "$@"
    }

    backport-dotfiles() {
        rsync-safe \
            backport \
            /git/brc/configs/ \
            "${RSYNC_DOTFILE_FILTER_ARGV[@]}" \
            "$@"
    }

    show-dotfiles() {
        vm-show /git/brc/configs/ -- "${FIND_DOTFILE_EXCLUSIONS[@]}"
    }

    diff-dotfiles() {
        vm-diff /git/brc/configs/ -- "${FIND_DOTFILE_EXCLUSIONS[@]}"
    }

    ##########################################################################
    # /data
    ##########################################################################
    RSYNC_DATA_DIRS=(
        /d/istio/
        /d/k8s/
        /df/
    )

    RSYNC_DATA_EXCLUSIONS=(
        .claude/
        rsync-saved/
        cluster.old/
    )
    FIND_DATA_EXCLUSIONS=(  # NB. Leading asterisk, no trailing slash
        '*/.claude'
        '*/rsync-saved'
        '*/cluster.old'
    )

    RSYNC_DATA_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/data
        $(gen-rsync-excl "${RSYNC_DATA_EXCLUSIONS[@]}")
    )

    get-data() {
        for d in "${RSYNC_DATA_DIRS[@]}"; do
            rsync-safe \
                get \
                "$d" \
                "${RSYNC_DATA_FILTER_ARGV[@]}" \
                "$@"
        done
    }

    backport-data() {
        for d in "${RSYNC_DATA_DIRS[@]}"; do
            rsync-safe \
                backport \
                "$d" \
                "${RSYNC_DATA_FILTER_ARGV[@]}" \
                "$@"
        done
    }

    show-data() {
        vm-show "${RSYNC_DATA_DIRS[@]}" -- "${FIND_DATA_EXCLUSIONS[@]}"
    }

    diff-data() {
        vm-diff "${RSYNC_DATA_DIRS[@]}" -- "${FIND_DATA_EXCLUSIONS[@]}"
    }

    ##########################################################################
    # sh-lib
    ##########################################################################
    RSYNC_SHLIB_EXCLUSIONS=(
        .claude/
        .git/index
    )
    FIND_SHLIB_EXCLUSIONS=(  # NB. Leading asterisk, no trailing slash
        '*/.claude'
        '*/.git'  # NOTE Don't show in diff
    )

    RSYNC_SHLIB_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /d/rsync-saved/sh-lib
        $(gen-rsync-excl "${RSYNC_SHLIB_EXCLUSIONS[@]}")
    )

    get-shlib() {
        rsync-safe \
            get \
            /git/brc/sh-lib/ \
            "${RSYNC_SHLIB_FILTER_ARGV[@]}" \
            "$@"
    }

    backport-shlib() {
        rsync-safe \
            backport \
            /git/brc/sh-lib/ \
            "${RSYNC_SHLIB_FILTER_ARGV[@]}" \
            "$@"
    }

    show-shlib() {
        vm-show /git/brc/sh-lib/ -- "${FIND_SHLIB_EXCLUSIONS[@]}"
    }

    diff-shlib() {
        vm-diff /git/brc/sh-lib/ -- "${FIND_SHLIB_EXCLUSIONS[@]}"
    }
fi

gf-status-all() {
    kill_line() {
        # erase current line ("\33[2K") and return the carriage ("\r")
        printf "\033[2K\r"
    }
    gf-recursion-helper() {
        local dir
        local git_status
        #
        for dir in $(find . -maxdepth 1 -type d); do
            if [ "${dir}" = . ]; then
                continue
            fi
            git_status=
            kill_line
            printf -- "%s" "${dir} ..."
            cd "${dir}"
            if [ -d .git ]; then
                git_status=$(git status --porcelain)
                if [ -n "$git_status" ]; then
                    kill_line
                    pwd
                    printf -- "%s\n\n" "${git_status}"
                fi
            else
                gf-recursion-helper
            fi
            cd ..
        done
        kill_line
    }
    #
    ( cd /gf && gf-recursion-helper )
}
