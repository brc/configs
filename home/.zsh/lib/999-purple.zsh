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

    source /opt/homebrew/share/zsh/site-functions/_google_cloud_sdk
    source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
    source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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

    ##########################################################################
    # /gf
    ##########################################################################
    RSYNC_GIT_FILTER_ARGV=(
        --no-checksum       # don't checksum the files (too slow)
        --open-noatime      # don't update the atimes (maybe faster)
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/gf
        --exclude .claude/
        --exclude providers/registry.terraform.io/hashicorp/local/
        --exclude 'backend/charts/vault/tmp/kubectl.*.out'
    )

    get-git() {
        rsync-safe \
            get \
            /gf/ \
            "${RSYNC_GIT_FILTER_ARGV[@]}" \
            "$@"
    }

    backport-git() {
        rsync-safe \
            backport \
            /gf/ \
            "${RSYNC_GIT_FILTER_ARGV[@]}" \
            "$@"
    }

    ##########################################################################
    # /gi/configs
    ##########################################################################
    RSYNC_DOTFILE_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/git/brc/configs
        # INCLUDES ---------------------------------------
        --include home/spacemacs/.cache/

        # XXX This doesn't work correctly across boxes with Projectile because
        #     Projectile dereferences symlinks and the paths end up being
        #     different (so the Treemacs path won't match the project).
        #--include home/spacemacs/.cache/treemacs-persist 

        --include home/spacemacs/.cache/transient/
        # EXCLUDES ---------------------------------------
        --exclude 'home/spacemacs/.cache/*'
        --exclude .claude/
        --exclude .git/index
        --exclude home/.vim/.undo/
        --exclude home/.zsh/history
        --exclude 'home/.zsh/.zcompdump*'
        --exclude home/spacemacs/recentf
        --exclude home/spacemacs/tree-sitter/
        --exclude home/spacemacs/elpa/
        --exclude home/spacemacs/quelpa/
        --exclude home/spacemacs/eln-cache
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

    ##########################################################################
    # /data
    ##########################################################################
    RSYNC_DATA_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /df/rsync-saved/data
        --exclude .claude/
        --exclude rsync-saved/
        --exclude cluster.old/
    )

    RSYNC_DATA_DIRS=(
        /d/istio/
        /d/k8s/
        /df/
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

    ##########################################################################
    # sh-lib
    ##########################################################################
    RSYNC_SHLIB_FILTER_ARGV=(
        --delete            # delete stray files
        --backup            # save old files (too paranoid)
        --backup-dir /d/rsync-saved/sh-lib
        --exclude .claude/
        --exclude .git/index
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
fi
