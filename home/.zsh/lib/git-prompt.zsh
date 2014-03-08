# zsh git prompt support
#
# Copyright (C) 2006,2007 Shawn O. Pearce <spearce@spearce.org>
# Copyright (C) 2014 Brett Campbell <invsblduck@gmail.com>
# Distributed under the GNU General Public License, version 2.0.
#
# This script allows you to see repository status in your prompt.
#
# To enable:
#
#    1) Copy this file to somewhere (e.g. ~/.git-prompt.zsh).
#    2) Add the following line to your .zshrc:
#           source ~/.git-prompt.zsh
#    3) Change your PS1 or RPROMPT to call __git_ps1 as command-substitution:
#           setopt prompt_subst
#           RPROMPT='$(__git_ps1 "=[%s]=")'
#        the optional argument will be used as format string.
#
# The repository status will be displayed only if you are currently in a
# git repository. The %s token is the placeholder for the repo status.
#
# The prompt status always includes the current branch name.
#
# In addition, if you set GIT_PS1_SHOWDIRTYSTATE to a nonempty value,
# unstaged (*) and staged (+) changes will be shown next to the branch
# name.
#
# You can also see if currently something is stashed, by setting
# GIT_PS1_SHOWSTASHSTATE to a nonempty value. If something is stashed,
# then a '$' will be shown next to the branch name.
#
# If you would like to see if there're untracked files, then you can set
# GIT_PS1_SHOWUNTRACKEDFILES to a nonempty value. If there're untracked
# files, then a '%' will be shown next to the branch name.
#
# By default, __git_ps1 will look for a remote named 'upstream' and compare
# HEAD to it (if 'upstream' exists), otherwise, literal '@{upstream}' is used,
# which is a dynamic value to git.
#
# If you would like to see a numerical difference in number of commits between
# HEAD and its upstream, set GIT_PS1_SHOWUPSTREAM to a nonempty value.
# Otherwise, a "<" indicates you are behind, ">" indicates you are ahead,
# "<>" indicates you have diverged and "=" indicates that there is no
# difference.
#
# If you would like to see more information about the identity of
# commits checked out as a detached HEAD, set GIT_PS1_DESCRIBE_STYLE
# to one of these values:
#
#     contains      relative to newer annotated tag (v1.6.3.2~35)
#     branch        relative to newer tag or branch (master~4)
#     describe      relative to older annotated tag (v1.6.3.1-13-gdd42c2f)
#     default       exactly matching tag
#
# If you would like a colored hint about the current dirty state, set
# GIT_PS1_SHOWCOLORHINTS to a nonempty value. The colors are based on
# the colored output of "git status -sb".


# used by GIT_PS1_SHOWUPSTREAM
# stores the divergence from upstream in $p
__git_ps1_show_upstream ()
{
    local key value count verbose
    [[ -n "${GIT_PS1_SHOWUPSTREAM}" ]] && verbose=1

    # Find our upstream
    if git config -z --get-regexp '^remote\.upstream\.' &>/dev/null; then
        upstream="upstream/${b##refs/heads/}"
    else
        upstream='@{upstream}'
    fi

    # Find how many commits we are ahead/behind our upstream
    count="$(git rev-list --count --left-right "$upstream"...HEAD 2>/dev/null)"

    # calculate the result
    # NOTE: tabs in the data!
    case "$count" in
        '')         # no upstream
            p=''
            ;;

        0$'\t'0)   # equal to upstream

            [[ -n $verbose ]] && p=' u=' || p='='
            ;;

        0$'\t'*)   # ahead of upstream

            [[ -n $verbose ]] && p=" u+${count#0$'\t'}" || p='>'
            ;;

        *$'\t'0)   # behind upstream

            [[ -n "$verbose" ]] && p=" u-${count%$'\t'0}" || p='<'
            ;;

        *)          # diverged from upstream
            [[ -n $verbose ]] && \
                p=" u+${count#*$'\t'}-${count%$'\t'*}" || p='<>'
            ;;
    esac

}

# Helper function that is meant to be called from __git_ps1.  It
# injects color codes into the appropriate gitstring variables used
# to build a gitstring.
__git_ps1_colorize_gitstring ()
{
    local c_red='%F{red}'
    local c_green='%F{green}'
    local c_lblue='%F{blue}'
    local c_clear='%f'

    local ok_color=$c_green
    local bad_color=$c_red
    local flags_color="$c_lblue"
    local branch_color=""

    if [ $detached = no ]; then
        branch_color="$ok_color"
    else
        branch_color="$bad_color"
    fi

    c="${branch_color}${c}"
    z="${c_clear}${z}"

    [ "$w" = '*' ] && w="${bad_color}${w}"
    [ -n "$i" ] && i="${ok_color}${i}"
    [ -n "$s" ] && s="${flags_color}${s}"
    [ -n "$u" ] && u="${bad_color}${u}"

    r="${c_clear}${r}"
}

# __git_ps1 accepts 0 or 1 arguments (i.e., format string)
# call this function from PROMPT or RPROMPT using command substitution.
__git_ps1 ()
{
    local detached=no
    local printf_format=' (%s)'
    local repo_info rev_parse_exit_code

    printf_format="${1:-$printf_format}"
    repo_info="$(git rev-parse --git-dir --is-inside-git-dir \
        --is-bare-repository --is-inside-work-tree \
        --short HEAD 2>/dev/null)"

    rev_parse_exit_code="$?"

    if [ -z "$repo_info" ]; then
        return
    fi

    local short_sha
    if [ "$rev_parse_exit_code" = "0" ]; then
        short_sha="${repo_info##*$'\n'}"
        repo_info="${repo_info%$'\n'*}"
    fi

    local inside_worktree="${repo_info##*$'\n'}"
    repo_info="${repo_info%$'\n'*}"

    local bare_repo="${repo_info##*$'\n'}"
    repo_info="${repo_info%$'\n'*}"

    local inside_gitdir="${repo_info##*$'\n'}"
    local g="${repo_info%$'\n'*}"

    local r=""
    local b=""
    local step=""
    local total=""
    if [ -d "$g/rebase-merge" ]; then
        read b 2>/dev/null <"$g/rebase-merge/head-name"
        read step 2>/dev/null <"$g/rebase-merge/msgnum"
        read total 2>/dev/null <"$g/rebase-merge/end"
        if [ -f "$g/rebase-merge/interactive" ]; then
            r="|REBASE-i"
        else
            r="|REBASE-m"
        fi
    else
        if [ -d "$g/rebase-apply" ]; then
            read step 2>/dev/null <"$g/rebase-apply/next"
            read total 2>/dev/null <"$g/rebase-apply/last"
            if [ -f "$g/rebase-apply/rebasing" ]; then
                read b 2>/dev/null <"$g/rebase-apply/head-name"
                r="|REBASE"
            elif [ -f "$g/rebase-apply/applying" ]; then
                r="|AM"
            else
                r="|AM/REBASE"
            fi
        elif [ -f "$g/MERGE_HEAD" ]; then
            r="|MERGING"
        elif [ -f "$g/CHERRY_PICK_HEAD" ]; then
            r="|CHERRY-PICKING"
        elif [ -f "$g/REVERT_HEAD" ]; then
            r="|REVERTING"
        elif [ -f "$g/BISECT_LOG" ]; then
            r="|BISECTING"
        fi

        if [ -n "$b" ]; then
            :
        elif [ -h "$g/HEAD" ]; then
            # symlink symbolic ref
            b="$(git symbolic-ref HEAD 2>/dev/null)"
        else
            local head=""
            if ! read head 2>/dev/null <"$g/HEAD"; then
                return
            fi
            # is it a symbolic ref?
            b="${head#ref: }"
            if [ "$head" = "$b" ]; then
                detached=yes
                b="$(
                case "${GIT_PS1_DESCRIBE_STYLE-}" in
                (contains)
                    git describe --contains HEAD ;;
                (branch)
                    git describe --contains --all HEAD ;;
                (describe)
                    git describe HEAD ;;
                (* | default)
                    git describe --tags --exact-match HEAD ;;
                esac 2>/dev/null)" ||

                b="$short_sha..."
                b="($b)"
            fi
        fi
    fi

    if [ -n "$step" ] && [ -n "$total" ]; then
        r="$r $step/$total"
    fi

    local w=""
    local i=""
    local s=""
    local u=""
    local c=""
    local p=""

    if [ "true" = "$inside_gitdir" ]; then
        if [ "true" = "$bare_repo" ]; then
            c="BARE:"
        else
            b="GIT_DIR!"
        fi
    elif [ "true" = "$inside_worktree" ]; then
        if [ -n "${GIT_PS1_SHOWDIRTYSTATE-}" ]; then
            git diff --no-ext-diff --quiet --exit-code || w='*'
            if [ -n "$short_sha" ]; then
                git diff-index --cached --quiet HEAD -- || i='+'
            else
                i='#'
            fi
        fi

        if [ -n "${GIT_PS1_SHOWSTASHSTATE-}" ] &&
           [ -r "$g/refs/stash" ]; then
            s='$'
        fi

        if [ -n "${GIT_PS1_SHOWUNTRACKEDFILES-}" ] &&
           git ls-files --others --exclude-standard --error-unmatch -- '*' \
            >/dev/null 2>/dev/null
        then
            u="%${ZSH_VERSION+%}"
        fi

        if [ -n "${GIT_PS1_SHOWUPSTREAM-}" ]; then
            __git_ps1_show_upstream
        fi
    fi

    local z="${GIT_PS1_STATESEPARATOR-" "}"

    if [ -n "${GIT_PS1_SHOWCOLORHINTS-}" ]; then
        __git_ps1_colorize_gitstring
    fi

    local f="${w}${i}${s}${u}"
    local gitstring="${c}${b##refs/heads/}${f:+${z}${f}}${r}${p}"

    printf -- "$printf_format" "$gitstring"
}
