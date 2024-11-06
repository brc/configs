__k8s_ps1_colorize_context() {
    local context="$1"
    local context_color="%{${fg[cyan]}%}"

    [[ $context =~ prod ]] && context_color="%{${fg_bold[red]}%}"
    printf -- "%s" "[${context_color}${context}%{${reset_color}%}]"
}

__k8s_ps1_current_context() {
    local context
    local kubeconfig="${KUBECONFIG:-${HOME}/.kube/config}"
    if [ -f "${kubeconfig}" ]; then
        context=$(/bin/grep '^current-context:' "${kubeconfig}" \
                    |/bin/awk '{print $2}')
        printf -- "%s" "$(__k8s_ps1_colorize_context ${context})"
    fi
}
