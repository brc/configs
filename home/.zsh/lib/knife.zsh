function ke {
    [ -n "$1" ] && knife environment show $* && return
    knife environment list
}

function kec {
    if [ -z "$1" -o -z "$2" ]; then
        echo "usage: kec <env1> <env2>"
        return 1
    fi

    knife exec -E \
        "nodes.find('chef_environment:${1}') do |n|
          puts 'updating ' + n.name
          n.chef_environment = '${2}'
          n.save
        end"

}
