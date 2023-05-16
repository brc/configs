#
# ~/.bash_profile
#

for f in ~/.openrc ~/.bashrc; do
    [[ -f $f ]] && . $f
done
