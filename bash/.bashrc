function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

export PATH="$HOME/.local/bin:$PATH"

alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "

pdfconstcrop() {
    pdfcrop --bbox "$(
        pdfcrop --verbose "$@" |
        grep '^%%HiResBoundingBox: ' |
        cut -d' ' -f2- |
        datamash -t' ' min 1 min 2 max 3 max 4
    )" "$@"
}

pdfcrop_all() {
    for FILE in *.pdf; do
        pdfconstcrop --margins '20 20 20 20' "${FILE}"
    done
}

eval "$(direnv hook bash)"
eval "$(starship init bash)"
