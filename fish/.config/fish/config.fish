for file in ~/.config/fish/conf.d/*.fish
    source $file
end

# Start X at login
if status --is-login
    if test -z "$DISPLAY" -a (fgconsole) = 1
        exec xinit -- -keeptty
    end
end
