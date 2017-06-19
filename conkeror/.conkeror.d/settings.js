homepage = "http://en.wikipedia.org/wiki/Special:Random";

define_key(default_base_keymap, "M-a", "execute-extended-command");
define_key(default_base_keymap, "G", "find-url-new-buffer");

view_source_use_external_editor = true;
editor_shell_command = "urxvt -e emacsclient -nw";

cwd = get_home_directory();
cwd.append("Downloads");

add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

session_pref("xpinstall.whitelist.required", false);
