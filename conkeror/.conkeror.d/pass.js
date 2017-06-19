interactive("run-pass-clipboard-on-current-domain",
            "Runs 'pass -c' on the current domain.",
            function (I) {
              var domain = I.buffer.document.domain;

              // filter possible 'www.'
              domain = domain.replace("www.", "");

              var result = yield shell_command_with_argument("pass -c", domain);

              if (result == 0) {
                I.buffer.window.minibuffer.message("Retrieved password for " +
                                                   domain + ".");
              } else {
                I.buffer.window.minibuffer.message("Failed to retrieve password for " + domain + ".");
              }
            });

define_key(default_global_keymap, "C-c C-p", 
           "run-pass-clipboard-on-current-domain");

interactive("pass",
            "Select password from pass",
            function (I) {
              var data = "", error = "";
              var result = yield shell_command(
                'bash -c \'function join_by { local IFS="$1"; shift; echo "$*"; } && prefix=${PASSWORD_STORE_DIR-~/.password-store} && password_files=( "$prefix"/**/*.gpg ) && password_files=( "${password_files[@]#"$prefix"/}" ) && password_files=( "${password_files[@]%.gpg}" ) && join_by "|" "${password_files[@]}"\'',
                $fds = [{ output: async_binary_string_writer("") },
                        { input: async_binary_reader(function (s) data+=s||"") },
                        { input: async_binary_reader(function (s) error+=s||"") }]);
              if (result != 0 || error != "")
                throw new interactive_error("status "+result+", "+error);
              var passwords = data.split("|");
              var passkey = yield I.minibuffer.read($prompt = "Select pass:",
                                                    $completer = new prefix_completer(
                                                      $completions = passwords.sort()
                                                    ),
                                                    $auto_complete = true,
                                                    $auto_complete_initial = true,
                                                    $auto_complete_delay = 0,
                                                    $require_match = true
                                                   );
              data = "";
              error = "";
              result = yield shell_command('bash -c \'pass -c ' + passkey + '\'',
                                           $fds = [{ output: async_binary_string_writer("") },
                                                   { input: async_binary_reader(function (s) data+=s||"") },
                                                   { input: async_binary_reader(function (s) error+=s||"") }]);
              if (result != 0 || error != "") {
                I.window.minibuffer.message("status "+result+", "+error);
              } else {
                I.window.minibuffer.message(data);
              }
            });
