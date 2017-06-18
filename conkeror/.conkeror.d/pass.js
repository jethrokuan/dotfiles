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
