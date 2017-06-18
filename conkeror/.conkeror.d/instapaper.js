// Rendering in instapaper
interactive("render_instapaper",
            "Render page with InstaPaper's Text view.",
            function (I) {
              var d = I.window.buffers.current.document;
              if(!d.body)
                throw('Please wait until the page has loaded.');
              browser_object_follow(
                I.window.buffers.current,
                OPEN_CURRENT_BUFFER,
                'http://www.instapaper.com/text?u='+encodeURIComponent(d.location.href));
              I.window.minibuffer.message("Rendering with InstaPaper ...");
            });

define_key(content_buffer_normal_keymap, "a", "render_instapaper");
