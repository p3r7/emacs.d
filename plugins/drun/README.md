# drun

A FreeDesktop XDG Desktop Entry menu and launcher.



## Dependencies

To launch a desktop entry, `drun` uses [dex](https://github.com/jceb/dex) by default.

Alternatives (such as `gtk-launch`, `exo-open`, `kioclient`) could be used instead by customizing `drun-executable` and `drun-executable-opts`.

For example for `kioclient`:

```el
(setq drun-executable "kioclient"
      drun-executable-opts '(exec))
```

## Documentation Links

- Official FreeDesktop Specifications: [desktop entries](https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html), [menu](https://specifications.freedesktop.org/menu-spec/latest/index.html)
- [ArchWiki page](https://wiki.archlinux.org/index.php/Desktop_entries)


## Similar projects

- [rofi](https://github.com/davatorium/rofi)'s `drun` mode
- relying on dmenu: [xdg-dmenu](https://github.com/fallc0nn/xdg-dmenu) (in bash), [dmenu-xdg](https://github.com/lharding/dmenu-xdg) (in python)


https://emacs.stackexchange.com/questions/45440/tramp-and-x11-forwarding-when-changing-user-su
https://lists.gnu.org/archive/html/help-gnu-emacs/2015-07/msg00363.html

https://askubuntu.com/questions/5172/running-a-desktop-file-in-the-terminal


Works inside remote shell-mode.

    dex /home/vagrant/.local/share/applications/ghidra.desktop
    dex /usr/share/applications/vlc.desktop

Doesn't work w/ `start-file-process` if dd is remote:

```el
(let ((default-directory "/ssh:vagrant@192.168.254.66:/vagrant/"))
  ;; (drun--start-file-process "hello" (generate-new-buffer "*hello*") "ls" "-l")
  ;; (drun--start-file-process "hello" (generate-new-buffer "*hello*") "/bin/dex" "/home/vagrant/.local/share/applications/ghidra.desktop")
  ;; (drun--start-file-process "hello" (generate-new-buffer "*hello*") "/bin/dex" "/usr/share/applications/vlc.desktop")
  (drun--start-file-process "hello" (generate-new-buffer "*hello*") "env")
  )
```

In that case, `start-file-process` ends up calling `tramp-handle-start-file-process`.

Patching it w/ a :stderr doesn't do much:

```el
(defun tramp-handle-start-file-process (name buffer program &rest args)
  (message "------------")
  (message name)
  (message program)

  (tramp-file-name-handler
   'make-process
   :name name
   :buffer buffer
   :stderr (generate-new-buffer "*hello2*") ; NB: fails if we reuse `buffer'
   :command (and program (cons program args))
   :noquery nil
   :file-handler t))
```

This one may work, but won't prompt for pwd:

```el
(make-process :name "hello"
              ;; NB: :stderr is mandatory
              :stderr (generate-new-buffer "*hello*")
              :command '("ssh" "-X" "vagrant@192.168.254.66" "\"dex /usr/share/applications/vlc.desktop\""))
```

This one works:

```el
(friendly-shell-command "dex /home/vagrant/.local/share/applications/ghidra.desktop"
                        :path "/ssh:vagrant@192.168.254.66:/vagrant/")
```
