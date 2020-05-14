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
