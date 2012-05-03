ArchLinux Package Management library wrapper and its frontend
=============================================================

This is Haskell wrapper for [libalpm](http://www.archlinux.org/pacman/), a library for package management for Linux systems.

It provides low-level bindings to C functions, as well as high-level monadic interface.

High-level interface has its frontend program called `halp`. This is `pacman` wrapper written in Haskell, supporting (supposedly) also AUR and Hackage automatic package management.

Notes
-----

The library is in early state of development. Currently not very much than simple C bindings is done. 

These bindings are mostly complete; there is no wrapping of most `alpm_list` functions since this list is transformed to Haskell list; there is also no bindings for log callback yet, because I know no sane way of wrapping vararg functions. Any help in this regard is appreciated.
