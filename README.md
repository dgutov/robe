# Zossima

Jump to definition in Emacs, driven by a live Ruby subprocess.

This builds on `inf-ruby` to handle Ruby subprocesses from Emacs. Once
you've loaded your inf-ruby process with the code for your project and
its dependencies Ruby keeps track of where each method is defined, so
you can use <kbd>M-.</kbd> to jump to the definition of a given method
and <kbd>M-,</kbd> to jump back.

It prompts you with a list of all known classes, and once you've
chosen one, narrows down to a list of methods.

## Install

Currently you should just check it out and run <kbd>M-x
package-install-file</kbd>. Once it's installed:

```lisp
(add-hook 'ruby-mode-hook 'zossima-mode)
```

## Todo

* Support for modules
* Package on Marmalade
* Support for multiple inf-rubies in one Emacs instance
* Using the class/method at point if applicable?
* Possibly use the same class/method selector for docs?

## Copying

Copyright © 2012 Phil Hagelberg

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
