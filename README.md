# Zossima

Jump to definition in Emacs, driven by a live Ruby subprocess.

This builds on `inf-ruby` to handle Ruby subprocesses from Emacs. Once
you've loaded your inf-ruby process with the code for your project and
its dependencies Ruby keeps track of where each method is defined, so
you can use <kbd>M-.</kbd> to jump to the definition of a given method
and <kbd>M-,</kbd> to jump back.

It looks for the method at point in all known classes and modules. If there's
none at point or the owner module is ambiguous, asks you to narrow it down.

## Install

Currently you should just check it out and run <kbd>M-x
package-install-file</kbd>. Once it's installed:

```lisp
(add-hook 'ruby-mode-hook 'zossima-mode)
```

## Todo

* Package on Marmalade
* Support for multiple inf-rubies in one Emacs instance
* Possibly use the same class/method selector for docs?
* Eval call target name in a safer way?
* Do not jump to private methods when the call has explicit receiver
* Jump to a class with no methods?
* Handle `delegate` and `send`, `Class.new.method` and `self.class.method`
* For methods defined through macros, optionally jump to where the macro was
  called, instead of its definition?

## Copying

Copyright © 2012 Phil Hagelberg

Copyright © 2012 Dmitry Gutov

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
