# Zossima

Jump to definition in Emacs, driven by a live Ruby subprocess.

This builds on `inf-ruby` to handle Ruby subprocesses from Emacs.

Once you've loaded your inf-ruby process with the code for your project and its
dependencies Ruby keeps track of where each method is defined.

You can jump to or read the documentation for the method, module (jump only),
`super` or constructor definition at point.

If the method call target is implicit (like with `super`), or the call target is
obvious (`Foo.new`, `2.seconds`), then we first try to look for the definition
in superclasses, descendants and included modules as appropriate.

If the result is ambiguous, you're prompted to pick the module/location.

## Install

Currently you should check it out from Git:

`git clone https://github.com/dgutov/zossima.git ~/zossima`

Once it's installed:

```lisp
(add-to-list 'load-path "~/zossima")
(require 'zossima)
(add-hook 'ruby-mode-hook 'zossima-mode)
```

## Notes

* We can't jump to the methods defined in C (such as most of the core classes).
  To read their docs, install `pry-doc` or add it to your Gemfile.
* We can't jump to lazily defined methods, such as `model.column` or `find_by_`
  `ActiveRecord` methods, before they've been called. This is treatable, but low
  priority.
* Jumping to methods defined with `Module#delegate` just brings us to the place
  where `delegate` is called, which is accurate, but often less than useful.

## Todo

* Package on Marmalade
* Support for multiple inf-rubies in one Emacs instance
* Eval call target name in a safer way?
* Do not jump to private methods when the call has explicit receiver
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
