# Robe

This builds on `inf-ruby` to handle Ruby subprocesses from Emacs.

Once you've loaded your inf-ruby process with the code for your project and its
dependencies, Ruby keeps track of where each method is defined.

You can jump to or read the documentation for the method, module (jump only),
`super` or constructor definition at point.

If the method call target is implicit (like with `super`), or the call target is
obvious (`Foo.new`, `self.foo`), then we first try to look for the definition in
superclasses, descendants and included modules as appropriate.

If the result is ambiguous, you're prompted to pick the module/location.

ElDoc support and constant and method completion (triggered with
<kbd>C-M-i</kbd>) are also provided.

Users of `auto-complete` can enable Robe completion with:

```lisp
(push 'ac-source-robe ac-sources)
```

## Install

Set up [Melpa](http://melpa.milkbox.net/#installing) if you haven't already,
then do `M-x package-install RET robe RET`.

In the init file:

```lisp
(add-hook 'ruby-mode-hook 'robe-mode)
```

## Compatibility

* Tested in Emacs 24.3 (unreleased), with Ruby 1.9.3, on GNU/Linux.
* Mostly works on MS Windows, with minor glitches.

## Notes

* We can't jump to methods defined in C (such as most of the core classes).
  To read their docs, install `pry-doc` or add it to your Gemfile.
* We can't jump to lazily defined methods, such as `model.column` or `find_by_`
  `ActiveRecord` methods, before they've been called. This is treatable, but low
  priority.
* Jumping to methods defined with `Module#delegate` just brings us to the place
  where `delegate` is called, which is accurate, but often less than useful.
* Having more than one `inf-ruby` buffer at the same time is not supported. If
  you see unexpected "Method not found" errors, check if you have an older one.

## Todo

* Handle `delegate` and `send`, `Class.new.method` and `self.class.method`.
* For methods defined through macros, optionally jump to where the macro was
  called, instead of its definition?
* Apropos search for classes and methods.
* Show information from tags and method sources: info_for(...).[tags|source].
* Type inference and local variable completion.

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
