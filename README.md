# Robe [![Build Status](https://travis-ci.org/dgutov/robe.png?branch=master)](https://travis-ci.org/dgutov/robe)

Robe is a code assistance tool that uses a Ruby REPL subprocess with
your application or gem code loaded, to provide information about
loaded classes and modules, and where each method is defined.

Generally, you'll want to start with `M-x inf-ruby-console-auto`.
If there's no Ruby console running, most interactive commands provided
by Robe will offer to launch it automatically.

As you change the code in your project, you'll want to update the
running process. To load the current file, type <kbd>C-c C-l</kbd>
(`ruby-load-file`), see [inf-ruby](https://github.com/nonsequitur/inf-ruby/)
for more commands. When you're developing a Rails project, you can
type <kbd>C-c C-k</kbd> instead, to reload the whole environment at once.

## Features

* Jump to method definition
* Jump to `super` or a constructor called at point
* Jump to a module or class (provided it has at least one method defined)
* Display method documentation
* Display information about method called at point using ElDoc
* Method and constant name completion

To see the available commands, type <kbd>M-x describe-package RET robe RET</kbd>.

## Details

When peforming one of the commands defined here, we either need to
narrow the method name at point down to a specific method in a specific
module, or enumerate the possible method names or constants allowed at
point (for code completion).

To do that, we look at the contents of the buffer, and the context at
point: in which method it is, of which class, and if it's in singleton
class context. Then we look at the method call at point.

If the method call target is implicit (there's no target or the method
is `super`), or the call target is obvious (`Foo.new`, `self.foo`),
then we first try to look for the definition in the inheritance
hierarchy of the target class. Otherwise, or if the initial search
yields no result, scan all defined classes and modules.

Depending on the command, if the result is ambiguous, you're either
prompted to resolve the ambiguity manually, or the results are merged
together.

## Install

Set up [MELPA](http://melpa.milkbox.net/#installing) if you haven't already,
then type <kbd>M-x package-install RET robe RET</kbd>.

In the init file:

```lisp
(add-hook 'ruby-mode-hook 'robe-mode)
```

## Dependencies

* `pry`
* `pry-doc` (on MRI)
* `method_source >= 0.8.2` (for compatibility with the latest Rubinius)

Note that if your project is using `Bundler`, the dependencies have to be added to the `Gemfile`.

## Completion

### [company-mode](http://company-mode.github.com/) ([screenshot](robe-company.png)):

```lisp
(push 'company-robe company-backends)
```

### [auto-complete](http://auto-complete.org/):

```lisp
(add-hook 'robe-mode-hook 'robe-ac-setup)
```

Both of the above work only when the connection to the Ruby subprocess has
been established. To do that, either use one of the core Robe commands, or
type <kbd>M-x robe-start</kbd>.

Built-in completion (triggered with <kbd>C-M-i</kbd>) is also supported,
no extra setup required.

## Integration with rvm.el

[rvm.el](https://github.com/senny/rvm.el) may not have activated the
correct project ruby before `robe-start` runs. Either manually run
<kbd>M-x rvm-activate-corresponding-ruby</kbd> before starting Robe,
or advise `robe-start` to activate rvm automatically.

```lisp
(defadvice robe-start (before activate-rvm-for-robe)
  (rvm-activate-corresponding-ruby))
```

## Compatibility

* Tested in Emacs 24.3+, with Ruby 1.9.3 and 2.0.0, on GNU/Linux.
* Essential features work with JRuby and the latest Rubinius.
  JRuby has longer startup, Rubinius is noticeably slower at runtime
  ([rubinius/rubinius#2390](https://github.com/rubinius/rubinius/issues/2390)).
* Mostly works on MS Windows, with minor glitches.
* Using Emacs trunk is currently recommended (24.3.50) for better recognition
  of context at point.

## Notes

* We can't jump to methods defined in C (such as most of the core classes).
  To read their docs, install `pry-doc` or add it to your Gemfile.
* We can't jump to lazily defined methods, such as `model.column` or `find_by_`
  `ActiveRecord` methods, before they've been called. This is treatable, but low
  priority.
* Jumping to methods defined with `Module#delegate` just brings us to the place
  where `delegate` is called, which is accurate, but often less than useful.
* Having more than one `inf-ruby` buffer at a time is not supported. If you see
  unexpected "Method not found" errors, check if you have an older one.
* We may get the context wrong for code inside a block if the method
  it's passed to uses `instance_eval` or `instance_exec`.

## TODO

* Handle `delegate` and `send`, `Class.new.method` and `self.class.method`.
* For methods defined through macros, optionally jump to where the macro was
  called, instead of its definition?
* Apropos search for classes and methods.
* Type inference and local variable completion.

## Copying

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

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/de7c96160d19b6945b432196a97eaaf3 "githalytics.com")](http://githalytics.com/dgutov/robe)
