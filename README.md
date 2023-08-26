# Proper generalizations from the oldtimers - UNIX, Smalltalk, Genera, Plan9

The most fundamental principles are still of *pipelining* and of *transforming* a strucrured text.

It has been generalized from daily practices of running mundane tasks on a *terminal*, where `ls`, `find` and `grep` were the main utilities in the 80s and 90s.

Notice that in the old time there were no way to easily copy and paste struff around, so they used temporary filles, which later became an editor buffers. This is actually a *big deal*.


# The motivating example.

Consider the following Emacs &ldquo;command&rdquo;:

> Signature (find-grep COMMAND-ARGS)
> 
> Documentation Run grep via find, with user-specified args COMMAND-ARGS.
> 
> Collect output in the &ldquo;**grep**&rdquo; buffer.

It uses the UNIX command `find` and then pipes its output to `grep` and then collects *its parsed* output into the buffer named `*grep*`. No *temp files*, no need to copy-paste from the terminal window, each line is a &ldquo;clickable&rdquo; reference to particular position in a file at which the *match* occur.

These two lines change the implementation of this command without affecting any interfaces and calling conventions.

```elisp
(when (executable-find "fd")
  (setq find-program "fd"))
```

There is a *classic example*

```emacs-lisp
(when (executable-find "rg")
  (setq grep-program "rg")
  (grep-apply-setting
   'grep-find-command
     '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
```

The first two lines change the implementation of the `grep-find` &ldquo;command&rdquo; (which is an &ldquo;interactive&rdquo; Lisp procedure (assuming it understand the *traditional* command line arguments).

The following lines specify an *alternative implementation* for the &ldquo;standard&rdquo; `grep-find` command. Again, no visible changes *at the use side*.

The cool part is that this command is used in `eshell` by *shadowing* (which is a classic PLT concept) the UNIX `grep` command and running the Lisp procedure instead, which collets and parses the output into the `*grep*` buffer.

There are lots of nice subtle hacks going on together &#x2013; wrapping of executables, pipelining of the output, clever shadowing of a standard symbol (name), and redirecting, parsing and indexing (making &ldquo;clickable&rdquo;) of the output.

This is the *golden standard*, and there are several fundamental principles *manifest itselves*, from UNIX, Smalltalk and Plan9.

Just open `M-x eshell` window and type something like

```eshell
grep "rg$" .emacs.d/init.el
```

and see what would happen.


# Another a-ha moment

Right now we are going through the &ldquo;LSP revolution&rdquo; &#x2013; most of the old-school *ad-hoc* tools are getting replased and even somehow &ldquo;standardized&rdquo; at the level of programmer&rsquo;s interfaces (client APIs).

It is a &ldquo;classic&rdquo; *client-server* architecture with a standardized (but unnecessary complex) protocol in between &#x2013; the universal (as Internet itself) way to do the right things.

The point is that the server does all the &ldquo;heavy lifting&rdquo; of parsing, indexing (cross-referencing) and maintaining a database of symbols (identificators) which used to be done at the client size with various *ad-hoc* solutions and tools.

It would be not a big deal, if not a single clever hack &#x2013; good (well-designed) language servers use the compiler&rsquo;s &ldquo;infrastructure&rdquo; which is provided as a set of &ldquo;hight level&rdquo; libraries &#x2013; the compiler&rsquo;s code is being *reused* for everything (via some API).

`clangd` does this, and following it many others do.

```
 $ ldd `which clangd`
	linux-vdso.so.1 (0x00007ffff7b31000)
	libclang-cpp.so.16+libcxx => /usr/lib/llvm/16/bin/../lib64/libclang-cpp.so.16+libcxx (0x00007fb9b8b07000)
	libLLVM-16+libcxx.so => /usr/lib/llvm/16/bin/../lib64/libLLVM-16+libcxx.so (0x00007fb9b1280000)
	libc++.so.1 => /usr/lib64/libc++.so.1 (0x00007fb9b1155000)
	libc++abi.so.1 => /usr/lib64/libc++abi.so.1 (0x00007fb9b1112000)
	libm.so.6 => /lib64/libm.so.6 (0x00007fb9b1038000)
	libunwind.so.8 => /usr/lib64/libunwind.so.8 (0x00007fb9b101b000)
	libc.so.6 => /lib64/libc.so.6 (0x00007fb9b0e41000)
	/lib64/ld-linux-x86-64.so.2 (0x00007fb9bf025000)
	libffi.so.8 => /usr/lib64/libffi.so.8 (0x00007fb9b0e34000)
	libz3.so.4.12 => /usr/lib64/libz3.so.4.12 (0x00007fb9af729000)
	libz.so.1 => /lib64/libz.so.1 (0x00007fb9af70a000)
	libzstd.so.1 => /lib64/libzstd.so.1 (0x00007fb9af61a000)
	libtinfo.so.6 => /lib64/libtinfo.so.6 (0x00007fb9af5d4000)
	libgcc_s.so.1 => /usr/lib/gcc/x86_64-pc-linux-gnu/13/libgcc_s.so.1 (0x00007fb9af5ae000)
	liblzma.so.5 => /lib64/liblzma.so.5 (0x00007fb9af578000)
	libgmp.so.10 => /usr/lib64/libgmp.so.10 (0x00007fb9af4c9000)
```

Notice this `libclang-cpp.so.16` &#x2013; this is the interface to the compiler.

So, all you need to do nowadays is to set up the `lsp-mode`, which is a LSP *client*, and the `lsp-ui` package, which is, well, a *user interface*.

```elisp
(yas-global-mode)
(which-key-mode)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
```

and *wallah*, just like that&#x2026;

`lsp-mode` does not re-implement the wheel, and *re-uses* `eldoc, xref` and `company-mode`, and &ldquo;standard&rdquo; `flycheck` &ldquo;microframework&rdquo;. There is also support for `ivy`.

Make sure you get it &#x2013; everything good and &ldquo;standard&rdquo; has been re-used, and the most tricky and difficult parts were &ldquo;offloaded&rdquo; (delegated) via *server* to the compiler.

This is how to do proper, principle-guided software engineering.

Same &ldquo;revolution&rdquo; is going on in the *neovim* world.


# more blah-blah

dired

the output of `ls` is going to be stored in (redirected to) a buffer, so it can be *re-used* without copy-pasting from a terminal.

This output, properly parsed, becomes the &ldquo;source&rdquo; for *selection* and for further actions.

Pipelining the output of commands into ediror buffers is essentially a Plan9 workflow (with the `acme`) editor.

grep

the output of the `grep` command will be stored in a buffer.

Hits from ‘M-x grep’ can be iterated through using \`C-x \`’. This runs Emacs command ‘next-error’.

`M-x find-name-dired` - search for filenames

`M-x find-grep-dired` - search for *contents*

`M-x grep-find` `M-x find-grep` `M-x rgrep` Run grep via find, and collect output in the **grep** buffer.

eshell

The cool trick is to use replacements for commands like `grep` and `find` with the correrspondig Emacs internal *commands* (interactive lisp functions) so that all the output will be properly parsed and stored in corresponding buffers.

This is the rationale for using `eshell`. It will use `grep`, which in turn, will call `rg` &#x2013; several layers of redirection.

workflows

one &ldquo;greps&rdquo; inside a &ldquo;project&rdquo; (starting from its &ldquo;root&rdquo;) with `rg-project` or `ag-progect`

even better (one more wrapping and an indirection) `counsel-rg` which will ask for a &ldquo;root directory&rdquo; (from where to start)

`rg-project` or `ag-project` will skip this question.

Emacs is a *very* large and complex system, so all the good (and bad) programming methodologies and principles can be seen within.

The classic examples are `plist` and `alist` ADTs, which are not *&ldquo;abstract&rdquo;* at all, and everyone has to check and maintain the required representation *invariants* explicitly.

A `plist` has to have an *even number of elements*, no duplicate &ldquo;keys&rdquo; and better to be *consistent* about the *types* of the elements. One has to use *getters and setters* to maintain these invariants. There is also a *predicate* `plistp` to check some of these.

An `alist` is a list-of-pairs (&ldquo;improper lists&rdquo;) which has its own set of constraints and invariants. It, of course, has its own set of *procedures* (a module&rsquo;s *public interface*) too.

Just like with *Common Lisp*, it is an *imperative language*, with little or no emphasis on pure functions and immutable data, so one has to pay careful attention to what modifies and rewrites what (procedures performing &ldquo;destructive&rdquo; updates).

Generalization (abstraction out) into modules (modes) and code re-use.

-   `fundamental-mode`
    -   Major mode: `prog-mode`
        -   Minor mode: `python-mode`
    -   `comint-mode`
        -   Python shell - a *comint buffer*. (Inferior mode)

Each *mode* has its own set of *key bindings*, variables, and associated (internal) *functions*.

-   `C-h m` *describes* a mode (by actually traversing its internal LIST structures).
-   `C-h f`, and `C-h v` in turn, describe *functions* (&ldquo;commands&rdquo;) and *variables*.

This suggests that we have to setup (with *hooks* that set variables and *minor modes*) the more general `prog-mode` before all the *language-specific* modes.

There are so-called *global modes* which handle *buffers of different kinds* (`company`, `yasnippet)`, etc.

There are *major sub-systems* within Emacs (indentation, completion, snippets, pretty-symbols) or even micro-frameworks (company, lsp).

Since Emacs configuration is *imperative*, one has to pay careful attention to *the particular implicit order* in which packages are being loaded and initialized.

-   ligatures
-   pretty-symbols (part of the `prog-mode`, extended by `tuareg`)
-   indentation according to the *mode*
-   company (a completion framework)
-   yasnippet (a snippets engine)

A *programming language mode* typically specifies the following:

-   syntax of expressions,
-   the customary rules for indentation,
-   how to do syntax highlighting for the language, and
-   how to find the beginning or end of a function definition.
-   features for compiling and
-   debugging programs

Workflows

write down a cheat-sheet for the most used *key bindings*

Then everything is *discoverable* and *self-documented*

-   each key binding can be examined with `C-h k`
-   each *function* or a &ldquo;command&rdquo; with `C-h f`
-   each *mode* can be examined with `C-h m`
-   and each *variable* with `C-h v`

The `showkey-mode` could log all the keys and the functions they *bound* to The `IELM` is a REPL to run any LISP function or evaluate any expression

the `comint-mode` (also `eshell`) `C-c C-u` comint-kill-input ^u `C-c C-w` backward-kill-word ^w `C-c C-c` comint-interrupt-subjob ^c

`C-M-i` completion-at-point

evaluation `C-M-x` python-shell-send-defun: Send the current defun to inferior Python process. `C-c C-c` python-shell-send-buffer: Send the entire buffer to inferior Python process. `C-c C-e` python-shell-send-statement: Send the statement at point to inferior Python process. `C-c C-l` python-shell-send-file: Send FILE-NAME to inferior Python PROCESS. `C-c C-r` python-shell-send-region: Send the region delimited by START and END to inferior Python process. `C-c C-s` python-shell-send-string: Send STRING to inferior Python process.

`M-p` comint-previous-input `M-n` comint-next-input `M-r` comint-history-isearch-backward-regexp

xref

-   `M-.`
-   `M-,`
-   `C-M-.`

movements

-   `M-a`
-   `M-e`
-   `C-M-a`
-   `C-M-e`
-   `C-M-u`
-   `C-M-d`

-   `C-c C-d` python-describe-at-point
-   `C-c C-f` python-eldoc-at-point

`C-c C-z` switch-to-shell

use `ipython` as an inferior interpreter

Shortcut Function General Emacs Controls Ctrl + G Cancel or suspend a command. Ctrl + G, then Ctrl + G, then Ctrl + G Forcibly suspend a command. Ctrl + L Refresh the current screen. Alt + X, then “recover session” Restore any unsaved buffers. Ctrl + X, then Ctrl + C Save all buffers and close Emacs. Alt + X, then “customize” Open the built-in customization menu. File Manipulation Ctrl + X, then Ctrl + F Open a File Buffer. Ctrl + X, then Ctrl + S Save the current file in the buffer. Ctrl + X, then S Save all files in buffer. Ctrl + X, then Ctrl + D Open a Dired Buffer. Ctrl + X, then Ctrl + W Write the current file to a different buffer. Ctrl + X, then Ctrl + Q Turn the current buffer to Read-Only. Text Selection Alt + H Select the paragraph before the cursor. Ctrl + Alt + H Select the function before the cursor. Ctrl + X, then Ctrl + P Select everything in the current screen. Ctrl + H Select the entire buffer. Ctrl + Space Activate the region select tool. Text Manipulation Ctrl + W Cut the text within the selected region. Ctrl + D Cut the character after the Ctrl + K Cut the entire line after the cursor. Alt + K Cut the entire sentence after the cursor. Alt + U Convert the word before the cursor to uppercase. Ctrl + X, then Ctrl + U Convert the selected region to uppercase. Alt + L Convert the word before the cursor to lowercase. Ctrl + X, then Ctrl + L Convert the selected region to lowercase. Ctrl + T Switch the two adjacent letters before the cursor. Alt + T Switch the two adjacent words before the cursor. Text Formatting Ctrl + O Add a new line above the cursor. Ctrl + X, then Ctrl + O Remove any empty lines around the cursor. Alt + \\ Remove all spaces around the cursor. Alt + Q Truncate the paragraph to the current column length. Ctrl + X, F Set the current column length. Searching and Replacing Ctrl + S Search for text after the cursor. Ctrl + R Search for text before the cursor. Alt + P Use the previously searched text for searching. Ctrl + Alt + S Search for text after the cursor using regex. Ctrl + Alt + R Search for text before the cursor using regex. M + % Enter Emacs’ Interactive Replace menu. Buffer Movement Ctrl + F Move the cursor one character forward. Ctrl + B Move the cursor one character backward. Alt + F Move the cursor one word forward. Alt + B Move the cursor one word backward. Ctrl + N Move the cursor one line down. Ctrl + P Move the cursor one line up. Ctrl + V Scroll the entire buffer screen down. Alt + V Scroll the entire buffer screen up. Ctrl + E Move the cursor to the end of the current line. Ctrl + A Move the cursor to the start of the current line. Alt + E Move the cursor to the end of the current sentence. Alt + A Move the cursor to the start of the current sentence. Buffer Manipulation Ctrl + X, then 2 Split the current buffer horizontally. Ctrl + X, then 3 Split the current buffer vertically. Ctrl + X, then 4, then B Open an existing buffer as a vertical split. Ctrl + X, then 4, then F Open a file as a vertical split. Ctrl + X, then 4, then D Open directory as a vertical split. Ctrl + X, then 1 Delete all other splits aside from the currently selected one. Ctrl + X, then 0 Delete the currently selected split. Ctrl + X, then B Switch to a different buffer. Ctrl + X, then Ctrl + B Print a list of all existing buffers. Command Buffer Controls ? Suggest potential completion options. Alt + P Rewrite the previous command in the buffer. Alt + R Search backwards through the command buffer history. Alt + F Search forwards through the command buffer history. Shell Support Alt + X, then “term” Open a VT100 Terminal Emulator. Alt + X, then “eshell” Open an Emacs Lisp Terminal. Alt + ! Run a shell command from the command buffer. Alt + & Run a shell command and fork the process to the background. Keyboard Macros Ctrl + X, then ( Create an Emacs keyboard macro. Ctrl + X, then ) Save an Emacs keyboard macro. Ctrl + X, then E Run the last Emacs macro defined. Lisp-specific Functions Ctrl + X, then Ctrl + E Run the currently selected Lisp expression. Ctrl + Alt + X Run the currently selected a Lisp function. Emacs Help System Ctrl + H, then ? Open a summary of all the options for the Help system. Ctrl + H, then A Search for a specific Help topic. Ctrl + H, then F Open a Help window about the highlighted Lisp function. Ctrl + H, then V Open a Help window about the highlighted Lisp variable. Ctrl + H, then M Open a Help window for the current Major Mode. Ctrl + H, then P Search for an installed Emacs package. Ctrl + H, then Shift + P Search for the documentation of an Emacs package. Ctrl + H, then I Open the Emacs Info Screen. Ctrl + H, then Ctrl + F Open the Emacs FAQ. Ctrl + H, then Ctrl + N View the most recent news about Emacs. Emacs Info Screen H Open the Info Screen tutorial. Space Scroll down the currently displayed text by half a screen. Backspace Scroll up the currently displayed text by half a screen. N Go to the next Info node for the document. P Go to the previous Info node for the document. T Go to the top Info node for the document. D Go to the document’s Table of Contents. L Go back to the last Info node that you read. Q Exit Emacs’ Info Screen Mode.