Adds a new Evil state called --LISP-- (<L>) with mnemonics key bindings
to navigate Lisp code and edit the sexp tree.

Principle:
----------

To execute a command while in normal state, the evil-leader is used.
By default, the prefix for each command is `<leader> m`.
Commands when executed set the current state to `lisp state`.

By example, to slurp three times while in normal state:
    <leader> m 3 s
Or to wrap a symbol in parenthesis then slurping two times:
    <leader> m w 2 s

Key Binding    | Function
---------------|------------------------------------------------------------
`leader'       | evil leader
`leader m %'   | evil jump item
`leader m :'   | ex command
`leader m ('   | insert expression before (same level as current one)
`leader m )'   | insert expression after (same level as current one)
`leader m $'   | go to the end of current sexp
`leader m ` k' | hybrid version of kill sexp (can be used in non lisp dialects)
`leader m ` p' | hybrid version of push sexp (can be used in non lisp dialects)
`leader m ` s' | hybrid version of slurp sexp (can be used in non lisp dialects)
`leader m ` t' | hybrid version of transpose sexp (can be used in non lisp dialects)
`leader m 0'   | go to the beginning of current sexp
`leader m a'   | absorb expression
`leader m b'   | forward barf expression
`leader m B'   | backward barf expression
`leader m c'   | convolute expression
`leader m ds'  | delete symbol
`leader m dw'  | delete word
`leader m dx'  | delete expression
`leader m e'   | (splice) unwrap current expression and kill all symbols after point
`leader m E'   | (splice) unwrap current expression and kill all symbols before point
`leader m h'   | previous symbol
`leader m H'   | go to previous sexp
`leader m i'   | switch to `insert state`
`leader m I'   | go to beginning of current expression and switch to `insert state`
`leader m j'   | next closing parenthesis
`leader m J'   | join expression
`leader m k'   | previous opening parenthesis
`leader m l'   | next symbol
`leader m L'   | go to next sexp
`leader m p'   | paste after
`leader m P'   | paste before
`leader m r'   | raise expression (replace parent expression by current one)
`leader m s'   | forwared slurp expression
`leader m S'   | backward slurp expression
`leader m t'   | transpose expression
`leader m u'   | undo
`leader m U'   | got to parent sexp backward
`leader m C-r' | redo
`leader m v'   | switch to `visual state`
`leader m V'   | switch to `visual line state`
`leader m C-v' | switch to `visual block state`
`leader m w'   | wrap expression with parenthesis
`leader m W'   | unwrap expression
`leader m y'   | copy expression

Configuration:
--------------

Key bindings are set only for `emacs-lisp-mode' by default.
It is possible to add major modes with the variable
`evil-lisp-state-major-modes'.

It is also possible to define the key bindings globally by
setting `evil-lisp-state-global' to t. In this case
`evil-lisp-state-major-modes' has no effect.

The prefix key is `<leader> m' by default, it is possible to
change the `m' key to anything else with the variable
`evil-lisp-state-leader-prefix'. Set it to an empty string
if you want all the commands to be directly available
under the `<leader>' key.
