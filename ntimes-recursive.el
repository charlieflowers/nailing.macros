;; -*- lexical-binding: t -*-

; This is Paul Graham's "ANSI Common Lisp" exercise 10.4 from page 174.

; Implement a version of ntimes that is implemented using a local recursive function instead of a do.
;
; Turns out, there's a huge section in the Emacs Lisp manual about recursion, and it's totally kosher. There is a suggestion that you
;   increase 2 variables that have some impact on how much stack space you get (to help avoid stack overflow). I'm not messing with
;   those right now, because this is just a learning exercise at the moment.

; So, you want someone to call like this: (ntimes-recursive (3 (print "hi")))
;
; I need to figure out what the expanded code should look like. But I have to do that in stages. Somewhere in the mix there needs
;   to be a local function like this:
;(defun do-task (n some-task)
;  (some-task)
;  (if (> n 1)
;      (do-task (- n 1) some-task)))

; Of course, that is a mere approximation and is not even compilable on its on. "some-task" is not the name of a function. I'm just
;   trying to get a feel for how the recursion would look.

; Now, we DO want multiple evaluation, because our semantics promise we will eval this stuff n times. We want to output code that
;   uses a local recursive function.

; And interestingly, here we go. Another case of whether or not to use "cl" inside elisp code. The Common Lisp way to declare a local
;   function is a form called FLET. But emacs does NOT include FLET. It is, however, provided in the cl package. Note that, ALL it
;   does is set the function to the symbol's "function" slot instead of its "value" slot, so that you can say (foo 42) instead of
;   (funcall foo 42). So it seems unquestioningly valuable. And I'm going to use it here. (Also, of course, keep in mind that it is
;   an exercise from a Common Lisp book that explicitly forced me down this road. So it's easy to make the case that FLET belongs here
;   without having to even comment on the debate for or against cl in general).

(require 'cl)

; How should the expansion for (ntimes-recursive 3 (print "hi")) look?

(flet ((f (x)
          (print "hi")
          (if (> x 1) (f (- x 1)))))
  (f 3))

; Yep, that seems to be it. And it runs successfully too.
; So let's generate that code...

(defmacro ntimes-recursive (n &rest actions)
  `(flet ((f (x)
             ,@actions
             (if (> x 1) (f (- x 1)))
             ))
     (f ,n)))

; Note, thus far I've left out the if that does the recursion. I want to check that I've embedded the actions properly.

(ntimes-recursive 3 (print "hi") (print "there"))

; That really does seem right. Due to some silly prelude error, I can't see the output because it swears I need a 'provide statement
;   at the bottom of this file even though it's not a package. So lemme run it in the repl.

; YES!!! Working BEAUTIFULLY!!!