; This is where I FULLY NAIL macros ONCE AND FOR ALL

(setq charlie-x 42)

(defmacro nil! (x)
  (list 'setq x nil))

(nil! charlie-x)

(defmacro cool-nil! (x)
  `(setq ,x nil))

(cool-nil! charlie-x)

(defmacro n-times (n &rest body)
  `(dotimes (counter ,n 42)
    ,@body))

(setq x 42)

(n-times 3
         (setq x (+ 5 x)))

(defmacro my-if (condition then-form else-forms)
  `(cond (,condition ,then-form)
         (t (progn (,@else-forms)))))

(my-if nil
       (print 'this-should-not-print)
       'completely-false)

;(defmacro nth-expr (n &rest expressions)
;  `(eval  (nth ,n (quote ,expressions))))

;(defmacro with-var (n &rest expressions)
;  (let ((the-expr `(nth ,n (quote ,expressions)))
;        (print the-expr)
;        the-expr)))


; (nth-expr 1 (+ 1 2) (* 3 4) (+ 100 7))

; (with-var 1 (+ 1 2) (* 3 4) (+ 100 7))

; OK, a TON of learning just occurred here!!!! I'm going to capture it ALL right here!
; So the exercise was from Paul Graham;s On Lisp, page 174, # 3: Define a macro called nth-expr that takes a number n followed
;  by one or more expressions, and returns the value of the nth expression.

; I had some struggles with lisp syntax, used some parens where they weren't needed, flopped around a bit, but eventually came up
; with this:

;(defmacro nth-expr (n &rest expressions)
;  `(eval  (nth ,n (quote ,expressions))))

; Now, that's pretty good, but has some major unacceptable problems. First of all, it seemed like I needed to use eval. Otherwise,
;  I could return the expression that was selected, but not eval it!
; THIS IS A CODE SMELL. ANYTIME YOU THINK YOU NEED EVAL IN A MACRO, YOU ARE PROBABLY CONFUSED ABOUT WHAT YOU WANT TO DO AT COMPILE-TIME
;  VERSUS RUNTIME! i'LL spell that out more below.

;; note: Originally, I had an implementation that was not using "quote". It had another unacceptable problem: it was evaluating all
;  of the expressions in the list, not just the chosen one. "quote" was the fix for that.

; Now, let's get a handle on the "eval inside macro" problem.
; I was using the following call as a "model" for what I wanted the macro to acheive:
; (nth-expr 1 (+ 1 2) (* 3 4) (+ 100 7))
;
; But that's not a very good model, because in it, EVERYTHING CAN BE RESOLVED AT COMPILE TIME! However, that is usually not the case with
; a macro. And this misleading (over-simplified) example was leading me down the wrong road.

; The best advice I know of yet is to start with exactly the expanded code you want. Then see how to generate that code. Based on
;  the over-simplified example, I thought, "Hey, the generated code should just be "12"". Because the 1th element is (* 3 4), which
;  is 12. But that's WRONG!!!

; Imagine someone made this call: (nth-expr 1 (+ 1 2) (yes-or-no-p "Yes or no?") (+ 100 7))

; Do you REALLY think that you want to eval yes-or-no-p at COMPILE time??? Of COURSE not. You want to evaluate that at RUNTIME. That's
;  99.99999999% of the time what you want with a macro!

; So, I fix my mindset, and now I say, "OK, let the expanded code be this:" (yes-or-no-p "Yes or no?")
; Well, that's a step in the right direction, but it is WRONG AGAIN.

; Why? Because, you're assuming that "n" should be resolved at compile time. But no.
; What if someone made this call: (nth-expr (read-from-minibuffer "Pick a number: ") (+ 1 2) (* 3 4) (+ 100 7))

; Again, read-from-minibuffer shold be evaluated at RUNTIME, not compile time. So again, we should not be evaluating their parameter, but
;  rather, moving it around into the right place in the input.

; And all of that sets the stage for the CORRECT implementation. Which I'm going to implement below when I get a chance to get back
; to work!

















(provide 'experiment)
