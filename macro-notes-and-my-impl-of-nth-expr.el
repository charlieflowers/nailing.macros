;; -*- lexical-binding: t -*-

; This is where I FULLY NAIL macros ONCE AND FOR ALL

(require  'cl)

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

; Here we go. I want the expanded code to look like this:
;(cond ((equal-p n 0) expr1)
;      ((equal-p n 1) expr2)
;      ((equal-p n 2) expr2)
;      (t (error "figure this out")))

; Or, to be more specific (which I think is probably wise when dealing with macros as a noob):
;(cond ((eq 1 0) (+ 1 2))
;      ((eq 1 1) (* 3 4))
;      ((eq 1 2) (+ 100 7))
;      (t (error "Figure this out")))

; Notehow I got rid of "n". In the expanded code, it won't be there. The "1" will merely repeat. But to be fully free from confusion,
;  let's see what it looks like when I use values for BOTH expressions that CLEARLY are runtime values:
; (cond ((eq (read-from-minibuffer "Pick number: ") 0) (print "expression 0") )
;      ((eq (read-from-minibuffer "Pick number: ") 1) ((yes-or-no-p "Yes or no?")))
;      ((eq (read-from-minibuffer "Pick number: ") 2) (+ 100 7))
;       (t (error "Figure this out")))


; As you can see, that highlights at least one additional problem. The condition should not be evaluated over and over again. Once
;  is enough and more is an error. Don't be confused by the fact that, in this case, read-from-minibuffer would be fine. It is merely
;  an EXAMPLE. We cannot be doing multiple evals of what they send unless it fits our intentional and explicit semantics.

; So, we're looking at something like this:
;(let (condition-value (read-from-minibuffer "Pick number: "))
;  (print "Condition value is " condition-value)
;  (cond ((eq condition-value 0) (print "expression 0"))
;        ((eq condition-value 1) (yes-or-no-p "Yes of no?"))
;        ((eq condition-value 2) (+ 100 7))
;        (t (error "Figure this out"))))

; Something is wrong with that because it won't eval. To figure it out, I'll wrap it in a function and use (interactive).
(defun test-it (x)
  "some doc string"
  (interactive "nPick number: ")
  (message "You picked %d" x)
  (cond ((eq x 0) (print "expression 0"))
        ((eq x 1) (yes-or-no-p "Yes of no?"))
        ((eq x 2) (+ 100 7))
        (t (error "Figure this out"))))

; Now that function works correctly. So I'm on the right track. Unfortunately, I don't know the right way to use read-from-minibuffer
;  yet, but I also don't care yet. That will come with time. The point is, I confirmed I have working code that only evals the
;  condition one time, but which chooses the correct expression based on the condition, and which DEFINITELY proves that it is
;  not determining the value of the condition or the expressions until RUNTIME (because it depends on answers I only provide at
;  runtime).

; The point is this: (1) First, nail down the condition value. (2) Then, generate the branches of the cond, each referring to the
;  condition value. (3) Tack on an error branch to the cond.

; So, here's what I want the generated code to look like:
(let ((condition-value 1))
  (cond ((eq condition-value 0) (print "expression 0"))
        ((eq condition-value 1) (* 3 4))
        ((eq condition-value 2) (+ 100 7))
        (t (error "You didn't give that many expressions, dude"))))

; And that is BEAUTIFUL. BUT WAIT! IT'S STILL NOT QUITE RIGHT!!!!!!!
; The only problem left that I see is one of hygiene! And you know I'm a stickler for hygiene! We introduce the "condition-value"
;  variable, and if the "end-user code" (if you'll pardon a lazy phrase) uses a var with that same name, whammo! So, let's use
;  gensym for it!
; Remember, the literal word "gensym" never (or at least not usually) shows up in the expanded code. It just gets used to produce
;  the expanded code. So I want the code to look something like this:
(let ((G342 1))
  (cond ((eq G342 0) (print "expression 0"))
        ((eq G342 1) (* 3 4))
        ((eq G342 2) (+ 100 7))
        (t (error "You didn't give enough expressions for that number"))))

; And THAT is RIGHT!!! JESUS! Most of the work is in figuring out WHAT you want the expanded code to look like! (Or is it? I say that
;  now, but I haven't gotten to the next part yet, so let's see).

; Note: Funny, I wrote all those notes above with GENSYM in mind because, earlier today, I did C-h f "gensym" and confirmed that my
;  emacs DOES have a function called GENSYM. Well, it turns out, that is NOT built into emacs! That's in a package called "cl", which
;  I THINK is common lisp extensions that you can use in emacs lisp. But it appears the "elisp way" is to use make-symbol, which
;  creates a symbol but does not intern it. And my good friend Pascal Bourguignon points out that make-symbol would also work
;  perfectly fine in Common Lisp ... it's just that the make-symbol approach is not PRINTABLE. If you print the expansion and try to
;  execute it, the symbol "foo" will not be the same as the symbol "foo", making that shit really hard to work with! That's WHY
;  Common Lisp has GENSYM. And it appears to be available to me due to the cl package. For now, I'm going to verify it really is cool
;  for me to use it (make sure it is not something totally different than I think), and then I'll use it.

; Well this is FASCINATING!! There is CONTROVERSY about the CL package!!! Summed up VERY nicely at:
; http://ergoemacs.org/emacs/elisp_common_lisp_in_emacs.html
;
; It appears that RMS (Stallman) had a certain vision in mind for Emacs Lisp that was *distinctly* different from Common Lisp. He
;  is NOT AT ALL a fan of Common Lisp. He feels it is UGLY. He appears to be more a fan of Scheme. I don't yet know which features he
;  hates so much, but I do have to give him credit for giving us Emacs, which is an amazing creation.
;
; However, the fact remains, we need a bunch of those functions that are implemented in the cl package. If not them, then we need their
;  more "ELisp-agreeable" equivalents. And those equivalents don't exist as far as I know. Right now, I need those capabilities. And
;  I do not share Stallman's "CL is ugly" view (hell I just learned it). I don't know enough of Scheme to begin to grasp those views yet.
;  But I do know the implementer of eshell, and XEmacs, and a bunch of others have used the HELL out of cl.el and found it to be
;  rock solid for themselves. So I'm going to use it when I need it -- including at runtime. One day way down the road maybe I'll revisit
;  that.

; So for now, I'm going to require "cl.el" and use cl-gensym (they named it that to stick it in its own CL "namespace").

; Here's the goal expansion code again:
(let ((G342 1))
  (cond ((eq G342 0) (print "expression 0"))
        ((eq G342 1) (* 3 4))
        ((eq G342 2) (+ 100 7))
        (t (error "You didn't give enough expressions for that number"))))

; Now, let's try to make some code that GENERATES that code.
;(defmacro nth-expr (number &rest expressions)
;  (let ((gensym (cl-gensym "condition-value")))
;    `(let ((,gensym ,number))
;       ())))

; Well, that's part one. I'm pretty sure that uses the gensym correctly. Now, however, we have the tricky bit of generating the
;  conditional clauses. That is gonna force me to brush up on some lisp! I'm picturing some do loop where I push shit onto a list. That
;  would work, but something in the other corner of my mind knows that would be a shitty brute force inelegant way to do it. And there
;  are the vague memories of beautiful, elegant uses of mapcar to do stuff like this. So, off to do a little reading....

; Indeed, mapcar looks beautiful for this. I wondered if it was going to be only in cl, but no, it is a built-in c function of Emacs.
;  We merely need a list of <condition expression>, and we can splice it right inside the cond.
(mapcar #'(lambda (expr) `((eq x c) expr)) '((+ 1 2) (* 3 4) (+ 100 7)))

; That's the right rough form. However, mapcar does NOT let me get the index of the element I'm currently working on. And I need that.
;  I could scan Graham's Common Lisp to see if he finds a way to do that with mapcar. But I just realized that Haskell seems to have
;  ways of doing this, and perhaps some of them exist in the lisp world.
;
; It looks like you can implement fold with reduce (and reduce is an Emacs function). But reduce or even fold is not a perfect fit
;  for what I'm trying to do.
;
; I wonder if there is another construct besides "case" that I can use to accomplish this job, that won't require the index. Let me see...

; Actually, I can't see how that would be possible. Each branch has to compare a runtime value to a number, and therefore the index is
;   absolutely going to be required. So Let's scan Graham for some fancy use of mapcar for this, and failing that we'll have to either
;   loop or recurse.

; OK, from what I can tell, there's no other "idiomatic" way to do it than the things I've already considered, such as writing an
;   explicit loop and explicitly pushing shit onto a list, or using a lexical closure to define the counter and incrementing it inside
;   my mapcar function. So ... I'd opt for using the lexical closure. But there's some uncertainty about how much / whether elisp
;   supports lexical scoping. So let me look into THAT :)

; OK, I checked into it. You turn it on on a file-by-file basis. It is off by default. However, emacs plans to depend more and more on it.
;   So I assume it is solid. And lexical closures are simply my second favorite language feature of lisp, after macros, so I'm DEFINITELY
;   gonna use it. I'll turn it on in this file and use it for this macro.

; The idea is to define a variable for the counter, use it in the mapcar function, and increment it in the mapcar function.
(let ((counter -1))
  (mapcar #'(lambda (expr)
              (incf counter)
              `((eq x ,counter) ,expr))
          '((+ 1 2) (* 3 4) (+ 100 7))))

; Yes, that is looking good. I *think* that "counter" is safe from variable capture, but my brain is sleepy now at nearly 1 am, and I'll
;   have to figure that out tomorrow. But, I can proceed with the code I have here and implement the macro tonight, hell yes.
; Again, here's the desired expansion code:

(let ((G342 1))
  (cond ((eq G342 0) (print "expression 0"))
        ((eq G342 1) (* 3 4))
        ((eq G342 2) (+ 100 7))
        (t (error "You didn't give enough expressions for that number"))))

(defmacro nth-expr (n &rest expressions)
  (let ((condition-sym (cl-gensym "condition-value")))
    `(let ((,condition-sym ,n))
       (cond ,@(let ((counter -1))
                 (mapcar #'(lambda (expr)
                             (incf counter)
                             `((eq ,condition-sym ,counter) ,expr)
                             )
                         expressions)))
       )))

(nth-expr 1 (+ 1 2) (* 3 4) (+ 100 7))

; well, SHIT, that is not working right. Tells me Symbol \ is not a function or some shit like that. Gonna have to give it up for
;  tonight and go to fucking BED. Back at it tomorrow. Later!

; AND THERE IT IS! I Got up this morning and knocked it right out. Just needed a little tweaking. The second arg to mapcar was
;   not the list of expressions, as I intended, but instead, (/, expressions). The / somehow arose from the fact that I had an unneeded
;   comma in front of "expressions". The comma was unneeded because we were still inside the scope of ",@". And the only remaining
;   thing I had to do was add " ,expr" to the "eq" line. Now I've confirmed it works properly and only evaluates the expression that
;   is chosen!

; A nested backquote is no big mystery. It's just like a nested regular quote, except it lets you "opt out of literalness" in the middle
;   somewhere. Sweet.














(test-it 0)



























(provide 'experiment)
