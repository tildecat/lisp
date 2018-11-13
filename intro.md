---
title: My introduction to Common Lisp
---

## Purpose
So I want to finally learn some Lisp. Today there are many languages claiming to be lisp, but as far as I can tell there are only 5 really active dialects.

The oldest one is [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) which started it's life in 1975. The nice thing about Scheme is that it's a **very** small language. Next one is [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp) which started almost 10 years later - in 1984. The nice thing about Common Lisp is that it is **very** big language. After next 10 years (in 1995) we got [Racket](https://racket-lang.org/) (back than called DrScheme). The nice thing about Racket is quality of its' documentation and it's macro system. Finally after 12 years (in 2007) we got [Clojure](https://en.wikipedia.org/wiki/Clojure). The nice thing about Clojure is that it's running on JVM and its solution to state management in multithreaded applications. So, those are (as far as I can tell) 4 major dialects active today[^1], where is the fifth one? It's [Emacs Lisp](https://en.wikipedia.org/wiki/Emacs_Lisp) (from 1985). The nice thing about Elisp is that it's used as scripting language in Emacs.

[^1]: It's 11 years since major new Lisp dialect, should we expect new one soon? Will it be [Fennel](https://fennel-lang.org/) or maybe [Hy](http://hylang.org/), or maybe one I've never heard of?

So which one to chose? I think all of them are good choice. I've dabbled in all of them except Elisp. In the end I've decided to learn Common Lisp and will be using [Sbcl](http://www.sbcl.org/) implementation.

Now that we have our language chosen, how should we learn it? There is a great [post by Steve Losh](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp) about this. I will follow it to some extent, but since I know *something* about CL[^2] I will sometimes make detours or might change some things. So the point of this document(s?) will by to document my journey that starts with me as almost complete CL beginner.

[^2]: I will reference Common Lisp as CL.

## Hello Euler

So my first program will be one solving first problem from [Project Euler](https://projecteuler.net/)[^3]. We are to find sum of all numbers that are multiplies of 3 or 5 and are below 1000. To do that we have to know how to define functions in CL. For our purposes it's enough to know that we first need to use *macro* [defun](http://clhs.lisp.se/Body/m_defun.htm), followed by name of function, its list of arguments as a list and finally its body. So function that returns `t` when number is multiple of 3 is one which checks reminder of division by 3. If it's 0 than we have multiple of 3, otherwise it's not the number we are looking for:

[^3]: I warned you that Steve's post will not be followed exactly.

```cl
(defun d3 (x) (= 0 (mod x 3)))
```

This declares function `d3` with one parameter `x`. It's body first computes reminder of division by 3 of `x` and than compares that reminder to 0. Now that we know that it's should be easy to write function that checks divisibility by 5:

```cl
(defun d5 (x) (= 0 (mod x 5)))
```

Since our numbers are to be divisible by 3 **or** 5 we need to combine those functions:

```cl
(defun d (x) (or (d3 x) (d5 x)))
```
Now we can detect our numbers. What is left is to find all such numbers below 1000. What seems to be simplest is to generate all numbers from 1 to 1000, remove ones for which `d` returns `nil` and finally sum what remains.

We can solve this in multiple ways. First we will try to write as much code as we can, so that later we can refactor it to use built in functions.

So how to generate this list of 1000 numbers? We will write a function that tracks what is the next number to generate. If it is less than maximum we want it will cons[^4] this nubmer with result of recursive call of itself with next number incremented by one:

[^4]: [cons](http://www.lispworks.com/documentation/lw70/CLHS/Body/f_cons.htm)ing is primary way to allocate in CL, it creates pair of values. Thos pairs are (among other things) building blocks of CL lists.

```cl
(defun gen (min max)
  (if (>= min max)
      nil
      (cons min (gen (+ 1 min) max))))
```
Using `trace` you can see how it is working:
```
CL-USER> (trace gen)
(GEN)
CL-USER> (gen 5 10)
  0: (GEN 5 10)
    1: (GEN 6 10)
      2: (GEN 7 10)
        3: (GEN 8 10)
          4: (GEN 9 10)
            5: (GEN 10 10)
            5: GEN returned NIL
          4: GEN returned (9)
        3: GEN returned (8 9)
      2: GEN returned (7 8 9)
    1: GEN returned (6 7 8 9)
  0: GEN returned (5 6 7 8 9)
(5 6 7 8 9)
```

Now we need to figure out how to remove elements from that list that do not satisfy our predicate `d`. To do that we will iterate over the list and apply our predicate to each element, ignoring ones which do not satisfy it and keeping (consing) those that do. To do that we will need to know how to apply any predicate to value without hardcoding it. In CL we can do that with [funcall](http://clhs.lisp.se/Body/f_funcal.htm) function.

```cl
(defun filter (pred list)
  (cond
    ((null list) nil)
	((funcall pred (car list)) (cons (car list) (filter pred (cdr list))))
	(t (filter pred (cdr list)))))
```
Final task is to sum together all filtered numbers:

```cl
(defun sum (l)
  (if (null l)
      0
      (+ (car l) (sum (cdr l)))))
```

We can use those functions we just wrote to arrive at solution:

```cl
(defun solve1 (&optional (max 1000))
  (sum (filter 'd (gen 1 max))))
```

And we can see that it is indeed working:
```
CL-USER> (untrace gen)
T
CL-USER> (solve1 1000)
233168
CL-USER> (solve1)
233168
```

But CL is a big language and we didn't have to write all of that by hand. In fact we can replace our `filter` function with [remove-if-not](http://clhs.lisp.se/Body/f_rm_rm.htm) and `sum` with [reduce](http://clhs.lisp.se/Body/f_reduce.htm):

```cl
(defun solve2 (&optional (max 1000))
  (reduce '+ (remove-if-not 'd (gen 1 max))))
```

Finally there is one huge function that can be used to replace all of our code called `loop`:

```cl
(defun solve (&optional (x 1000))
  (loop for n from 1 below x by 1 when (d n) sum n))
```

## Performance optimization

CL is supposed to be fast, so lets measure how fast our functions are. To do that we will use [time](http://clhs.lisp.se/Body/m_time.htm) macro:

```
CL-USER> (loop for f in '(solve1 solve2 solve) collect (time (funcall f 10000)))
Evaluation took:
  0.001 seconds of real time
  0.001666 seconds of total run time (0.001666 user, 0.000000 system)
  200.00% CPU
  5,657,736 processor cycles
  229,376 bytes consed
  
Evaluation took:
  0.002 seconds of real time
  0.001398 seconds of total run time (0.001398 user, 0.000000 system)
  50.00% CPU
  4,744,446 processor cycles
  229,376 bytes consed
  
Evaluation took:
  0.001 seconds of real time
  0.000921 seconds of total run time (0.000921 user, 0.000000 system)
  100.00% CPU
  3,128,434 processor cycles
  0 bytes consed
  
(23331668 23331668 23331668)
```

We see that last function we wrote seems to be fastest, but the times are all very small - can we increase problem size? In case of our own functions not really - `gen` suffers from serious issue. It can't be optimized into loop via [tail call optimization](https://en.wikipedia.org/wiki/Tail_call):
```
CL-USER> (solve1 1000)
233168
CL-USER> (solve1 10000)
23331668
CL-USER> (solve1 100000)
Control stack guard page temporarily disabled: proceed with caution
; Evaluation aborted on #<SB-KERNEL::CONTROL-STACK-EXHAUSTED {10021CD643}>.
CL-USER> (solve2 1000)
233168
CL-USER> (solve2 10000)
23331668
CL-USER> (solve2 100000)
Control stack guard page temporarily disabled: proceed with caution
; Evaluation aborted on #<SB-KERNEL::CONTROL-STACK-EXHAUSTED {10023AE083}>.
```
Instead of fixing this problem with `gen` lets see what micro-optimizations we can apply[^5] to final version of `solve`. To do that lets first estabilish some baseline performance:

[^5]: Just for fun and education.

```
CL-USER> (time (solve 100000000))
  3.767 seconds of real time
  3.766975 seconds of total run time (3.766975 user, 0.000000 system)
  100.00% CPU
  12,808,428,054 processor cycles
  0 bytes consed
  
2333333316666668
```

Now we can ask our compiler to generate fast code for our function using [optimize](http://clhs.lisp.se/Body/d_optimi.htm#speed) declaration:

```cl
(defun solve-opt (&optional (x 1000))
  (declare (optimize (speed 3)))
  (loop for n from 1 below x by 1 when (d n) sum n))
```

Compiling that function generate 4 notes from compiler:

```
; in: DEFUN SOLVE-OPT
;     (LOOP FOR N FROM 1 BELOW X BY 1
;           WHEN (D N)
;           SUM ...)
; --> BLOCK LET SB-LOOP::WITH-SUM-COUNT LET TAGBODY WHEN IF >= OR LET 
; --> IF = IF 
; ==>
;   (= SB-C::X SB-C::Y)
; 
; note: unable to open code because: The operands might not be the same type.

; --> BLOCK LET SB-LOOP::WITH-SUM-COUNT LET TAGBODY WHEN IF >= OR LET > 
; --> IF 
; ==>
;   (> SB-C::X SB-C::Y)
; 
; note: forced to do GENERIC-> (cost 10)
;       unable to do inline fixnum comparison (cost 4) because:
;       The first argument is a (INTEGER 1), not a FIXNUM.
;       The second argument is a REAL, not a FIXNUM.

; --> BLOCK LET SB-LOOP::WITH-SUM-COUNT LET TAGBODY IF SETQ THE 
; ==>
;   (+ #:LOOP-SUM-1 N)
; 
; note: forced to do GENERIC-+ (cost 10)
;       unable to do inline fixnum arithmetic (cost 2) because:
;       The first argument is a UNSIGNED-BYTE, not a FIXNUM.
;       The second argument is a (INTEGER 1), not a FIXNUM.
;       The result is a (VALUES (INTEGER 1) &OPTIONAL), not a (VALUES FIXNUM
;                                                                     &REST T).
;       unable to do inline (signed-byte 64) arithmetic (cost 5) because:
;       The first argument is a UNSIGNED-BYTE, not a (SIGNED-BYTE 64).
;       The second argument is a (INTEGER 1), not a (SIGNED-BYTE 64).
;       The result is a (VALUES (INTEGER 1) &OPTIONAL), not a (VALUES
;                                                              (SIGNED-BYTE 64)
;                                                              &REST T).
;       etc.

; --> BLOCK LET SB-LOOP::WITH-SUM-COUNT LET TAGBODY 
; --> SB-LOOP::LOOP-DESETQ SETQ THE 1+ 
; ==>
;   (+ N 1)
; 
; note: forced to do GENERIC-+ (cost 10)
;       unable to do inline fixnum arithmetic (cost 1) because:
;       The first argument is a (INTEGER 1), not a FIXNUM.
;       The result is a (VALUES (INTEGER 2) &OPTIONAL), not a (VALUES FIXNUM
;                                                                     &REST T).
;       unable to do inline fixnum arithmetic (cost 2) because:
;       The first argument is a (INTEGER 1), not a FIXNUM.
;       The result is a (VALUES (INTEGER 2) &OPTIONAL), not a (VALUES FIXNUM
;                                                                     &REST T).
;       etc.
; 
; compilation unit finished
;   printed 4 notes
```

It seems that there are some problems with our function that prevent compiler from generating fast code. But did those problems stopped compiler completely? Lets see:

```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  3.775 seconds of real time
  3.774962 seconds of total run time (3.774962 user, 0.000000 system)
  100.00% CPU
  12,834,910,795 processor cycles
  0 bytes consed
  
2333333316666668
```
No change - it seems that compiler wasn't able to optimize our code. So lets see one more time those notes. It looks like addition somewhere in `loop` macro  can't assume that me are adding integers (`GENERIC-+`). Similar issue seems to be with `>` operator (`GENERIC->`). We should be able to fix that by specifying types:

```cl
(defun solve-opt (&optional (x 1000))
  (declare (optimize (speed 3)))
  (loop for n fixnum from 1 below x by 1 when (d n) sum n))
```

This change make our code a little bit faster (from 3.775s down to 3.392s):

```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  3.392 seconds of real time
  3.392041 seconds of total run time (3.392041 user, 0.000000 system)
  100.00% CPU
  11,533,339,425 processor cycles
  0 bytes consed
  
2333333316666668
```

But when compiling we still got 1 note:

```
; note: forced to do GENERIC-+ (cost 10)
;       unable to do inline fixnum arithmetic (cost 2) because:
;       The first argument is a UNSIGNED-BYTE, not a FIXNUM.
;       The result is a (VALUES (INTEGER 1) &OPTIONAL), not a (VALUES FIXNUM
;                                                                     &REST T).
;       unable to do inline (signed-byte 64) arithmetic (cost 5) because:
;       The first argument is a UNSIGNED-BYTE, not a (SIGNED-BYTE 64).
;       The result is a (VALUES (INTEGER 1) &OPTIONAL), not a (VALUES
;                                                              (SIGNED-BYTE 64)
;                                                              &REST T).
;       etc.
```

Is this related to missing type specifier of function argument `x`? No, it's not - I tried to specify x to be `fixnum` but that did not remove that note. The problem is that `sum` *section*[^6] of loop needs to know type in which it should store result. So lets specify that:

[^6]: What is the proper way to address this part of code?

```cl
(defun solve-opt (&optional (x 1000))
  (declare (optimize (speed 3)))
  (loop for n fixnum from 1 below x by 1 when (d n) sum n fixnum))
```

Did it change anything? Not really:

```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  3.357 seconds of real time
  3.356738 seconds of total run time (3.356738 user, 0.000000 system)
  100.00% CPU
  11,413,433,639 processor cycles
  0 bytes consed
  
2333333316666668
```
Lets add optimizations declaration to `d3`, `d5` and `d5`:

```cl
(defun d3 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 3)))
(defun d5 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 5)))
(defun d (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (or (d3 x) (d5 x)))
```

This still produces 2 notes but I don't know enough about CL to fix them. But this still is a big win (from 3.357s to 2.421s):

```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  2.421 seconds of real time
  2.420994 seconds of total run time (2.420969 user, 0.000025 system)
  100.00% CPU
  8,231,452,745 processor cycles
  0 bytes consed
  
2333333316666668
```

## Profiling

So is this the end? Maybe not - we can try [profiling](http://www.sbcl.org/manual/#Statistical-Profiler) our code and stop optimizing blindly using :

```
CL-USER> (require :sb-sprof)
("SB-SPROF")
CL-USER> (sb-sprof:with-profiling (:report :flat) (solve-opt 100000000))

Number of samples:   242
Sample interval:     0.01 seconds
Total sampling time: 2.4199998 seconds
Number of cycles:    0
Sampled threads:
 #<SB-THREAD:THREAD "repl-thread" RUNNING {1003DA83E3}>

           Self        Total        Cumul
  Nr  Count     %  Count     %  Count     %    Calls  Function
------------------------------------------------------------------------
   1    120  49.6    120  49.6    120  49.6        -  D3
   2     80  33.1     80  33.1    200  82.6        -  D5
   3     21   8.7    138  57.0    221  91.3        -  D
   4     18   7.4    244 100.8    239  98.8        -  SOLVE-OPT
   5      0   0.0    242 100.0    239  98.8        -  "Unknown component: #x52C45920"
...
```

We can see that majority of time is spent in `d3` and `d5` functions. Maybe we can [inline](http://clhs.lisp.se/Body/d_inline.htm) them? Lets do that:

```cl
(declaim (inline d3))
(defun d3 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 3)))
(declaim (inline d5))
(defun d5 (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (= 0 (mod x 5)))
```
This gives us another big performance win (from 2.421s down to 1.807s):
```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  1.807 seconds of real time
  1.807050 seconds of total run time (1.807050 user, 0.000000 system)
  100.00% CPU
  6,143,959,987 processor cycles
  0 bytes consed
  
2333333316666668
```
Profiling once again shows that `d` is major time sink:
```
CL-USER> (sb-sprof:with-profiling (:report :flat) (solve-opt 100000000))

Number of samples:   181
Sample interval:     0.01 seconds
Total sampling time: 1.81 seconds
Number of cycles:    0
Sampled threads:
 #<SB-THREAD:THREAD "repl-thread" RUNNING {1003DA83E3}>

           Self        Total        Cumul
  Nr  Count     %  Count     %  Count     %    Calls  Function
------------------------------------------------------------------------
   1    168  92.8    168  92.8    168  92.8        -  D
   2     12   6.6    183 101.1    180  99.4        -  SOLVE-OPT
   3      0   0.0    181 100.0    180  99.4        -  "Unknown component: #x52C28DD0"
```
So once again we add inlining declaration:
```cl
(declaim (inline d))
(defun d (x)
  (declare (optimize (speed 3)))
  (declare (type fixnum x))
  (or (d3 x) (d5 x))
```
This brings us to very big performance improvement (from 1.807s down to 0.243s):
```
CL-USER> (time (solve-opt 100000000))
Evaluation took:
  0.243 seconds of real time
  0.243403 seconds of total run time (0.243403 user, 0.000000 system)
  100.00% CPU
  827,563,685 processor cycles
  0 bytes consed
  
2333333316666668
```
Here my CL knowledge ends. Profiling shows 100% time spent in `solve-opt`. I know that it's easy in CL to get assembly generated for function (via [disassemble](http://clhs.lisp.se/Body/f_disass.htm) function) but it's above my pay grade to know what to do with that.

In the end we still got very nice improvement, from 3.767s down to 0.243s.
