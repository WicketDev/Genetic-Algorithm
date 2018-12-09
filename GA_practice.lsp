
;;; A SIMPLE GENETIC ALGORITHM OPERATING OVER FLOATING-POINT VECTORS


#|
In this project you will do three things:

1. Implement a very simple abstract high-level evolutionary computation framework

2. Implement the functions necessary for a floating-point genetic algorithm

3. Test it on various objective functions and find good settings of parameters
   which work well on those functions


|#



;;; Some utility Functions and Macros that you might find to be useful

(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

;;; Example usage
;;;
;;; (let ((x 0))
;;;    (while (< x 5)
;;;        (print x)
;;;        (incf x)))


(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))

	




;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 


;;; TOURNAMENT SELECTION
  
(defparameter *tournament-size* 6) 
(defparameter *best-inds* '());these allow me to find the overall best ind and fitness at end of run
(defparameter *best-fits* '())
(defparameter *current-best-fit* 0)
(defparameter *best-of-inds* '())
(defparameter *best-of-fits* '())


(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."
	(let (best next pos)
		(setf best (nth (random (length population)) fitnesses)) ;picks random (fitness of) individual and marks it best
		(loop for i from 2 to *tournament-size* ;iterates through list
			do (setf next (nth (random (length population)) fitnesses)) ;randomly selects another in population to compare
				if (> next best) 
					do (setf best next)) ;if next is better than best, it becomes best
					(setf pos(position best fitnesses))
					(nth pos population)));returns the position of best in population 
				

 
(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"
	(let ((x 0) parents)
		(while (< x num) ;as the algorithm calls for 2 parents, num is always 2, selects 2 parents and puts into a list that is returned
			(setf parents (append parents (list (tournament-select-one population fitnesses))))
			(incf x)) parents))




(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setf *best-inds* (append *best-inds* (list best-ind)));modified this to put all best ind in a list
		  (setq best-fit fit)
		  (setf *current-best-fit* best-fit)
		  (setf *best-fits* (append *best-fits* (list best-fit))))) pop fitnesses);modified to put all best fitnesses in a list
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses)) 
	



(defun evolve (generations pop-size
	       &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"
	;;setup should be either "yes" if you want to skip having to answer questions to change variables,and "no" otherwise
	(float-vector-sum-setup setup);allows the option to change variables to be skipped, useful when running multiple times in a row
	(progn
		(let (pop pop-fit parents best kids)
			(if (/=(mod pop-size 2) 0) (progn (format t "Pop-size must be multiple of 2. One has been added to your pop-size to comply.~%") 
				(setf pop-size (1+ pop-size)))) ;ensures pop-size is even
		(dotimes (i pop-size) (setf pop (append pop (list (funcall creator))))) ;creates new random population using tournament selection
		(dotimes (i generations) ;repeats for specified generations
			(setf pop-fit (mapcar evaluator pop)) ;generates fitness for current population
			(funcall printer pop pop-fit);prints best of current generation
			(if (equalp best nil) (setf best *current-best-fit*) ;on first iteration sets best to current
				(if (< best *current-best-fit*) 
					(setf best *current-best-fit*))) ;otherwise sets best to highest fitness individual so far
				(setf kids nil)	
			(dotimes (i (/ pop-size 2));generates another population using genetic algorithm
				(setf parents (funcall selector 2 pop pop-fit)) ;selects 2 parents 
				(setf kids (append kids (funcall modifier (first parents) (second parents))))) ;crossover and mutates parents into 2 new kids/individuals
				
			(setf pop kids)) ;list of kids becomes new population
			
		(setf pop-fit (mapcar evaluator pop)) ;generates fitness for last population
		(funcall printer pop pop-fit) ; prints best of last generation	
		(if (< best *current-best-fit*) 
			(setf best *current-best-fit*)) ;if last gen best was better than previous best, sets it to best
			(setf *best-of-inds* (append *best-of-inds* *best-inds*)) ;allows to keep all for multiple runs
			(setf *best-of-fits* (append *best-of-fits* *best-fits*)) ;allows to keep all for multiple runs
		(format t "~%Best Individual Overall...~%Fitness: ~a~%Individual: ~a~%Max Value: ~a~%Mutation rate: ~a~%Mutation Probability: ~a~%Crossover Probabilty: ~a~%Tournament Size: ~a~%" best (nth (position best *best-fits*) *best-inds*) *float-max* *mutation-variance* *mutation-probability* *crossover-probability* *tournament-size*)))) ;prints overall best individual and knob values


(defun test (runs generations pop-size creator selector modifier evaluator printer)
			(dotimes (i runs)
				(evolve generations pop-size :setup "yes" :creator creator :selector selector :modifier modifier :evaluator evaluator :printer printer)) 
				(format t "Average fitness: ~a." (/ (reduce #'+ *best-of-fits*) (length *best-of-fits*))));returns the average fitness of a number of runs with same parameters

;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")
  
  ;; function that returns a random number between -x and x
(defun min-to-max-random ()
	(- (random (* 2 *float-max*)) *float-max*))
	

(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"

	(generate-list *float-vector-length* #'min-to-max-random));creates a random individual of specified length




(defparameter *crossover-probability* 0.2
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.9
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.02
  "Per-gene mutation variance in gaussian convolution")



(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."

	(progn 
		(let ((l (length ind1)))
			(dotimes (i l) ;for length of individual
				(if (>= *crossover-probability* (random 1.0)) ;if random is greater or equal to prob, crossover
					(rotatef (elt ind1 i) (elt ind2 i))))))) ;changes the i element of individual if true




(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."

	(progn 
		(let ((len (length ind)) (n (gaussian-random 0 *mutation-variance*)))
			(dotimes (i len) 
				(if (>= *mutation-probability*(random 1.0)) ;determines if ith ind gets mutated or not
					(progn
						(loop until (and (< (+ (elt ind i) n) *float-max*) (> (+ (elt ind i) n) *float-min*)) ;loops until a usable n is found that keeps the new ind within bounds 
						do (setf n (gaussian-random 0 *mutation-variance*)));generates new gaussian random number 
					(setf (elt ind i) (+ (elt ind i) n)))) ; sets i of ind to (i of ind + n)
				ind)))) ;returns new ind








(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
then mutates the children.  *crossover-probability* is the probability that any
given allele will crossover.  *mutation-probability* is the probability that any
given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

	(progn 
		(let ((i1 (copy-list ind1)) (i2 (copy-list ind2)) kids);copies individuals
		(uniform-crossover i1 i2) ;calls crossover on inds
		(setf kids (list i1 i2));sets results to kids
		(mapcar #'gaussian-convolution kids);calls mutation on kids and returns results
		kids)))


(defun float-vector-sum-setup (skip)
  " this function allows the knobs to be played with during execution of program"
(let ((answer (string "no")));default answer is no
(if (string-equal skip "no")
(progn (format t "~%Care to change any default values? y/n~%");allows this to be skipped if changes not needed
(setf answer (read))
(if (or (string-equal answer "y") (string-equal answer "yes"));if yes, goes through various knobs and asks if want changed
	(progn (format t "~%Crossover probability is ~a. Change crossover probability? y/n~%" *crossover-probability*)
		(setf answer (read))
			(if (or (string-equal answer "y") (string-equal answer "yes"));if yes, allows change to be made, assumes correct values are input
				(progn (format t "Enter crossover probability btwn 0.0 and 1.0~%")
				(setf *crossover-probability* (read))))
		
		(format t "Mutation probability set to ~a. Change mutation probability? y/n~%" *mutation-probability*)
		(setf answer (read))
			(if (or (string-equal answer "y") (string-equal answer "yes"));if yes, allows change to be made, assumes correct values are input
				(progn (format t "Enter mutation probability btwn 0.0 and 1.0~%")
				(setf *mutation-probability* (read))))

		(format t "Mutation variance set to ~a. Change mutation variance? y/n~%" *mutation-variance*)
		(setf answer (read))
			(if (or (string-equal answer "y") (string-equal answer "yes"));if yes, allows change to be made, assumes correct values are input
				(progn (format t "Enter mutation variance.~%")
				(setf *mutation-variance* (read))))

		(format t "Tournament size set to ~a. Change tournament size? y/n~%" *tournament-size*)
		(setf answer (read))
			(if (or (string-equal answer "y") (string-equal answer "yes"));if yes, allows change to be made, assumes correct values are input
				(progn (format t "Enter tournament size between 2 and 20.~%")
				(setf *tournament-size* (read))))))))
		
		(setf *float-vector-length* 20);here for easy change if wanted, but unlikely to change
		(setf *float-max* 5.12)
		(setf *float-min* -5.12)
		(setf *best-fits* '()) ;ensures these are reset to empty list every run
		(setf *best-inds* '())))
  
  





;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;; 
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.  
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.


(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))




;;; an example way to fire up the GA.  If you've got it tuned right, it should quickly
;;; find individuals which are all very close to +5.12

#|
(evolve 50 1000
 	:setup "no"
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
    :evaluator #'sum-f
	:printer #'simple-printer)
	
	(test 50 50 1000 #'float-vector-creator #'tournament-selector #'float-vector-modifier #'sum-f #'simple-printer)
|#

"The best results I got were from using the sum-f function for fitness selection, step-f, and schwefel all came in 2nd and the others had terrible results. The best setting for the various other knobs
were the default settings I have here, variance: .02, mutation prob: .9, crossover prob: .2, and tournament size: 6 The best result I got was: 
Best Individual Overall...
Fitness: 101.5195
Individual: (5.1153073 5.050518 5.116727 5.10667 5.1029554 5.093485 5.1008134 5.10259 5.0646258 5.0843744 4.98362 5.1045694 5.018002 4.964659 5.1026244 5.1198106 5.091679 5.0579496 5.0841956 5.0543213)
Max Value: 5.12
Mutation rate: 0.02
Mutation Probability: 0.9
Crossover Probabilty: 0.2
Tournament Size: 6
There are a lot of knobs to play with in this program and since they are mostly reals, there are a ton of options to choose from. I set it up so I could I could opt to change none, or just individual knobs per run. 