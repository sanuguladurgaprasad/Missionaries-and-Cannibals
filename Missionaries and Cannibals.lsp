;(50 points) Write Lisp code that solves the missionaries and cannibals problem assuming that you have
;one boat, and 15 cannibals and 15 missionaries. Assume that the boat can carry at most six people
;and there is a single boat available. You cannot also have the boat moving with no one onboard. The
;cannibals should never outnumber the missionaries (either on the boat or at both sides of the river).
;You can write your own code or modify the code from the Russell’s web site to solve the problem.
;You need to printout the sequence of moves leading to a correct solution. Try the same problem with
;24 cannibals and 24 missionaries. You should not hard-code your solution


;state of each node in the state space
(defstruct state
	state  ;array which tracks the number of Missionaries, Cannibals on the initial side
	level                                  ; and the boat position (+1 for initial side -1 for goal side)
	parent ;parent id and id fields to track the predecessor state nodes
	id     
	heuristic ;heuristic is taken as the sum of missionaries and cannibals on the initial side
)

(setq *goal_state* ()) ;indicates goal state
(setq *path_sequence* '())  ;contains sequence of state nodes which lead to the solution
(defvar *id* 0)       ;unique id for each state node   
(defparameter *visited* (make-hash-table)) ;keeps track of visited state nodes
(defparameter *idState_Map* (make-hash-table))  ;for tracking the predecessor nodes thorugh id member in the state node
(setf *heap* (make-array 0 :fill-pointer t :adjustable t)) ;keeps track of state node with minimum heuristic

;Entry Point for the Missionaries and Cannibals Problem
(defun M_and_C(no_missionaries no_cannibals boat_size) 
	(initialize no_missionaries no_cannibals)
	(solve_puzzle no_missionaries no_cannibals boat_size)
)

;Initialization of start state and goal state
(defun initialize(no_missionaries no_cannibals)
	(setf *id* (+ *id* 1))
	(setf state_array (make-array 3))
	(setf (aref state_array 0) no_missionaries)
   	(setf (aref state_array 1) no_cannibals)
    	(setf (aref state_array 2) 1)
	;push state state into heap
	(vector-push-extend (make-state 
            :state state_array 
			:level 0
            :parent 0
			:id *id*
			:heuristic 0
          ) *heap*
    )
	;set goal state
	(setf state_array (make-array 3))
	(setf (aref state_array 0) 0)
    (setf (aref state_array 1) 0)
    (setf (aref state_array 2) -1)
	(setf *goal_state* (make-state 
            :state state_array
          )
	)
)

;Solves the puzzle by expanding a state based on lowest heuristic function and generating its successors
(defun solve_puzzle(no_missionaries no_cannibals boat_size)
	(setf curr_state (vector-pop *heap*))
	;if goal state is equal to the popped node print the sequence of boat moves and return from the function
	(if (equalp (state-state curr_state) (state-state *goal_state*))	
		(progn
			;Computes all the states from state state to goal state in that order
			(calculate_path curr_state)
			;Prints the sequence of boat moves with number of missionaries and cannibals on initial side, on goal side and in the boat
			(print_sequence no_missionaries)
			;return from the function as we reached the goal state
			(return-from solve_puzzle)
		)
		(progn
			;everytime the state node is popped its put into a hashmap for the purpose of tracking parent states
			(setf (gethash (state-id curr_state) *idState_Map*) curr_state)
			(setf key (+ (+ (* 10000 (aref (state-state curr_state) 0)) (* 100 (aref (state-state curr_state) 1))) 
			(aref (state-state curr_state) 2)))
			(setf (gethash key *visited*) curr_state)
			(block outer
			;generates different successor state combinations for each expanded state node
				(loop for i from 0 to boat_size do
					(loop for j from 0 to boat_size do
						(cond ((and (= i 0) (= j 0)))
							  ((and (> i 0) (< i j)))
							  ((> (+ i j) boat_size) (return-from outer)) ;all above three conditions limit the number of cannibals
							  (t                                          ; to be <= number of missionaries on the boat
								(defvar bS)  ;boat side for the successor
								(defvar mis) ;number of missionaries on the initial side
								(defvar can) ;number of cannibals on the initial side
								(defvar mis1) ;number of missionaries on the goal side
								(defvar can1) ;number of cannibals on the goal side
								(if (= (aref (state-state curr_state) 2) 1)
									(setf bS -1)
									(setf bS 1)
								)
								(setf mis (+ (aref (state-state curr_state) 0) (* i bS)))
								(setf can (+ (aref (state-state curr_state) 1) (* j bS)))
								(setf mis1 (- no_missionaries mis))
								(setf can1 (- no_cannibals can))
								;following 2 if conditions limit the number of cannibals to be <= number of missoinaries on the 
								;initial side and goal side
								(if (or (and (= mis 0) (and (<= can no_cannibals) (>= can 1))) (and (and (>= can 0) (>= mis can)) (<= mis no_missionaries)))
									(progn
										(if (or (and (= mis1 0) (and (<= can1 no_cannibals) (>= can1 1))) 
													(and (and (>= can1 0) (>= mis1 can1)) (<= mis1 no_missionaries)))
											(progn
												(setf temp_state (make-array 3))
												(setf (aref temp_state 0) mis)
												(setf (aref temp_state 1) can)
												(setf (aref temp_state 2) bS)
												(setf key (+ (+ (* 10000 mis) (* 100 can)) bS))
												;if the state is univisited then create a new state node and push into heap
												(if (not (gethash key *visited*))
													(progn
														(setf *id* (+ *id* 1))
														(vector-push-extend (make-state 
																:state temp_state
																:level (+ (state-level curr_state) 1)
																:parent (state-id curr_state)
																:id *id*
																:heuristic (+ mis can)
															) *heap*
														)
													)
												)
											)
										)
									)
								)
							  )
						)
					)
				)
			)
		)
	)
	;sort heap after expanding each node and generating successors
	(sort_heap 0 (- (length *heap*) 1))
	;Process recursively to pick the state node with lowest heuritic
	(solve_puzzle no_missionaries no_cannibals boat_size)
)

;Quick sort partition method
(defun partition(low high)
	(setf pivot (state-heuristic (aref *heap* high)))
	(setf i (- low 1))
	(loop for j from low to (- high 1) do
		(if (>= (state-heuristic (aref *heap* j)) pivot)
			(progn
				(setf i (+ i 1))
				(rotatef (aref *heap* i) (aref *heap* j))
			)
		)
	)
	(rotatef (aref *heap* (+ i 1)) (aref *heap* high))
	(+ i 1)
)

;Quick sort
(defun sort_heap(low high)
	(if (< low high)
		(progn
			(setf p (partition low high))
			(sort_heap low (- p 1))
			(sort_heap (+ p 1) high)
		)
	)
)

;Populate the path sequence with all the nodes from start state to goal state recursively
(defun calculate_path(goal_node)
	(push goal_node *path_sequence*)
	(setf pId (state-parent goal_node))
	(if (not (= pId 0))
		(progn
			(calculate_path (gethash pId *idState_Map*))
		)
	)
)

;Prints the initial side state and goal side state for each boat move
(defun print_sequence(total)
	(format t "Initial Side State          Boat          Goal Side State~%")
	(setf temp_state (make-array 2)) ;tracks the current initial side state
	(setf cur_state (make-array 2)) ;tracks current goal side state
	(setf prev_state (make-array 2)) ;tracks previous goal side state
	(setf boat (make-array 2))  ;for printing boat confguration
	(setf k (- (length *path_sequence*) 1)) ;length of path sequence
	
	(loop for i from 1 to (- (length *path_sequence*) 1) do
		;get each element in path sequence and populate temp_state, cur_state, prev_state and boat variables
		(setf temp_state (state-state (nth i *path_sequence*)))
		(setf (aref cur_state 0) (- total (aref temp_state 0)))
		(setf (aref cur_state 1) (- total (aref temp_state 1)))
		(if (= -1 (aref temp_state 2))
			(progn
				(setf (aref boat 0) (- (aref (state-state (nth (- i 1) *path_sequence*)) 0) (aref temp_state 0)))
				(setf (aref boat 1) (- (aref (state-state (nth (- i 1) *path_sequence*)) 1) (aref temp_state 1)))
			)
			(progn
				(setf (aref boat 0) (- (aref prev_state 0) (aref cur_state 0)))
				(setf (aref boat 1) (- (aref prev_state 1) (aref cur_state 1)))
			)
		)
		(setf (aref prev_state 0) (aref cur_state 0))
		(setf (aref prev_state 1) (aref cur_state 1))
		;printing the number of missionaries and cannibals on initial side, on goal side and in the boat
		;the printing differs based on the position of the boat, so we check boat position with the temp_state 
		(if (= -1 (aref temp_state 2))
			(format t "    [~d,~d]            ===== [~d,~d] ====>        [~d,~d] ~%" (aref temp_state 0) (aref temp_state 1)
																 (aref boat 0)(aref boat 1)
																 (aref cur_state 0)(aref cur_state 1))
			(format t "    [~d,~d]            <==== [~d,~d] =====        [~d,~d] ~%" (aref temp_state 0) (aref temp_state 1)
																 (aref boat 0)(aref boat 1)
																 (aref cur_state 0)(aref cur_state 1))
		)
	)
	(format t "number of boat moves = ~d ~%" k)
)

;(M_and_C 24 24 6)
