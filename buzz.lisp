(defun sentence ()    (append (noun-phrase) (verb-phrase)))
 
(defun verb-adj-phrase () (append (management-verb) (article) (adjective) (noun-needs-article)))
 
 
 
 
 
(defun management-sentence () (append (intro) (topic) (management-verb) (noun-phrase)))
 
(defun verb-phrase () (random-elt (list (append (management-verb) (article) (adjective) (noun-needs-article))
                                    (append (management-verb) (adjective) (noun)))))
 
(defun article ()     (one-of '("a")))
(defun greeting () (random-elt '("Team," "Dear All,")))
(defun farewell () (random-elt '("Regards," "All the best,")))
(defun title () ( one-of '("Director" "Team Lead" "Vice President" "General Manager" "Sr. Director")))
(defun dept-adjective () (one-of '("International" "Creative" "Employee" "Standard" "Personal")))
(defun dept-noun () (one-of '("Sales" "Information" "Technology")))
(defun dept-verb () (one-of '("Modernization" "Services" "Security")))
 
(defun signature (name) (append (list name) (title) (dept-adjective) (dept-noun) (dept-verb)))
 
 
(defun noun-needs-article ()  (one-of '("solution"  "web application" "sql database")))
(defun noun-no-article () (one-of '("data integrity" "big data" "data security" "collaboration")))
(defun noun-phrase () (random-elt (list (append (article) (adjective) (noun-needs-article))
                                        (append (adjective) (noun-no-article))
                                        (append (article) (adjective) (noun-no-article) (noun-needs-article)))))
 
(defun management-verb ()        (one-of '("implement" "acquire" "generate" "scrub" "retain" "produce" "re-engineer")))
 
(defun adjective () (one-of '("multi-platform" "robust" "highly interconnected" "lean" "agile" "virtual" "social-media-enabled")))
(defun topic () (one-of '("the business has requested that we" "upper management has approved our plan to" "I will be forming a task force to" "we have been offered the opportunity to" "I am excited in the direction the organization has been heading, and have the utmost confidence that we will ")))
(defun intro () (one-of '("Despite past difficulties," "Thanks to our concerted efforts," "Going into the new quarter," "Building on our past success," "Based on our existing systems," "As you are all aware,")))
 
(not nil)
 
;;TODO - make this efficient by stopping after the first similar element
(defun compare-sentence-to-list (sentence sentence-list)
  (if (not sentence-list)
      t
     
      (let ((return-value t))
        (dolist (old-sentence sentence-list)
          (loop for old-word in old-sentence
             do
               (loop for new-word in sentence
                  do (if (equalp old-word new-word)
                         (setf return-value nil)))))
        return-value)))
                        
(compare-sentence-to-list '("h" "i" "a") '(("a" "b" "c") ("e" "f" "g")))
(compare-sentence-to-list '("h" "i" "a") '(()))
 
(defun generate-sentences (number-of-sentences)
  (let ((sentences nil)
        (current-sentence nil))
    (loop for i from 0 below number-of-sentences
         do
         (setf current-sentence (management-sentence))
         (if (compare-sentence-to-list current-sentence sentences)
             (setf sentences (append sentences (list current-sentence)))
             (setf i (- i 1))))
 
    sentences))
 
(generate-sentences 2)
 
setf sentences (append sentences (management-sentence)))
        
 
(verb-phrase)
(management-sentence)
 
(defun print-sentence()
  (format t "~a~%" (greeting))
   
  (dolist (sentence (generate-sentences 2))
    (dolist (sentence-part sentence)
      (format t "~a " sentence-part))
    (format t ".~%"))
 
  (format t "~a~%" (farewell))
  (dolist (signature-part (signature "Andrew Pierce,"))
    (format t "~a " signature-part))
  (format t "~%~%"))
 
(print-sentence)
 
 
 
;;; ==============================
 
(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))
 
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
 
;;; ==============================
