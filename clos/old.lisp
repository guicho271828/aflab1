(define-local-function %rec (open closed)
  (match (rb-minimum-node open)
         ((rb-node _ _ _ nil _)
          (%rec (rb-remove-minimum-node open) closed))
         ((rb-node _ _ f* list _)
          (destructuring-bind (now . rest)
              (if tiebreak (funcall tiebreak list) list)
            (if (goal-p now)
                (restart-case
                    (progn
                      (signal 'solution-found :solution now)
                      (values now f*))
                  (continue ()
                    (%iter-edge (rb-insert open f* rest)
                                (insert-queue f* now closed)
                                now (edges now))))
                (%iter-edge (rb-insert open f* rest)
                            (insert-queue f* now closed)
                            now (edges now)))))
         (_ (signal 'path-not-found))))

(define-local-function %iter-edge (open closed now edges)
  (ematch edges
          ((list)
           (%rec open closed))
          ((list* (and e (or (edge (eq now) neighbor)
                             (edge neighbor (eq now)))) rest)
           (let* ((h (h neighbor))
                  (old-g* (g neighbor))
                  (old-f* (+ h old-g*))
                  (new-g* (+ (g now) (c e)))
                  (new-f* (+ h new-g*)))
             (cond
               ((when-let ((member (rb-member old-f* open)))
                          (find neighbor member :test test))
                (if (< new-g* old-g*) ; f'(m) < f*(m)
                    (progn
                      (setf (g neighbor) new-g*
                            (parent neighbor) now)
                      (%iter-edge
                       (insert-queue
                        new-f* neighbor
                        (remove-queue
                         old-f* neighbor open))
                       closed now rest))
                    (%iter-edge open closed now rest)))
               ;;
               ((when-let ((member (rb-member old-f* closed)))
                          (find neighbor member :test test))
                (if (< new-g* old-g*)
                    (progn
                      (setf (g neighbor) new-g*
                            (parent neighbor) now)
                      (%iter-edge
                       (insert-queue new-f* neighbor open)
                       (remove-queue old-f* neighbor closed)
                       now rest))
                    (%iter-edge open closed now rest)))
               ;;
               (t (setf (g neighbor) new-g*)
                  (setf (parent neighbor) now)
                  (%iter-edge
                   (insert-queue new-f* neighbor open)
                   closed now rest)))))))


